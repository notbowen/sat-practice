open Lwt.Infix
open Model

let base_url =
  "https://qbank-api.collegeboard.org/msreportingquestionbank-prod/questionbank"

let lookup_url = base_url ^ "/lookup"
let questions_url = base_url ^ "/digital/get-questions"
let question_url = base_url ^ "/digital/get-question"

(* The Educator Question Bank renders individual questions in a modal and does
   not publish stable per-question URLs.  Keep the public question ID next to
   this link in the UI so the source item can still be identified exactly. *)
let results_url =
  "https://satsuiteeducatorquestionbank.collegeboard.org/digital/results"

let json_headers =
  Cohttp.Header.init_with "content-type" "application/json"
  |> fun headers ->
  Cohttp.Header.add headers "accept" "application/json"
  |> fun headers ->
  Cohttp.Header.add headers "user-agent" "sat-revision-local/1.0"

let with_timeout seconds promise =
  Lwt.pick
    [
      (promise >|= fun value -> Ok value);
      (Lwt_unix.sleep seconds >|= fun () -> Error "Question bank request timed out");
    ]

let call_once ?body method_ url =
  let uri = Uri.of_string url in
  let body = Option.map Cohttp_lwt.Body.of_string body in
  Lwt.catch
    (fun () ->
      with_timeout 12.
        (Cohttp_lwt_unix.Client.call ?body ~headers:json_headers method_ uri)
      >>= function
      | Error message -> Lwt.return (Error message)
      | Ok (response, response_body) ->
          Cohttp_lwt.Body.to_string response_body >|= fun content ->
          let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
          if code >= 200 && code < 300 then Ok content
          else
            Error
              (Printf.sprintf "Question bank returned HTTP %d" code))
    (fun error -> Lwt.return (Error (Printexc.to_string error)))

let rec call_with_retry ?body method_ url attempts =
  call_once ?body method_ url >>= function
  | Ok _ as success -> Lwt.return success
  | Error _ as failure when attempts <= 0 -> Lwt.return failure
  | Error _ ->
      Lwt_unix.sleep 0.25 >>= fun () ->
      call_with_retry ?body method_ url (attempts - 1)

let get_json url =
  call_with_retry `GET url 1 >|= function
  | Error _ as error -> error
  | Ok body -> (
      try Ok (Yojson.Safe.from_string body)
      with Yojson.Json_error message -> Error ("Invalid question bank JSON: " ^ message))

let post_json url json =
  let body = Yojson.Safe.to_string json in
  call_with_retry ~body `POST url 1 >|= function
  | Error _ as error -> error
  | Ok response -> (
      try Ok (Yojson.Safe.from_string response)
      with Yojson.Json_error message -> Error ("Invalid question bank JSON: " ^ message))

let member name = function
  | `Assoc values -> List.assoc_opt name values |> Option.value ~default:`Null
  | _ -> `Null

let string_member name json =
  match member name json with
  | `String value -> value
  | `Int value -> string_of_int value
  | `Intlit value -> value
  | _ -> ""

let optional_string_member name json =
  match member name json with
  | `String value when not (String.equal value "") -> Some value
  | _ -> None

let int64_member name json =
  match member name json with
  | `Int value -> Int64.of_int value
  | `Intlit value -> Int64.of_string_opt value |> Option.value ~default:0L
  | `Float value -> Int64.of_float value
  | _ -> 0L

let list_member name json = match member name json with `List values -> values | _ -> []

let parse_metadata section synced_at json =
  let external_id = string_member "external_id" json in
  let question_id = string_member "questionId" json in
  let difficulty = difficulty_of_string (string_member "difficulty" json) in
  if String.equal external_id "" || String.equal question_id "" then
    Error "Question metadata is missing an identifier"
  else
    match difficulty with
    | None -> Error "Question metadata has an unsupported difficulty"
    | Some difficulty ->
        Ok
          {
            external_id;
            question_id;
            section;
            difficulty;
            domain_code = string_member "primary_class_cd" json;
            domain_name = string_member "primary_class_cd_desc" json;
            skill_code = string_member "skill_cd" json;
            skill_name = string_member "skill_desc" json;
            item_type = None;
            source_updated_at = int64_member "updateDate" json;
            synced_at;
          }

let parse_metadata_list section synced_at = function
  | `List values ->
      let parsed = List.filter_map (fun value -> Result.to_option (parse_metadata section synced_at value)) values in
      if parsed = [] then Error "Question list contained no usable metadata" else Ok parsed
  | _ -> Error "Question list was not an array"

let get_lookup () = get_json lookup_url

let fetch_metadata section =
  let test, domain =
    match section with
    | Reading_writing -> (1, "INI,CAS,EOI,SEC")
    | Math -> (2, "H,P,Q,S")
  in
  let request =
    `Assoc
      [
        ("asmtEventId", `Int 99);
        ("test", `Int test);
        ("domain", `String domain);
      ]
  in
  let synced_at = Int64.of_float (Unix.gettimeofday ()) in
  post_json questions_url request >|= function
  | Error _ as error -> error
  | Ok json -> parse_metadata_list section synced_at json

let fetch_all_metadata () =
  get_lookup () >>= function
  | Error _ as error -> Lwt.return error
  | Ok _ ->
      Lwt.both (fetch_metadata Reading_writing) (fetch_metadata Math)
      >|= function
      | Ok reading_writing, Ok math -> Ok (reading_writing @ math)
      | Error message, _ | _, Error message -> Error message

let parse_answer_options json =
  list_member "answerOptions" json
  |> List.mapi (fun index option_json ->
         {
           letter = String.make 1 (Char.chr (Char.code 'A' + index));
           content = string_member "content" option_json |> Sanitize.fragment;
         })

let parse_correct_answers json =
  match member "correct_answer" json with
  | `List values ->
      List.filter_map
        (function `String value -> Some value | `Int value -> Some (string_of_int value) | _ -> None)
        values
  | `String value -> [ value ]
  | `Int value -> [ string_of_int value ]
  | _ -> []

let parse_detail json =
  let external_id =
    match optional_string_member "externalid" json with
    | Some value -> value
    | None -> string_member "external_id" json
  in
  let item_type =
    match String.lowercase_ascii (string_member "type" json) with
    | "spr" -> Student_response
    | _ -> Multiple_choice
  in
  let correct_answers = parse_correct_answers json in
  if String.equal external_id "" || correct_answers = [] then
    Error "Question detail is missing its identifier or answer key"
  else
    Ok
      {
        external_id;
        stimulus = string_member "stimulus" json |> Sanitize.fragment;
        stem = string_member "stem" json |> Sanitize.fragment;
        rationale = string_member "rationale" json |> Sanitize.fragment;
        item_type;
        answer_options = parse_answer_options json;
        correct_answers;
      }

type cache_entry = {
  detail : question_detail;
  mutable touched_at : float;
}

let detail_cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 131
let cache_lock = Lwt_mutex.create ()
let cache_capacity = 128
let cache_ttl_seconds = 30. *. 60.

let prune_cache now =
  Hashtbl.filter_map_inplace
    (fun _ entry ->
      if now -. entry.touched_at > cache_ttl_seconds then None else Some entry)
    detail_cache;
  while Hashtbl.length detail_cache > cache_capacity do
    let oldest =
      Hashtbl.fold
        (fun key entry current ->
          match current with
          | None -> Some (key, entry.touched_at)
          | Some (_, timestamp) when entry.touched_at < timestamp ->
              Some (key, entry.touched_at)
          | Some _ -> current)
        detail_cache None
    in
    match oldest with Some (key, _) -> Hashtbl.remove detail_cache key | None -> ()
  done

let cached_detail external_id =
  Lwt_mutex.with_lock cache_lock (fun () ->
      let now = Unix.gettimeofday () in
      prune_cache now;
      let result = Hashtbl.find_opt detail_cache external_id in
      Option.iter (fun entry -> entry.touched_at <- now) result;
      Lwt.return (Option.map (fun entry -> entry.detail) result))

let store_detail detail =
  Lwt_mutex.with_lock cache_lock (fun () ->
      Hashtbl.replace detail_cache detail.external_id
        { detail; touched_at = Unix.gettimeofday () };
      prune_cache (Unix.gettimeofday ());
      Lwt.return_unit)

let get_question external_id =
  cached_detail external_id >>= function
  | Some detail -> Lwt.return (Ok detail)
  | None ->
      post_json question_url (`Assoc [ ("external_id", `String external_id) ])
      >>= function
      | Error _ as error -> Lwt.return error
      | Ok json -> (
          match parse_detail json with
          | Error _ as error -> Lwt.return error
          | Ok detail -> store_detail detail >|= fun () -> Ok detail)

let map_bounded ~concurrency f values =
  let pool = Lwt_pool.create concurrency (fun () -> Lwt.return_unit) in
  Lwt_list.map_p (fun value -> Lwt_pool.use pool (fun () -> f value)) values

let get_questions values = map_bounded ~concurrency:4 get_question values
