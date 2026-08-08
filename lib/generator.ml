open Model

module String_set = Set.Make (String)

type quota = {
  domain_code : string;
  easy : int;
  medium : int;
  hard : int;
}

(* Mirrors the real digital SAT's balanced difficulty mix: roughly one third
   easy, one third medium, one third hard per module. *)
let reading_writing_quotas =
  [
    { domain_code = "CAS"; easy = 3; medium = 3; hard = 2 };
    { domain_code = "INI"; easy = 2; medium = 3; hard = 2 };
    { domain_code = "SEC"; easy = 2; medium = 2; hard = 3 };
    { domain_code = "EOI"; easy = 2; medium = 1; hard = 2 };
  ]

let math_quotas =
  [
    { domain_code = "H"; easy = 3; medium = 3; hard = 2 };
    { domain_code = "P"; easy = 2; medium = 3; hard = 3 };
    { domain_code = "Q"; easy = 1; medium = 1; hard = 1 };
    { domain_code = "S"; easy = 1; medium = 1; hard = 1 };
  ]

let shuffle state values =
  let array = Array.of_list values in
  for index = Array.length array - 1 downto 1 do
    let other = Random.State.int state (index + 1) in
    let tmp = array.(index) in
    array.(index) <- array.(other);
    array.(other) <- tmp
  done;
  Array.to_list array

let take count values =
  let rec loop remaining selected rest =
    if remaining = 0 then (List.rev selected, rest)
    else
      match rest with
      | [] -> (List.rev selected, [])
      | value :: tail -> loop (remaining - 1) (value :: selected) tail
  in
  loop count [] values

let pick_exact state ~used (questions : question_metadata list) domain_code difficulty count =
  let candidates =
    questions
    |> List.filter (fun (question : question_metadata) ->
           not (String_set.mem question.external_id used)
           && String.equal question.domain_code domain_code
           && question.difficulty = difficulty)
    |> shuffle state
  in
  let selected, _ = take count candidates in
  let used =
    List.fold_left
      (fun set (question : question_metadata) ->
        String_set.add question.external_id set)
      used selected
  in
  (selected, used, count - List.length selected)

let pick_fallback state ~used (questions : question_metadata list) ~section ~domain_code count =
  let same_domain =
    questions
    |> List.filter (fun (question : question_metadata) ->
           not (String_set.mem question.external_id used)
           && question.section = section
           && String.equal question.domain_code domain_code)
    |> shuffle state
  in
  let selected_domain, _ = take count same_domain in
  let used =
    List.fold_left
      (fun set (question : question_metadata) ->
        String_set.add question.external_id set)
      used selected_domain
  in
  let remaining = count - List.length selected_domain in
  if remaining = 0 then (selected_domain, used)
  else
    let anywhere =
      questions
      |> List.filter (fun (question : question_metadata) ->
             not (String_set.mem question.external_id used)
             && question.section = section)
      |> shuffle state
    in
    let selected_anywhere, _ = take remaining anywhere in
    let used =
      List.fold_left
        (fun set (question : question_metadata) ->
          String_set.add question.external_id set)
        used selected_anywhere
    in
    (selected_domain @ selected_anywhere, used)

let difficulty_rank = function Easy -> 0 | Medium -> 1 | Hard -> 2

let favor_math_student_responses state ~eligible ~selected ~used =
  let existing =
    List.fold_left
      (fun count (question : question_metadata) ->
        count + if question.item_type = Some Student_response then 1 else 0)
      0 selected
  in
  let needed = max 0 (6 - existing) in
  let rec swap needed selected used remaining =
    if needed = 0 then (selected @ remaining, used)
    else
      match remaining with
      | [] -> (selected, used)
      | (question : question_metadata) :: rest ->
          if question.item_type = Some Student_response then
            swap needed (question :: selected) used rest
          else
            let candidates =
              eligible
              |> List.filter (fun (candidate : question_metadata) ->
                     candidate.item_type = Some Student_response
                     && candidate.section = Math
                     && candidate.difficulty = question.difficulty
                     && String.equal candidate.domain_code question.domain_code
                     && not (String_set.mem candidate.external_id used))
              |> shuffle state
            in
            match candidates with
            | replacement :: _ ->
                let used =
                  used
                  |> String_set.remove question.external_id
                  |> String_set.add replacement.external_id
                in
                swap (needed - 1) (replacement :: selected) used rest
            | [] -> swap needed (question :: selected) used rest
  in
  swap needed [] used (shuffle state selected)

let order_questions state kind (questions : question_metadata list) =
  let questions = shuffle state questions in
  match module_section kind with
  | Math ->
      List.stable_sort
        (fun (a : question_metadata) (b : question_metadata) ->
          Int.compare (difficulty_rank a.difficulty) (difficulty_rank b.difficulty))
        questions
  | Reading_writing ->
      let domain_rank = function
        | "CAS" -> 0
        | "INI" -> 1
        | "SEC" -> 2
        | "EOI" -> 3
        | _ -> 4
      in
      List.stable_sort
        (fun (a : question_metadata) (b : question_metadata) ->
          let by_domain =
            Int.compare (domain_rank a.domain_code) (domain_rank b.domain_code)
          in
          if by_domain <> 0 then by_domain
          else
            let by_difficulty =
              Int.compare (difficulty_rank a.difficulty) (difficulty_rank b.difficulty)
            in
            by_difficulty)
        questions

let generate_module state ~used ~eligible kind =
  let section = module_section kind in
  let quotas =
    match section with
    | Reading_writing -> reading_writing_quotas
    | Math -> math_quotas
  in
  let selected = ref [] in
  let used = ref used in
  let deficits = ref [] in
  List.iter
    (fun quota ->
      List.iter
        (fun (difficulty, count) ->
          let picked, next_used, deficit =
            pick_exact state ~used:!used eligible quota.domain_code difficulty count
          in
          selected := picked @ !selected;
          used := next_used;
          if deficit > 0 then deficits := (quota.domain_code, deficit) :: !deficits)
        [ (Easy, quota.easy); (Medium, quota.medium); (Hard, quota.hard) ])
    quotas;
  let relaxed = !deficits <> [] in
  List.iter
    (fun (domain_code, count) ->
      let picked, next_used =
        pick_fallback state ~used:!used eligible ~section ~domain_code count
      in
      selected := picked @ !selected;
      used := next_used)
    !deficits;
  let expected = module_question_count kind in
  if List.length !selected <> expected then
    Error
      (Printf.sprintf
         "Only %d eligible questions remain for %s; %d are required."
         (List.length !selected) (module_label kind) expected)
  else
    let final_questions, final_used =
      match section with
      | Reading_writing -> (!selected, !used)
      | Math ->
          favor_math_student_responses state ~eligible ~selected:!selected ~used:!used
    in
    Ok
      ( { kind; questions = order_questions state kind final_questions; relaxed_blueprint = relaxed },
        final_used )

let generate ?(seed = Random.bits ()) ~eligible modules =
  let state = Random.State.make [| seed |] in
  let modules = List.sort compare_module modules in
  let rec loop used assignments = function
    | [] -> Ok (List.rev assignments)
    | kind :: rest -> (
        match generate_module state ~used ~eligible kind with
        | Error message -> Error message
        | Ok (assignment, next_used) ->
            loop next_used (assignment :: assignments) rest)
  in
  loop String_set.empty [] modules
