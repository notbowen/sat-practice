open Lwt.Infix
open Model

let session_cookie = "sat_session"
let guest_csrf_cookie = "sat_guest_csrf"
let session_lifetime = 7. *. 24. *. 60. *. 60.
let session_field = Dream.new_field ~name:"sat-user" ()

let secure_cookies =
  match Sys.getenv_opt "SAT_COOKIE_SECURE" with
  | Some value -> List.mem (String.lowercase_ascii value) [ "1"; "true"; "yes" ]
  | None -> false

let escape = Dream.html_escape

let int_param request name = int_of_string_opt (Dream.param request name)

let parse_modules fields =
  all_modules
  |> List.filter (fun kind ->
         List.exists
           (fun (name, value) ->
             String.equal name "module" && String.equal value (module_to_string kind))
           fields)

let module_codes selected = String.split_on_char ',' selected
let selected selected kind = List.mem (module_to_string kind) (module_codes selected)

let format_time timestamp =
  let tm = Unix.localtime (Int64.to_float timestamp) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

let status_label = function
  | "in_progress" -> "In progress"
  | "grading_pending" -> "Grading pending"
  | "completed" -> "Completed"
  | "pending" -> "Not started"
  | "active" -> "In progress"
  | "submitted" -> "Submitted"
  | value -> value

let layout ?user ?(title = "SAT Revision") ?notice content =
  let user_nav =
    match user with
    | None ->
        {|<a href="/login">Log in</a><a class="button button-small" href="/register">Create account</a>|}
    | Some (session : Db.session_user) ->
        Printf.sprintf
          {|<span class="username">%s</span><form action="/logout" method="post"><input type="hidden" name="csrf" value="%s"><button class="link-button" type="submit">Log out</button></form>|}
          (escape session.user.username) (escape session.csrf_token)
  in
  let notice = match notice with None -> "" | Some message -> Printf.sprintf {|<div class="notice">%s</div>|} (escape message) in
  Printf.sprintf
    {|<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>%s · SAT Revision</title><link rel="stylesheet" href="/static/main.css"></head>
<body><header class="site-header"><a class="brand" href="/"><span>SAT</span> Revision</a><nav>%s</nav></header>
<main class="page">%s%s</main><footer>Private local study tool · Not affiliated with College Board</footer></body></html>|}
    (escape title) user_nav notice content

let security_headers handler request =
  handler request >|= fun response ->
  Dream.set_header response "Content-Security-Policy"
    "default-src 'self'; img-src 'self' data: https://*.collegeboard.org; media-src 'self' data: https://*.collegeboard.org; style-src 'self'; script-src 'self'; connect-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'";
  Dream.set_header response "X-Content-Type-Options" "nosniff";
  Dream.set_header response "Referrer-Policy" "no-referrer";
  Dream.set_header response "X-Frame-Options" "DENY";
  Dream.set_header response "Cache-Control" "no-store";
  response

let session_middleware db handler request =
  match Dream.cookie ~prefix:None ~decrypt:false ~secure:secure_cookies request session_cookie with
  | None -> handler request
  | Some token ->
      Db.find_session db (Security.sha256 token) >>= fun session ->
      Option.iter (Dream.set_field request session_field) session;
      handler request

let current_session request = Dream.field request session_field

let require_session handler request =
  match current_session request with
  | Some session -> handler session request
  | None -> Dream.redirect request ("/login?next=" ^ Uri.pct_encode (Dream.target request))

let unauthorized () = Dream.json ~status:`Unauthorized {|{"error":"authentication required"}|}
let not_found () = Dream.html ~status:`Not_Found (layout ~title:"Not found" {|<section class="card"><h1>Not found</h1><p>That page or attempt does not exist.</p></section>|})

let form_fields request =
  Dream.form ~csrf:false request >|= function `Ok fields -> Ok fields | _ -> Error "Invalid form submission."

let field name fields = List.assoc_opt name fields |> Option.value ~default:""

let valid_csrf session request fields =
  Security.constant_time_equal session.Db.csrf_token (field "csrf" fields)

let valid_json_csrf session request =
  match Dream.header request "X-CSRF-Token" with
  | Some token -> Security.constant_time_equal session.Db.csrf_token token
  | None -> false

let guest_csrf request =
  match Dream.cookie ~prefix:None ~decrypt:false ~secure:secure_cookies request guest_csrf_cookie with
  | Some token when String.length token = 64 -> token
  | _ -> Security.random_token 32

let set_guest_csrf response request token =
  Dream.set_cookie ~prefix:None ~encrypt:false ~path:(Some "/") ~secure:secure_cookies
    ~http_only:true ~same_site:(Some `Strict) ~max_age:3600. response request
    guest_csrf_cookie token

let set_session_cookie response request token =
  Dream.set_cookie ~prefix:None ~encrypt:false ~path:(Some "/") ~secure:secure_cookies
    ~http_only:true ~same_site:(Some `Strict) ~max_age:session_lifetime response
    request session_cookie token

let auth_page ~register ~csrf ?error () =
  let heading, action, submit, alternate =
    if register then
      ("Create your account", "/register", "Create account",
       {|Already have an account? <a href="/login">Log in</a>.|})
    else
      ("Welcome back", "/login", "Log in",
       {|New here? <a href="/register">Create an account</a>.|})
  in
  let error = match error with None -> "" | Some message -> Printf.sprintf {|<div class="error">%s</div>|} (escape message) in
  let hint = if register then {|<small>10–256 characters. Usernames may use letters, numbers, _ and -.</small>|} else "" in
  layout ~title:heading
    (Printf.sprintf
       {|<section class="auth-card"><p class="eyebrow">LOCAL STUDY WORKSPACE</p><h1>%s</h1>%s<form class="stack" action="%s" method="post"><input type="hidden" name="csrf" value="%s"><label>Username<input name="username" autocomplete="username" required minlength="3" maxlength="32"></label><label>Password<input type="password" name="password" autocomplete="%s" required minlength="10" maxlength="256"></label>%s<button class="button" type="submit">%s</button></form><p class="muted">%s</p></section>|}
       heading error action (escape csrf)
       (if register then "new-password" else "current-password") hint submit alternate)

let show_auth_page ~register request =
  let csrf = guest_csrf request in
  Dream.html (auth_page ~register ~csrf ()) >|= fun response ->
  set_guest_csrf response request csrf;
  response

let throttle : (string, float list) Hashtbl.t = Hashtbl.create 31

let login_allowed username =
  let key = String.lowercase_ascii username in
  let cutoff = Unix.gettimeofday () -. 300. in
  let recent = Hashtbl.find_opt throttle key |> Option.value ~default:[] |> List.filter ((<) cutoff) in
  Hashtbl.replace throttle key recent;
  List.length recent < 6

let record_login_failure username =
  let key = String.lowercase_ascii username in
  let recent = Hashtbl.find_opt throttle key |> Option.value ~default:[] in
  Hashtbl.replace throttle key (Unix.gettimeofday () :: recent)

let clear_login_failures username = Hashtbl.remove throttle (String.lowercase_ascii username)

let handle_register db request =
  form_fields request >>= function
  | Error message -> Dream.html ~status:`Bad_Request (auth_page ~register:true ~csrf:(guest_csrf request) ~error:message ())
  | Ok fields ->
      let csrf = field "csrf" fields and cookie = guest_csrf request in
      if not (Security.constant_time_equal csrf cookie) then
        Dream.html ~status:`Forbidden (auth_page ~register:true ~csrf:cookie ~error:"Security token mismatch. Reload and try again." ())
      else
        let username = String.trim (field "username" fields) in
        let password = field "password" fields in
        if not (Security.valid_username username) then
          Dream.html ~status:`Bad_Request (auth_page ~register:true ~csrf:cookie ~error:"Use a 3–32 character username containing only letters, numbers, _ or -." ())
        else if not (Security.valid_password password) then
          Dream.html ~status:`Bad_Request (auth_page ~register:true ~csrf:cookie ~error:"Password must be between 10 and 256 characters." ())
        else
          match Security.hash_password password with
          | Error message -> Dream.html ~status:`Internal_Server_Error (auth_page ~register:true ~csrf:cookie ~error:message ())
          | Ok password_hash ->
              Lwt.catch
                (fun () ->
                  Db.create_user db ~username ~password_hash >>= fun user ->
                  let token = Security.random_token 32 and csrf_token = Security.random_token 32 in
                  let expires_at = Int64.of_float (Unix.gettimeofday () +. session_lifetime) in
                  Db.create_session db ~user_id:user.id ~token_hash:(Security.sha256 token) ~csrf_token ~expires_at >>= fun () ->
                  Dream.redirect request "/" >|= fun response -> set_session_cookie response request token; response)
                (fun _ -> Dream.html ~status:`Conflict (auth_page ~register:true ~csrf:cookie ~error:"That username is already registered." ()))

let handle_login db request =
  form_fields request >>= function
  | Error message -> Dream.html ~status:`Bad_Request (auth_page ~register:false ~csrf:(guest_csrf request) ~error:message ())
  | Ok fields ->
      let csrf = field "csrf" fields and cookie = guest_csrf request in
      if not (Security.constant_time_equal csrf cookie) then
        Dream.html ~status:`Forbidden (auth_page ~register:false ~csrf:cookie ~error:"Security token mismatch. Reload and try again." ())
      else
        let username = String.trim (field "username" fields) in
        let password = field "password" fields in
        if not (login_allowed username) then
          Dream.html ~status:`Too_Many_Requests (auth_page ~register:false ~csrf:cookie ~error:"Too many failed logins. Wait five minutes and try again." ())
        else
          Db.find_user_auth db username >>= function
          | Some (user_id, stored_username, password_hash)
            when Security.verify_password ~encoded:password_hash password ->
              clear_login_failures username;
              let token = Security.random_token 32 and csrf_token = Security.random_token 32 in
              let expires_at = Int64.of_float (Unix.gettimeofday () +. session_lifetime) in
              Db.create_session db ~user_id ~token_hash:(Security.sha256 token) ~csrf_token ~expires_at >>= fun () ->
              Dream.redirect request "/" >|= fun response -> set_session_cookie response request token; response
          | _ ->
              record_login_failure username;
              Dream.html ~status:`Unauthorized (auth_page ~register:false ~csrf:cookie ~error:"Username or password is incorrect." ())

let handle_logout db session request =
  form_fields request >>= function
  | Ok fields when valid_csrf session request fields ->
      let token = Dream.cookie ~prefix:None ~decrypt:false ~secure:secure_cookies request session_cookie in
      (match token with None -> Lwt.return_unit | Some token -> Db.delete_session db (Security.sha256 token)) >>= fun () ->
      Dream.redirect request "/login" >|= fun response ->
      Dream.drop_cookie ~prefix:None ~path:(Some "/") ~secure:secure_cookies response request session_cookie;
      response
  | _ -> Dream.empty `Forbidden

let dashboard db session request =
  Db.list_attempts db session.Db.user.id >>= fun attempts ->
  let rows =
    attempts
    |> List.map (fun (attempt : Db.attempt_summary) ->
           Printf.sprintf
             {|<a class="attempt-row" href="/attempts/%d"><span><strong>Practice set #%d</strong><small>%s · %s</small></span><span class="status status-%s">%s</span></a>|}
             attempt.id attempt.id (escape attempt.selected_modules)
             (format_time attempt.created_at) (escape attempt.status)
             (escape (status_label attempt.status)))
    |> String.concat ""
  in
  let rows = if rows = "" then {|<div class="empty"><h3>No practice sets yet</h3><p>Generate one to start building your progress history.</p></div>|} else rows in
  Dream.html
    (layout ~user:session ~title:"Dashboard"
       (Printf.sprintf
          {|<section class="hero"><div><p class="eyebrow">YOUR REVISION DESK</p><h1>Train on the questions that still challenge you.</h1><p>Correct answers retire permanently. Wrong and skipped questions stay in the pool.</p></div><a class="button" href="/attempts/new">Generate a test set</a></section><section class="stats-strip"><div><strong>80%%</strong><span>hard questions</span></div><div><strong>20%%</strong><span>medium questions</span></div><div><strong>0</strong><span>completed repeats</span></div></section><section><div class="section-heading"><h2>Practice history</h2></div><div class="attempt-list">%s</div></section>|}
          rows))

let new_attempt_page session =
  let cards =
    all_modules
    |> List.map (fun kind ->
           let minutes = module_duration_seconds kind / 60 in
           Printf.sprintf
             {|<label class="module-card"><input type="checkbox" name="module" value="%s"><span class="checkmark"></span><span><strong>%s</strong><small>%d questions · %d minutes</small></span></label>|}
             (module_to_string kind) (escape (module_label kind))
             (module_question_count kind) minutes)
    |> String.concat ""
  in
  layout ~user:session ~title:"New practice set"
    (Printf.sprintf
       {|<section class="narrow"><a class="back" href="/">← Dashboard</a><p class="eyebrow">BUILD A PRACTICE SET</p><h1>Choose exactly what you want to train.</h1><p class="lede">Every module uses a hard-heavy blueprint: about 80%% hard and 20%% medium, with no easy questions.</p><form class="stack" action="/attempts" method="post"><input type="hidden" name="csrf" value="%s"><div class="module-grid">%s</div><button class="button" type="submit">Generate selected modules</button></form></section>|}
       (escape session.Db.csrf_token) cards)

let create_attempt db session request =
  form_fields request >>= function
  | Ok fields when valid_csrf session request fields ->
      let modules = parse_modules fields in
      if modules = [] then Dream.html ~status:`Bad_Request (new_attempt_page session)
      else
        Db.eligible_questions db session.Db.user.id >>= fun eligible ->
        (match Generator.generate ~eligible modules with
         | Error message -> Dream.html ~status:`Conflict (layout ~user:session ~title:"Question pool unavailable" (Printf.sprintf {|<section class="card"><h1>Not enough eligible questions</h1><p>%s</p><p>Refresh the metadata or choose fewer modules.</p><a class="button" href="/attempts/new">Back</a></section>|} (escape message)))
         | Ok assignments ->
             Db.create_attempt db ~user_id:session.Db.user.id assignments >>= fun attempt_id ->
             Dream.redirect request (Printf.sprintf "/attempts/%d" attempt_id))
  | _ -> Dream.empty `Forbidden

let module_action module_ =
  match module_.Db.status with
  | "pending" -> "Start module"
  | "active" -> "Resume module"
  | "grading_pending" -> "Retry grading"
  | "submitted" -> "Completed"
  | _ -> "Open"

let attempt_page db session request =
  match int_param request "attempt_id" with
  | None -> not_found ()
  | Some attempt_id ->
      Db.get_attempt db ~user_id:session.Db.user.id attempt_id >>= function
      | None -> not_found ()
      | Some attempt ->
          Db.get_attempt_modules db attempt_id >>= fun modules ->
          let cards =
            modules
            |> List.map (fun (module_ : Db.attempt_module) ->
                   let action = module_action module_ in
                   let button =
                     match module_.status with
                     | "submitted" -> {|<span class="complete-mark">✓ Submitted</span>|}
                     | "active" -> Printf.sprintf {|<a class="button button-small" href="/attempts/%d/modules/%d/take?question=1">%s</a>|} attempt_id module_.id action
                     | "grading_pending" -> Printf.sprintf {|<form action="/attempts/%d/modules/%d/submit" method="post"><input type="hidden" name="csrf" value="%s"><button class="button button-small" type="submit">%s</button></form>|} attempt_id module_.id (escape session.csrf_token) action
                     | _ -> Printf.sprintf {|<form action="/attempts/%d/modules/%d/start" method="post"><input type="hidden" name="csrf" value="%s"><button class="button button-small" type="submit">%s</button></form>|} attempt_id module_.id (escape session.csrf_token) action
                   in
                   Printf.sprintf {|<article class="module-summary"><div><p class="eyebrow">%s</p><h3>%s</h3><p>%d questions · %d minutes</p></div><div class="module-action"><span class="status status-%s">%s</span>%s</div></article>|}
                     (if module_.relaxed_blueprint then "RELAXED BLUEPRINT" else "HARD-HEAVY BLUEPRINT")
                     (escape (module_label module_.kind)) module_.question_count
                     (module_.duration_seconds / 60) (escape module_.status)
                     (escape (status_label module_.status)) button)
            |> String.concat ""
          in
          let result_link =
            if List.exists (fun (m : Db.attempt_module) -> m.status = "submitted") modules then
              Printf.sprintf {|<a class="button button-secondary" href="/attempts/%d/results">View results</a>|} attempt_id
            else ""
          in
          Dream.html (layout ~user:session ~title:(Printf.sprintf "Practice set #%d" attempt_id)
            (Printf.sprintf {|<section class="narrow wide"><a class="back" href="/">← Dashboard</a><div class="section-heading"><div><p class="eyebrow">PRACTICE SET #%d</p><h1>Your selected modules</h1><p>Breaks between modules are untimed. Once started, a module's clock does not pause.</p></div>%s</div><div class="module-list">%s</div></section>|} attempt_id result_link cards))

let owned_module db session request =
  match int_param request "attempt_id", int_param request "module_id" with
  | Some attempt_id, Some module_id ->
      Db.get_module db ~user_id:session.Db.user.id ~attempt_id module_id
      >|= Option.map (fun module_ -> (attempt_id, module_))
  | _ -> Lwt.return_none

let start_module db session request =
  form_fields request >>= function
  | Ok fields when valid_csrf session request fields ->
      owned_module db session request >>= (function
        | None -> not_found ()
        | Some (attempt_id, module_) ->
            Db.start_module db module_ >>= fun module_ ->
            Dream.redirect request (Printf.sprintf "/attempts/%d/modules/%d/take?question=1" attempt_id module_.id))
  | _ -> Dream.empty `Forbidden

let json_assoc json name = match json with `Assoc fields -> List.assoc_opt name fields | _ -> None
let json_string json name = match json_assoc json name with Some (`String value) -> Some value | _ -> None
let json_bool json name = match json_assoc json name with Some (`Bool value) -> Some value | _ -> None

let grade_module db ~user_id module_ =
  Db.lock_module_for_grading db module_ >>= fun () ->
  Db.grading_questions db module_.Db.id >>= fun questions ->
  College_board.map_bounded ~concurrency:4
    (fun (question : Db.grading_question) ->
      College_board.get_question question.external_id >|= fun detail -> (question, detail))
    questions
  >>= fun fetched ->
  let rec gather grades (items : (Db.grading_question * (question_detail, string) result) list) =
    match items with
    | [] -> Ok (List.rev grades)
    | (_question, Error message) :: _ -> Error message
    | (question, Ok detail) :: rest ->
        let answer = Option.value ~default:"" question.Db.answer in
        let correct = Score.is_correct ~submitted:answer ~accepted:detail.correct_answers in
        gather ((question, correct) :: grades) rest
  in
  match gather [] fetched with
  | Error message -> Lwt.return (Error message)
  | Ok grades ->
      Lwt_list.iter_s
        (fun ((question : Db.grading_question), detail_result) -> match detail_result with
          | Error _ -> Lwt.return_unit
          | Ok detail -> Db.update_item_type db question.Db.external_id detail.item_type)
        fetched
      >>= fun () ->
      Db.apply_grades db ~user_id module_ grades >>= fun () ->
      Db.get_attempt db ~user_id module_.attempt_id >>= function
      | None -> Lwt.return (Error "Attempt disappeared while grading")
      | Some attempt ->
          Db.section_counts db attempt.id >>= fun counts ->
          Db.get_attempt_modules db attempt.id >>= fun modules ->
          let complete kind = List.exists (fun (m : Db.attempt_module) -> m.kind=kind && m.status="submitted") modules in
          let range_for section correct total =
            let both = match section with
              | Reading_writing -> complete Reading_writing_1 && complete Reading_writing_2
              | Math -> complete Math_1 && complete Math_2
            in
            if both then Some (Score.section_score section ~correct ~total) else None
          in
          let rows = List.map (fun (section, correct, total) -> section, correct, total, range_for section correct total) counts in
          Db.save_scores db attempt.id rows >|= fun () -> Ok ()

let submit_module db session request =
  form_fields request >>= function
  | Ok fields when valid_csrf session request fields ->
      owned_module db session request >>= (function
        | None -> not_found ()
        | Some (attempt_id, module_) ->
            grade_module db ~user_id:session.Db.user.id module_ >>= function
            | Ok () -> Dream.redirect request (Printf.sprintf "/attempts/%d/results" attempt_id)
            | Error message ->
                Dream.html ~status:`Service_Unavailable
                  (layout ~user:session ~title:"Grading pending"
                     (Printf.sprintf {|<section class="card"><p class="eyebrow">RESPONSES SAVED</p><h1>Grading is pending</h1><p>The live question content is unavailable right now. Your responses are locked safely and can be graded later.</p><p class="muted">%s</p><a class="button" href="/attempts/%d">Return to attempt</a></section>|} (escape message) attempt_id)))
  | _ -> Dream.empty `Forbidden

let deadline_expired module_ =
  match module_.Db.deadline_at with Some deadline -> Int64.compare (Db.now ()) deadline >= 0 | None -> false

let answer_control (assigned : Db.assigned_question) (detail : question_detail) =
  match detail.item_type with
  | Student_response ->
      Printf.sprintf {|<label class="spr-label">Your answer<input id="answer-input" class="spr-input" autocomplete="off" inputmode="decimal" value="%s"></label>|}
        (escape (Option.value ~default:"" assigned.answer))
  | Multiple_choice ->
      detail.answer_options
      |> List.map (fun option ->
             let checked = match assigned.answer with Some answer when String.equal answer option.letter -> " checked" | _ -> "" in
             Printf.sprintf {|<label class="choice"><input type="radio" name="answer" value="%s"%s><span class="choice-letter">%s</span><span class="choice-content">%s</span></label>|}
               (escape option.letter) checked (escape option.letter) option.content)
      |> String.concat ""

let palette_html attempt_id module_id current states =
  states
  |> List.map (fun (state : Db.question_state) ->
         let classes = [ if state.position=current then "current" else ""; if state.answered then "answered" else "unanswered"; if state.flagged then "flagged" else "" ] |> List.filter ((<>) "") |> String.concat " " in
         Printf.sprintf {|<a class="palette-item %s" href="/attempts/%d/modules/%d/take?question=%d" aria-label="Question %d%s">%d%s</a>|}
           classes attempt_id module_id state.position state.position
           (if state.flagged then ", marked for review" else "") state.position
           (if state.flagged then {|<span>★</span>|} else ""))
  |> String.concat ""

let take_module db session request =
  owned_module db session request >>= function
  | None -> not_found ()
  | Some (attempt_id, module_) when module_.status = "submitted" -> Dream.redirect request (Printf.sprintf "/attempts/%d/results" attempt_id)
  | Some (attempt_id, module_) when module_.status = "grading_pending" -> Dream.redirect request (Printf.sprintf "/attempts/%d" attempt_id)
  | Some (attempt_id, module_) when module_.status = "pending" -> Dream.redirect request (Printf.sprintf "/attempts/%d" attempt_id)
  | Some (attempt_id, module_) when deadline_expired module_ ->
      grade_module db ~user_id:session.Db.user.id module_ >>= fun _ -> Dream.redirect request (Printf.sprintf "/attempts/%d/results" attempt_id)
  | Some (attempt_id, module_) ->
      let position = Option.bind (Dream.query request "question") int_of_string_opt |> Option.value ~default:1 |> max 1 |> min module_.question_count in
      Db.get_question db ~user_id:session.Db.user.id ~module_id:module_.id position >>= function
      | None -> not_found ()
      | Some assigned ->
          College_board.get_question assigned.external_id >>= (function
            | Error message ->
                Dream.html ~status:`Service_Unavailable
                  (layout ~user:session ~title:"Question unavailable"
                     (Printf.sprintf {|<section class="card"><h1>This question could not be loaded</h1><p>Your timer is still running. Retry shortly or move to another question from the attempt.</p><p class="muted">%s</p><a class="button" href="/attempts/%d/modules/%d/take?question=%d">Retry</a></section>|} (escape message) attempt_id module_.id position))
            | Ok detail ->
                Db.update_item_type db assigned.external_id detail.item_type >>= fun () ->
                Db.question_states db ~user_id:session.Db.user.id ~module_id:module_.id >>= fun states ->
                let previous = if position > 1 then Printf.sprintf {|<a class="button button-secondary" href="/attempts/%d/modules/%d/take?question=%d">Previous</a>|} attempt_id module_.id (position-1) else {|<span></span>|} in
                let next = if position < module_.question_count then Printf.sprintf {|<a class="button" href="/attempts/%d/modules/%d/take?question=%d">Next</a>|} attempt_id module_.id (position+1) else {|<button id="submit-module" class="button" type="button">Finish module</button>|} in
                let deadline = Option.value ~default:(Int64.add (Db.now ()) (Int64.of_int module_.duration_seconds)) module_.deadline_at in
                let question_html = detail.stimulus ^ detail.stem in
                let body =
                  Printf.sprintf
                    {|<div id="test-app" data-attempt-id="%d" data-module-id="%d" data-question-id="%d" data-deadline="%Ld" data-csrf="%s" data-results-url="/attempts/%d/results"><header class="test-header"><a class="brand" href="/attempts/%d"><span>SAT</span> Revision</a><strong>%s</strong><div id="timer" class="timer">--:--</div></header><div class="test-layout"><aside class="palette"><h2>Questions</h2><div class="palette-grid">%s</div><div class="palette-key"><span>● Answered</span><span>○ Unanswered</span><span>★ Review</span></div></aside><main class="question-pane"><div class="question-meta"><span>Question %d of %d</span><span>%s · %s</span></div><article class="question-content">%s</article><div id="answer-control" class="answers">%s</div><div class="question-tools"><label><input id="flag-input" type="checkbox"%s> Mark for review</label><span id="save-state" aria-live="polite">Saved</span></div><nav class="question-nav">%s%s</nav></main></div></div><script src="/static/test.js" defer></script>|}
                    attempt_id module_.id assigned.id deadline (escape session.csrf_token) attempt_id attempt_id
                    (escape (module_label module_.kind)) (palette_html attempt_id module_.id position states)
                    position module_.question_count (escape assigned.domain_name) (escape (difficulty_label assigned.difficulty))
                    question_html (answer_control assigned detail) (if assigned.flagged then " checked" else "") previous next
                in Dream.html body)

let api_question db session request =
  match int_param request "attempt_id", int_param request "module_id", int_param request "position" with
  | Some attempt_id, Some module_id, Some position ->
      Db.get_module db ~user_id:session.Db.user.id ~attempt_id module_id >>= (function
        | None -> Dream.json ~status:`Not_Found {|{"error":"not found"}|}
        | Some module_ when module_.status <> "active" || deadline_expired module_ -> Dream.json ~status:`Conflict {|{"error":"module is not active"}|}
        | Some _ ->
            Db.get_question db ~user_id:session.Db.user.id ~module_id position >>= function
            | None -> Dream.json ~status:`Not_Found {|{"error":"not found"}|}
            | Some assigned -> College_board.get_question assigned.external_id >>= function
                | Error message -> Dream.json ~status:`Service_Unavailable (Yojson.Safe.to_string (`Assoc ["error", `String message]))
                | Ok detail ->
                    let options = `List (List.map (fun option -> `Assoc ["letter",`String option.letter; "content",`String option.content]) detail.answer_options) in
                    Dream.json (Yojson.Safe.to_string (`Assoc ["position",`Int position; "stimulus",`String detail.stimulus; "stem",`String detail.stem; "type",`String (item_type_to_string detail.item_type); "options",options; "answer", (match assigned.answer with None -> `Null | Some a -> `String a); "flagged",`Bool assigned.flagged])))
  | _ -> Dream.json ~status:`Bad_Request {|{"error":"invalid identifier"}|}

let api_save db session request =
  if not (valid_json_csrf session request) then Dream.json ~status:`Forbidden {|{"error":"CSRF rejected"}|}
  else
    owned_module db session request >>= function
    | None -> Dream.json ~status:`Not_Found {|{"error":"not found"}|}
    | Some (_, module_) when module_.status <> "active" || deadline_expired module_ -> Dream.json ~status:`Conflict {|{"error":"module is locked"}|}
    | Some (_, module_) ->
        Dream.body request >>= fun body ->
        (try
           let json = Yojson.Safe.from_string body in
           let question_id = match json_assoc json "question_id" with Some (`Int value) -> Some value | _ -> None in
           match question_id with
           | None -> Dream.json ~status:`Bad_Request {|{"error":"question_id is required"}|}
           | Some question_id ->
               let answer = match json_assoc json "answer" with Some (`String value) -> Some value | Some `Null -> None | _ -> None in
               let flagged = json_bool json "flagged" in
               (match json_assoc json "answer" with
                | Some _ -> Db.save_answer db ~module_id:module_.id ~question_id answer
                | None -> Lwt.return_unit) >>= fun () ->
               (match flagged with
                | Some value -> Db.save_flag db ~module_id:module_.id ~question_id value
                | None -> Lwt.return_unit) >>= fun () ->
               Dream.json {|{"saved":true}|}
         with Yojson.Json_error _ -> Dream.json ~status:`Bad_Request {|{"error":"invalid JSON"}|})

let api_submit db session request =
  if not (valid_json_csrf session request) then Dream.json ~status:`Forbidden {|{"error":"CSRF rejected"}|}
  else
    owned_module db session request >>= function
    | None -> Dream.json ~status:`Not_Found {|{"error":"not found"}|}
    | Some (_, module_) ->
        grade_module db ~user_id:session.Db.user.id module_ >>= function
        | Ok () -> Dream.json {|{"submitted":true}|}
        | Error message -> Dream.json ~status:`Service_Unavailable (Yojson.Safe.to_string (`Assoc ["error",`String message; "grading_pending",`Bool true]))

let results_page db session request =
  match int_param request "attempt_id" with
  | None -> not_found ()
  | Some attempt_id ->
      Db.get_attempt db ~user_id:session.Db.user.id attempt_id >>= function
      | None -> not_found ()
      | Some attempt ->
          Db.scores db attempt_id >>= fun scores ->
          Db.breakdown db attempt_id >>= fun breakdown ->
          let score_cards =
            scores |> List.map (fun (score : Db.score_row) ->
              let percent = if score.total=0 then 0 else (score.correct * 100) / score.total in
              let estimate = match score.range with
                | None -> {|<span class="score-pending">Complete both modules in this section for an estimate.</span>|}
                | Some range -> Printf.sprintf {|<span class="estimate">~%d</span><small>published fixed-form range %d–%d</small>|} range.estimate range.low range.high
              in
              Printf.sprintf {|<article class="score-card"><p class="eyebrow">%s</p><div class="score-main"><strong>%d%%</strong><span>%d / %d correct</span></div>%s</article>|}
                (escape (section_label score.section)) percent score.correct score.total estimate)
            |> String.concat ""
          in
          let total =
            let find section = Option.bind (List.find_opt (fun (row : Db.score_row) -> row.section=section) scores) (fun row -> row.range) in
            match find Reading_writing, find Math with
            | Some rw, Some math -> let range = Score.total_score rw math in Printf.sprintf {|<div class="total-score"><span>Unofficial total estimate</span><strong>~%d</strong><small>range %d–%d</small></div>|} range.estimate range.low range.high
            | _ -> ""
          in
          let breakdown_rows =
            breakdown |> List.map (fun (row : Db.breakdown_row) ->
              Printf.sprintf {|<tr><td>%s</td><td>%s</td><td>%s</td><td><strong>%d / %d</strong></td></tr>|}
                (escape (section_label row.section)) (escape row.domain) (escape (difficulty_label row.difficulty)) row.correct row.total)
            |> String.concat ""
          in
          let pending = if attempt.status="grading_pending" then {|<div class="notice">One or more modules are locked with grading pending. Return to the attempt to retry.</div>|} else "" in
          Dream.html (layout ~user:session ~title:"Results"
            (Printf.sprintf {|<section class="narrow wide"><a class="back" href="/attempts/%d">← Practice set</a><div class="section-heading"><div><p class="eyebrow">RESULTS</p><h1>Practice set #%d</h1></div><a class="button button-secondary" href="/attempts/%d/review">Review mistakes</a></div>%s<div class="score-grid">%s%s</div><p class="disclaimer">Score projections are unofficial. They scale accuracy to SAT Practice Test 8's published fixed-form raw score table; the real digital SAT uses unpublished item parameters and adaptive scoring.</p><section class="card"><h2>Performance breakdown</h2><div class="table-wrap"><table><thead><tr><th>Section</th><th>Domain</th><th>Difficulty</th><th>Correct</th></tr></thead><tbody>%s</tbody></table></div></section></section>|}
              attempt_id attempt_id attempt_id pending score_cards total breakdown_rows))

let review_page db session request =
  match int_param request "attempt_id" with
  | None -> not_found ()
  | Some attempt_id ->
      Db.get_attempt db ~user_id:session.Db.user.id attempt_id >>= function
      | None -> not_found ()
      | Some _ ->
          Db.review_questions db ~user_id:session.Db.user.id attempt_id >>= fun questions ->
          let index = Option.bind (Dream.query request "item") int_of_string_opt |> Option.value ~default:1 |> max 1 in
          match List.nth_opt questions (index-1) with
          | None ->
              Dream.html (layout ~user:session ~title:"Review" (Printf.sprintf {|<section class="card"><p class="eyebrow">REVIEW</p><h1>No mistakes to review</h1><p>Every submitted question in this practice set was correct.</p><a class="button" href="/attempts/%d/results">Back to results</a></section>|} attempt_id))
          | Some question ->
              College_board.get_question question.external_id >>= function
              | Error message -> Dream.html ~status:`Service_Unavailable (layout ~user:session ~title:"Review unavailable" (Printf.sprintf {|<section class="card"><h1>Review content is temporarily unavailable</h1><p>%s</p></section>|} (escape message)))
              | Ok detail ->
                  let choices = match detail.item_type with
                    | Student_response -> ""
                    | Multiple_choice -> detail.answer_options |> List.map (fun option -> Printf.sprintf {|<div class="review-choice"><strong>%s</strong>%s</div>|} (escape option.letter) option.content) |> String.concat ""
                  in
                  let previous = if index > 1 then Printf.sprintf {|<a class="button button-secondary" href="/attempts/%d/review?item=%d">Previous</a>|} attempt_id (index-1) else {|<span></span>|} in
                  let next = if index < List.length questions then Printf.sprintf {|<a class="button" href="/attempts/%d/review?item=%d">Next mistake</a>|} attempt_id (index+1) else Printf.sprintf {|<a class="button" href="/attempts/%d/results">Done</a>|} attempt_id in
                  Dream.html (layout ~user:session ~title:"Review mistakes"
                    (Printf.sprintf {|<section class="narrow"><a class="back" href="/attempts/%d/results">← Results</a><div class="question-meta"><span>Mistake %d of %d · %s</span><span>%s · %s</span></div><article class="card review-card"><div class="question-content">%s%s</div><div class="review-choices">%s</div><div class="answer-comparison"><div><span>Your answer</span><strong>%s</strong></div><div class="correct"><span>Correct answer</span><strong>%s</strong></div></div><section class="rationale"><h2>Why</h2>%s</section></article><nav class="question-nav">%s%s</nav></section>|}
                      attempt_id index (List.length questions) (escape (module_label question.module_kind))
                      (escape question.domain) (escape (difficulty_label question.difficulty)) detail.stimulus detail.stem choices
                      (escape (Option.value ~default:"Unanswered" question.answer))
                      (escape (String.concat " or " detail.correct_answers)) detail.rationale previous next))

let refresh_metadata db =
  Db.metadata_synced_at db >>= fun last_sync ->
  let stale = match last_sync with None -> true | Some timestamp -> Int64.to_float timestamp < Unix.gettimeofday () -. 86400. in
  if not stale then Lwt.return (Ok `Fresh)
  else
    College_board.fetch_all_metadata () >>= function
    | Error message -> Lwt.return (Error message)
    | Ok questions -> Db.upsert_metadata db questions >|= fun () -> Ok (`Updated (List.length questions))

let rec expiry_worker db =
  Lwt_unix.sleep 30. >>= fun () ->
  Lwt.catch
    (fun () ->
      Db.expired_or_pending_modules db >>= fun modules ->
      Lwt_list.iter_s
        (fun (user_id, module_) ->
          grade_module db ~user_id module_ >|= fun _ -> ())
        modules)
    (fun error ->
      Printf.eprintf "Automatic grading pass failed: %s\n%!" (Printexc.to_string error);
      Lwt.return_unit)
  >>= fun () -> expiry_worker db

let routes db =
  Dream.router
    [
      Dream.get "/static/**" (Dream.static "static");
      Dream.get "/healthz" (fun _ -> Dream.json {|{"status":"ok"}|});
      Dream.get "/register" (show_auth_page ~register:true);
      Dream.post "/register" (handle_register db);
      Dream.get "/login" (show_auth_page ~register:false);
      Dream.post "/login" (handle_login db);
      Dream.post "/logout" (require_session (handle_logout db));
      Dream.get "/" (require_session (dashboard db));
      Dream.get "/attempts/new" (require_session (fun session _ -> Dream.html (new_attempt_page session)));
      Dream.post "/attempts" (require_session (create_attempt db));
      Dream.get "/attempts/:attempt_id" (require_session (attempt_page db));
      Dream.post "/attempts/:attempt_id/modules/:module_id/start" (require_session (start_module db));
      Dream.get "/attempts/:attempt_id/modules/:module_id/take" (require_session (take_module db));
      Dream.post "/attempts/:attempt_id/modules/:module_id/submit" (require_session (submit_module db));
      Dream.get "/attempts/:attempt_id/results" (require_session (results_page db));
      Dream.get "/attempts/:attempt_id/review" (require_session (review_page db));
      Dream.get "/api/attempts/:attempt_id/modules/:module_id/questions/:position" (fun request -> match current_session request with Some session -> api_question db session request | None -> unauthorized ());
      Dream.post "/api/attempts/:attempt_id/modules/:module_id/save" (fun request -> match current_session request with Some session -> api_save db session request | None -> unauthorized ());
      Dream.post "/api/attempts/:attempt_id/modules/:module_id/submit" (fun request -> match current_session request with Some session -> api_submit db session request | None -> unauthorized ());
    ]

let handler db = security_headers @@ session_middleware db @@ routes db
