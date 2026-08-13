open Sat
open Model
open Lwt.Infix

let contains haystack needle =
  try ignore (Str.search_forward (Str.regexp_string needle) haystack 0); true
  with Not_found -> false

let check_not_contains label needle value =
  Alcotest.(check bool) label false (contains (String.lowercase_ascii value) (String.lowercase_ascii needle))

let metadata ?(item_type = None) section difficulty domain_code index =
  {
    external_id = Printf.sprintf "%s-%s-%s-%04d"
        (section_to_string section) domain_code (difficulty_to_string difficulty) index;
    question_id = Printf.sprintf "q-%d" index;
    section;
    difficulty;
    domain_code;
    domain_name = domain_code ^ " domain";
    skill_code = "SK";
    skill_name = "Skill";
    item_type;
    source_updated_at = 1L;
    synced_at = 1L;
  }

let fixture_pool () =
  let cells =
    [
      (Reading_writing, "CAS"); (Reading_writing, "INI");
      (Reading_writing, "SEC"); (Reading_writing, "EOI");
      (Math, "H"); (Math, "P"); (Math, "Q"); (Math, "S");
    ]
  in
  List.concat_map
    (fun (section, domain) ->
      List.concat_map
        (fun difficulty ->
          List.init 40 (fun index -> metadata section difficulty domain index))
        [ Easy; Medium; Hard ])
    cells

let test_sanitize () =
  let dirty =
    {|<div onclick="steal()"><script>alert(1)</script><p style="color:red;text-align:center">Safe</p><a href="javascript:alert(1)">link</a><math><mfrac><mn>1</mn><mn>2</mn></mfrac><mfenced open="|" close="|" separators=","><mi>x</mi></mfenced></math><svg onload="bad()" viewBox="0 0 10 10"><defs><clipPath id="clip1"><rect width="4" height="4"/></clipPath></defs><foreignObject><script>x</script></foreignObject><path d="M0 0L1 1" clip-path="url(#clip1)" style="fill:none;stroke:#000000;stroke-width:0.7;stroke-dasharray:3, 2;clip-path:url(#clip1)"/><circle cx="1" cy="1" r="1" style="fill:url(https://evil.example/x);fill-opacity:0.5;behavior:url(x)"/><use xlink:href="#glyph-1"/><use href="#glyph-2"/><use xlink:href="https://evil.example/x.svg#a"/></svg><img src="https://evil.example/x" onerror="bad()"></div>|}
  in
  let clean = Sanitize.fragment dirty in
  Alcotest.(check bool) "safe text retained" true (contains clean "Safe");
  Alcotest.(check bool) "MathML retained" true (contains clean "mfrac");
  Alcotest.(check bool) "MathML opening fence retained" true (contains clean {|open="|"|});
  Alcotest.(check bool) "MathML closing fence retained" true (contains clean {|close="|"|});
  Alcotest.(check bool) "MathML separators retained" true (contains clean {|separators=","|});
  Alcotest.(check bool) "safe SVG retained" true (contains clean "<path");
  Alcotest.(check bool) "safe style retained" true (contains clean "text-align: center");
  Alcotest.(check bool) "SVG fill retained" true (contains clean "fill: none");
  Alcotest.(check bool) "SVG stroke retained" true (contains clean "stroke: #000000");
  Alcotest.(check bool) "SVG dasharray retained" true (contains clean "stroke-dasharray: 3, 2");
  Alcotest.(check bool) "SVG opacity retained" true (contains clean "fill-opacity: 0.5");
  Alcotest.(check bool) "SVG clip-path style retained" true (contains clean "clip-path: url(#clip1)");
  Alcotest.(check bool) "SVG clip-path attribute retained" true (contains clean {|clip-path="url(#clip1)"|});
  Alcotest.(check bool) "clipPath retained" true (contains (String.lowercase_ascii clean) "clippath");
  Alcotest.(check bool) "use glyph retained" true (contains clean "<use");
  Alcotest.(check bool) "xlink fragment retained" true (contains clean {|href="#glyph-1"|});
  Alcotest.(check bool) "href fragment retained" true (contains clean {|href="#glyph-2"|});
  check_not_contains "scripts removed" "script" clean;
  check_not_contains "events removed" "onclick" clean;
  check_not_contains "SVG events removed" "onload" clean;
  check_not_contains "paint url removed" "url(https" clean;
  check_not_contains "unknown style property removed" "behavior" clean;
  check_not_contains "unsafe external image removed" "evil.example" clean;
  check_not_contains "unsafe href removed" "javascript:" clean;
  check_not_contains "foreignObject removed" "foreignobject" clean

let test_metadata_parsing () =
  let json =
    Yojson.Safe.from_string
      {|[{"external_id":"abc","questionId":"qid","difficulty":"H","primary_class_cd":"INI","primary_class_cd_desc":"Information and Ideas","skill_cd":"INF","skill_desc":"Inferences","updateDate":1700000000000},{"difficulty":"H"}]|}
  in
  match College_board.parse_metadata_list Reading_writing 22L json with
  | Error message -> Alcotest.fail message
  | Ok questions ->
      Alcotest.(check int) "skips malformed upstream row" 1 (List.length questions);
      let question = List.hd questions in
      Alcotest.(check string) "external id" "abc" question.external_id;
      Alcotest.(check string) "domain" "INI" question.domain_code;
      Alcotest.(check string) "difficulty" "H" (difficulty_to_string question.difficulty)

let test_detail_parsing () =
  let json =
    Yojson.Safe.from_string
      {|{"externalid":"abc","type":"mcq","stimulus":"<p onclick='bad()'>Read me</p>","stem":"<script>bad()</script><p>Choose.</p>","rationale":"<p>Because.</p>","answerOptions":[{"content":"<p>One</p>"},{"content":"<p>Two</p>"}],"correct_answer":["B"]}|}
  in
  match College_board.parse_detail json with
  | Error message -> Alcotest.fail message
  | Ok detail ->
      Alcotest.(check string) "id" "abc" detail.external_id;
      Alcotest.(check int) "options" 2 (List.length detail.answer_options);
      Alcotest.(check (list string)) "key" [ "B" ] detail.correct_answers;
      check_not_contains "detail sanitized" "onclick" detail.stimulus;
      check_not_contains "stem sanitized" "script" detail.stem

let count_difficulty difficulty questions =
  List.fold_left (fun total (q : question_metadata) -> total + if q.difficulty=difficulty then 1 else 0) 0 questions

let unique_ids assignments =
  assignments
  |> List.concat_map (fun assignment -> assignment.questions)
  |> List.map (fun (q : question_metadata) -> q.external_id)
  |> List.sort_uniq String.compare
  |> List.length

let test_generator_higher_route_mix () =
  let eligible = fixture_pool () in
  for seed = 1 to 100 do
    match Generator.generate ~seed ~eligible all_modules with
    | Error message -> Alcotest.failf "seed %d: %s" seed message
    | Ok assignments ->
        Alcotest.(check int) "all modules" 4 (List.length assignments);
        Alcotest.(check int) "no duplicate IDs" 98 (unique_ids assignments);
        List.iter
          (fun assignment ->
            let expected = module_question_count assignment.kind in
            Alcotest.(check int) "module length" expected (List.length assignment.questions);
            match assignment.kind with
            | Reading_writing_1 ->
                Alcotest.(check int) "RW easy" 9 (count_difficulty Easy assignment.questions);
                Alcotest.(check int) "RW medium" 9 (count_difficulty Medium assignment.questions);
                Alcotest.(check int) "RW hard" 9 (count_difficulty Hard assignment.questions)
            | Reading_writing_2 ->
                Alcotest.(check int) "RW higher-route easy" 4 (count_difficulty Easy assignment.questions);
                Alcotest.(check int) "RW higher-route medium" 10 (count_difficulty Medium assignment.questions);
                Alcotest.(check int) "RW higher-route hard" 13 (count_difficulty Hard assignment.questions)
            | Math_1 ->
                Alcotest.(check int) "Math easy" 7 (count_difficulty Easy assignment.questions);
                Alcotest.(check int) "Math medium" 8 (count_difficulty Medium assignment.questions);
                Alcotest.(check int) "Math hard" 7 (count_difficulty Hard assignment.questions)
            | Math_2 ->
                Alcotest.(check int) "Math higher-route easy" 3 (count_difficulty Easy assignment.questions);
                Alcotest.(check int) "Math higher-route medium" 8 (count_difficulty Medium assignment.questions);
                Alcotest.(check int) "Math higher-route hard" 11 (count_difficulty Hard assignment.questions))
          assignments
  done

let test_generator_fallback () =
  let eligible = fixture_pool () |> List.filter (fun q -> not (q.section=Reading_writing && q.domain_code="CAS" && q.difficulty=Medium)) in
  match Generator.generate ~seed:7 ~eligible [ Reading_writing_1 ] with
  | Error message -> Alcotest.fail message
  | Ok [ assignment ] ->
      Alcotest.(check bool) "relaxed marker" true assignment.relaxed_blueprint;
      Alcotest.(check int) "still full" 27 (List.length assignment.questions);
      Alcotest.(check bool) "stays within section" true
        (List.for_all (fun (q : question_metadata) -> q.section = Reading_writing) assignment.questions)
  | Ok _ -> Alcotest.fail "unexpected assignment count"

let test_math_student_response_preference () =
  let eligible =
    fixture_pool ()
    |> List.map (fun question ->
           if question.section=Math && String.ends_with ~suffix:"-0000" question.external_id
           then { question with item_type=Some Student_response }
           else question)
  in
  match Generator.generate ~seed:17 ~eligible [ Math_1 ] with
  | Error message -> Alcotest.fail message
  | Ok [ assignment ] ->
      let count = List.fold_left (fun n (q : question_metadata) -> n + if q.item_type=Some Student_response then 1 else 0) 0 assignment.questions in
      Alcotest.(check int) "uses known SPR questions where cells permit" 6 count
  | Ok _ -> Alcotest.fail "unexpected assignment count"

let test_answers () =
  Alcotest.(check string) "whitespace and commas" "1234" (Score.normalize_answer " 1, 2 3\t4 \n");
  Alcotest.(check string) "unicode minus" "-7/2" (Score.normalize_answer "−7 / 2");
  Alcotest.(check string) "leading decimal zero" "0.88" (Score.normalize_answer ".88");
  Alcotest.(check bool) "MCQ correct" true (Score.is_correct ~submitted:"b" ~accepted:["B"]);
  Alcotest.(check bool) "SPR variant" true (Score.is_correct ~submitted:" 1,250 " ~accepted:["1250"; "1.25e3"]);
  Alcotest.(check bool) "omitted leading zero" true (Score.is_correct ~submitted:"0.88" ~accepted:[".88"]);
  Alcotest.(check bool) "negative omitted leading zero" true (Score.is_correct ~submitted:"-.88" ~accepted:["-0.88"]);
  Alcotest.(check bool) "blank wrong" false (Score.is_correct ~submitted:"  " ~accepted:[""]);
  Alcotest.(check bool) "no float tolerance" false (Score.is_correct ~submitted:"0.333" ~accepted:["1/3"])

let test_scoring () =
  let rw_min = Score.section_score Reading_writing ~correct:0 ~total:54 in
  let rw_max = Score.section_score Reading_writing ~correct:54 ~total:54 in
  let math_max = Score.section_score Math ~correct:44 ~total:44 in
  Alcotest.(check int) "RW floor" 200 rw_min.estimate;
  Alcotest.(check int) "RW ceiling" 800 rw_max.estimate;
  Alcotest.(check int) "Math ceiling" 800 math_max.estimate;
  Alcotest.(check int) "scaled raw" 33 (Score.equivalent_raw ~correct:27 ~total:54 ~curve_max:66);
  Array.iteri
    (fun raw (low, high) ->
      let score = Score.section_score Reading_writing ~correct:raw ~total:66 in
      Alcotest.(check int) "RW boundary low" low score.low;
      Alcotest.(check int) "RW boundary high" high score.high)
    Score.reading_writing_ranges;
  Array.iteri
    (fun raw (low, high) ->
      let score = Score.section_score Math ~correct:raw ~total:54 in
      Alcotest.(check int) "Math boundary low" low score.low;
      Alcotest.(check int) "Math boundary high" high score.high)
    Score.math_ranges

let test_passwords () =
  Alcotest.(check bool) "valid username" true (Security.valid_username "learner_01");
  Alcotest.(check bool) "invalid username" false (Security.valid_username "bad name");
  match Security.hash_password "correct horse battery" with
  | Error message -> Alcotest.fail message
  | Ok hash ->
      Alcotest.(check bool) "argon2 verifies" true (Security.verify_password ~encoded:hash "correct horse battery");
      Alcotest.(check bool) "argon2 rejects" false (Security.verify_password ~encoded:hash "wrong password")

let database_test () =
  let path = Filename.temp_file "sat-test-" ".db" in
  let run =
    Db.connect path >>= fun db ->
    Db.create_user db ~username:"alice" ~password_hash:"hash" >>= fun alice ->
    Db.create_user db ~username:"bob" ~password_hash:"hash" >>= fun bob ->
    let expired = Int64.sub (Db.now ()) 1L in
    Db.create_session db ~user_id:alice.id ~token_hash:"expired" ~csrf_token:"csrf" ~expires_at:expired >>= fun () ->
    Db.find_session db "expired" >>= fun missing_session ->
    Alcotest.(check bool) "expired session rejected" true (Option.is_none missing_session);
    let eligible = fixture_pool () in
    Db.upsert_metadata db eligible >>= fun () ->
    Db.eligible_questions db alice.id >>= fun initial ->
    Alcotest.(check int) "metadata inserted" (List.length eligible) (List.length initial);
    let assignments = match Generator.generate ~seed:9 ~eligible:initial [ Math_1 ] with Ok value -> value | Error message -> Alcotest.fail message in
    Db.create_attempt db ~user_id:alice.id assignments >>= fun attempt_id ->
    Db.get_attempt db ~user_id:bob.id attempt_id >>= fun foreign_attempt ->
    Alcotest.(check bool) "cross-user attempt isolation" true (Option.is_none foreign_attempt);
    Db.get_attempt_modules db attempt_id >>= fun modules ->
    let module_ = List.hd modules in
    Db.start_module db module_ >>= fun active_module ->
    Alcotest.(check string) "started" "active" active_module.status;
    Alcotest.(check bool) "deadline stored" true (Option.is_some active_module.deadline_at);
    Db.get_question db ~user_id:alice.id ~module_id:module_.id 1 >>= fun question ->
    let question = Option.get question in
    Lwt.join
      [ Db.save_answer db ~module_id:module_.id ~question_id:question.id (Some "A");
        Db.save_flag db ~module_id:module_.id ~question_id:question.id true ]
    >>= fun () ->
    Db.question_states db ~user_id:alice.id ~module_id:module_.id >>= fun states ->
    let first = List.hd states in
    Alcotest.(check bool) "concurrent answer saved" true first.answered;
    Alcotest.(check bool) "concurrent flag saved" true first.flagged;
    Db.lock_module_for_grading db active_module >>= fun () ->
    Db.grading_questions db module_.id >>= fun grading_questions ->
    let grades = List.mapi (fun index question -> question, index=0) grading_questions in
    Db.apply_grades db ~user_id:alice.id active_module grades >>= fun () ->
    Db.eligible_questions db alice.id >>= fun remaining ->
    Alcotest.(check int) "correct question excluded" (List.length eligible - 1) (List.length remaining);
    Db.get_attempt db ~user_id:alice.id attempt_id >>= fun attempt ->
    Alcotest.(check string) "single module attempt completed" "completed" (Option.get attempt).status;
    Lwt.return_unit
  in
  Lwt_main.run run

let application_security_test () =
  let path = Filename.temp_file "sat-app-test-" ".db" in
  let db, attempt_id, alice_id =
    Lwt_main.run
      (Db.connect path >>= fun db ->
       Db.create_user db ~username:"route_alice" ~password_hash:"hash" >>= fun alice ->
       Db.create_user db ~username:"route_bob" ~password_hash:"hash" >>= fun bob ->
       let expires_at = Int64.add (Db.now ()) 3600L in
       Db.create_session db ~user_id:alice.id ~token_hash:(Security.sha256 "alice-token") ~csrf_token:"alice-csrf" ~expires_at >>= fun () ->
       Db.create_session db ~user_id:bob.id ~token_hash:(Security.sha256 "bob-token") ~csrf_token:"bob-csrf" ~expires_at >>= fun () ->
       let eligible = fixture_pool () in
       Db.upsert_metadata db eligible >>= fun () ->
       let assignments = match Generator.generate ~seed:91 ~eligible [ Reading_writing_1 ] with Ok value -> value | Error message -> Alcotest.fail message in
       Db.create_attempt db ~user_id:alice.id assignments >|= fun attempt_id -> db, attempt_id, alice.id)
  in
  let call = Dream.test (App.handler db) in
  let alice_headers = [ "Cookie", "sat_session=alice-token" ] in
  let protected = call (Dream.request ~target:"/attempts/new" ~headers:alice_headers "") in
  Alcotest.(check int) "valid session accepted" 200 (Dream.status_to_int (Dream.status protected));
  let rejected =
    call
      (Dream.request ~method_:`POST ~target:"/attempts"
         ~headers:(("Content-Type", "application/x-www-form-urlencoded") :: alice_headers)
         "csrf=wrong&module=rw1")
  in
  Alcotest.(check int) "CSRF rejected" 403 (Dream.status_to_int (Dream.status rejected));
  let bob_headers = [ "Cookie", "sat_session=bob-token" ] in
  let isolated = call (Dream.request ~target:(Printf.sprintf "/attempts/%d" attempt_id) ~headers:bob_headers "") in
  Alcotest.(check int) "cross-user route hidden" 404 (Dream.status_to_int (Dream.status isolated));
  let form = ("Content-Type", "application/x-www-form-urlencoded") in
  let delete_target = Printf.sprintf "/attempts/%d/delete" attempt_id in
  let foreign_delete =
    call (Dream.request ~method_:`POST ~target:delete_target ~headers:(form :: bob_headers) "csrf=bob-csrf")
  in
  Alcotest.(check int) "foreign delete redirects" 303 (Dream.status_to_int (Dream.status foreign_delete));
  Alcotest.(check bool) "attempt survives foreign delete" true
    (Option.is_some (Lwt_main.run (Db.get_attempt db ~user_id:alice_id attempt_id)));
  let owner_delete =
    call (Dream.request ~method_:`POST ~target:delete_target ~headers:(form :: alice_headers) "csrf=alice-csrf")
  in
  Alcotest.(check int) "owner delete redirects" 303 (Dream.status_to_int (Dream.status owner_delete));
  let deleted, modules_gone =
    Lwt_main.run
      (Db.get_attempt db ~user_id:alice_id attempt_id >>= fun attempt ->
       Db.get_attempt_modules db attempt_id >|= fun modules -> attempt, modules)
  in
  Alcotest.(check bool) "attempt deleted" true (Option.is_none deleted);
  Alcotest.(check int) "cascade removes modules" 0 (List.length modules_gone)

let () =
  Alcotest.run "SAT revision"
    [
      ("sanitization", [ Alcotest.test_case "malicious HTML, MathML and SVG" `Quick test_sanitize ]);
      ("upstream JSON", [ Alcotest.test_case "metadata" `Quick test_metadata_parsing; Alcotest.test_case "detail" `Quick test_detail_parsing ]);
      ("generator", [ Alcotest.test_case "routing and higher-route quota properties" `Quick test_generator_higher_route_mix; Alcotest.test_case "quota fallback" `Quick test_generator_fallback; Alcotest.test_case "Math student response preference" `Quick test_math_student_response_preference ]);
      ("grading", [ Alcotest.test_case "answer normalization" `Quick test_answers ]);
      ("scoring", [ Alcotest.test_case "all table boundaries" `Quick test_scoring ]);
      ("security", [ Alcotest.test_case "Argon2id" `Slow test_passwords ]);
      ("SQLite integration", [ Alcotest.test_case "transactions and isolation" `Quick database_test ]);
      ("HTTP integration", [ Alcotest.test_case "sessions, CSRF, ownership" `Quick application_security_test ]);
    ]
