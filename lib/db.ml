open Lwt.Infix
open Model

module Q = struct
  open Caqti_request.Infix
  open Caqti_type

  let pragma_foreign_keys = (unit ->. unit) "PRAGMA foreign_keys = ON"
  let pragma_busy_timeout = (unit ->! int) "PRAGMA busy_timeout = 5000"
  let pragma_wal = (unit ->! string) "PRAGMA journal_mode = WAL"

  let migrations =
    [
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, applied_at INTEGER NOT NULL)";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT COLLATE NOCASE NOT NULL UNIQUE, password_hash TEXT NOT NULL, created_at INTEGER NOT NULL)";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS sessions (token_hash TEXT PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, expires_at INTEGER NOT NULL, created_at INTEGER NOT NULL)";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS questions (external_id TEXT PRIMARY KEY, question_id TEXT NOT NULL, section TEXT NOT NULL, difficulty TEXT NOT NULL, domain_code TEXT NOT NULL, domain_name TEXT NOT NULL, skill_code TEXT NOT NULL, skill_name TEXT NOT NULL, item_type TEXT, source_updated_at INTEGER NOT NULL, synced_at INTEGER NOT NULL)";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS metadata_state (key TEXT PRIMARY KEY, value TEXT NOT NULL)";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS attempts (id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, status TEXT NOT NULL, selected_modules TEXT NOT NULL, created_at INTEGER NOT NULL, completed_at INTEGER)";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS attempt_modules (id INTEGER PRIMARY KEY AUTOINCREMENT, attempt_id INTEGER NOT NULL REFERENCES attempts(id) ON DELETE CASCADE, module_code TEXT NOT NULL, sequence INTEGER NOT NULL, duration_seconds INTEGER NOT NULL, status TEXT NOT NULL, relaxed_blueprint INTEGER NOT NULL DEFAULT 0, started_at INTEGER, deadline_at INTEGER, submitted_at INTEGER, UNIQUE(attempt_id, module_code))";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS attempt_questions (id INTEGER PRIMARY KEY AUTOINCREMENT, attempt_module_id INTEGER NOT NULL REFERENCES attempt_modules(id) ON DELETE CASCADE, external_id TEXT NOT NULL REFERENCES questions(external_id), position INTEGER NOT NULL, answer TEXT, flagged INTEGER NOT NULL DEFAULT 0, is_correct INTEGER, UNIQUE(attempt_module_id, position), UNIQUE(attempt_module_id, external_id))";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS user_question_progress (user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, external_id TEXT NOT NULL REFERENCES questions(external_id), status TEXT NOT NULL, attempts_count INTEGER NOT NULL DEFAULT 0, last_attempted_at INTEGER NOT NULL, PRIMARY KEY(user_id, external_id))";
      (unit ->. unit)
        "CREATE TABLE IF NOT EXISTS attempt_scores (attempt_id INTEGER NOT NULL REFERENCES attempts(id) ON DELETE CASCADE, section TEXT NOT NULL, correct_count INTEGER NOT NULL, total_count INTEGER NOT NULL, low_score INTEGER, high_score INTEGER, estimate INTEGER, PRIMARY KEY(attempt_id, section))";
      (unit ->. unit)
        "CREATE INDEX IF NOT EXISTS sessions_expiry_idx ON sessions(expires_at)";
      (unit ->. unit)
        "CREATE INDEX IF NOT EXISTS attempts_user_idx ON attempts(user_id, created_at DESC)";
      (unit ->. unit)
        "CREATE INDEX IF NOT EXISTS attempt_modules_attempt_idx ON attempt_modules(attempt_id, sequence)";
      (unit ->. unit)
        "CREATE INDEX IF NOT EXISTS attempt_questions_module_idx ON attempt_questions(attempt_module_id, position)";
      (unit ->. unit)
        "CREATE INDEX IF NOT EXISTS progress_eligible_idx ON user_question_progress(user_id, status, external_id)";
      (unit ->. unit)
        "CREATE INDEX IF NOT EXISTS questions_blueprint_idx ON questions(section, domain_code, difficulty)";
      (unit ->. unit)
        "INSERT OR IGNORE INTO schema_migrations(version, applied_at) VALUES (1, unixepoch())";
    ]

  let insert_user =
    (t3 string string int64 ->. unit)
      "INSERT INTO users(username, password_hash, created_at) VALUES (?, ?, ?)"

  let last_insert_id = (unit ->! int) "SELECT last_insert_rowid()"

  let user_auth =
    (string ->? t3 int string string)
      "SELECT id, username, password_hash FROM users WHERE username = ? COLLATE NOCASE"

  let insert_session =
    (t5 string int string int64 int64 ->. unit)
      "INSERT INTO sessions(token_hash, user_id, csrf_token, expires_at, created_at) VALUES (?, ?, ?, ?, ?)"

  let session_user =
    (t2 string int64 ->? t3 int string string)
      "SELECT u.id, u.username, s.csrf_token FROM sessions s JOIN users u ON u.id = s.user_id WHERE s.token_hash = ? AND s.expires_at > ?"

  let delete_session = (string ->. unit) "DELETE FROM sessions WHERE token_hash = ?"
  let delete_expired_sessions = (int64 ->. unit) "DELETE FROM sessions WHERE expires_at <= ?"

  let upsert_question =
    (t11 string string string string string string string string
       (option string) int64 int64
    ->. unit)
      "INSERT INTO questions(external_id, question_id, section, difficulty, domain_code, domain_name, skill_code, skill_name, item_type, source_updated_at, synced_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(external_id) DO UPDATE SET question_id=excluded.question_id, section=excluded.section, difficulty=excluded.difficulty, domain_code=excluded.domain_code, domain_name=excluded.domain_name, skill_code=excluded.skill_code, skill_name=excluded.skill_name, source_updated_at=excluded.source_updated_at, synced_at=excluded.synced_at"

  let metadata_synced_at =
    (unit ->? int64) "SELECT MAX(synced_at) FROM questions HAVING COUNT(*) > 0"

  let eligible_questions =
    (int ->* t11 string string string string string string string string
       (option string) int64 int64)
      "SELECT q.external_id, q.question_id, q.section, q.difficulty, q.domain_code, q.domain_name, q.skill_code, q.skill_name, q.item_type, q.source_updated_at, q.synced_at FROM questions q LEFT JOIN user_question_progress p ON p.external_id=q.external_id AND p.user_id=? WHERE p.status IS NULL OR p.status <> 'done'"

  let update_item_type =
    (t2 string string ->. unit)
      "UPDATE questions SET item_type=? WHERE external_id=?"

  let insert_attempt =
    (t4 int string string int64 ->. unit)
      "INSERT INTO attempts(user_id, status, selected_modules, created_at) VALUES (?, ?, ?, ?)"

  let insert_attempt_module =
    (t6 int string int int string int ->. unit)
      "INSERT INTO attempt_modules(attempt_id, module_code, sequence, duration_seconds, status, relaxed_blueprint) VALUES (?, ?, ?, ?, ?, ?)"

  let insert_attempt_question =
    (t3 int string int ->. unit)
      "INSERT INTO attempt_questions(attempt_module_id, external_id, position) VALUES (?, ?, ?)"

  let list_attempts =
    (int ->* t6 int string string int64 (option int64) int)
      "SELECT a.id, a.status, a.selected_modules, a.created_at, a.completed_at, COUNT(am.id) FROM attempts a LEFT JOIN attempt_modules am ON am.attempt_id=a.id WHERE a.user_id=? GROUP BY a.id ORDER BY a.created_at DESC"

  let owned_attempt =
    (t2 int int ->? t5 int string string int64 (option int64))
      "SELECT id, status, selected_modules, created_at, completed_at FROM attempts WHERE id=? AND user_id=?"

  let delete_attempt =
    (t2 int int ->. unit) "DELETE FROM attempts WHERE id=? AND user_id=?"

  let attempt_modules =
    (int ->* t10 int string int int string int (option int64) (option int64)
       (option int64) int)
      "SELECT id, module_code, sequence, duration_seconds, status, relaxed_blueprint, started_at, deadline_at, submitted_at, (SELECT COUNT(*) FROM attempt_questions aq WHERE aq.attempt_module_id=am.id) FROM attempt_modules am WHERE attempt_id=? ORDER BY sequence"

  let owned_module =
    (t3 int int int ->? t10 int int string int string int (option int64)
       (option int64) (option int64) int)
      "SELECT am.id, am.attempt_id, am.module_code, am.duration_seconds, am.status, am.relaxed_blueprint, am.started_at, am.deadline_at, am.submitted_at, (SELECT COUNT(*) FROM attempt_questions aq WHERE aq.attempt_module_id=am.id) FROM attempt_modules am JOIN attempts a ON a.id=am.attempt_id WHERE am.id=? AND am.attempt_id=? AND a.user_id=?"

  let start_module =
    (t3 int64 int64 int ->. unit)
      "UPDATE attempt_modules SET status='active', started_at=?, deadline_at=? WHERE id=? AND status='pending'"

  let question_for_test =
    (t3 int int int ->? t10 int string string int (option string) int string string string string)
      "SELECT aq.id, aq.external_id, q.question_id, aq.position, aq.answer, aq.flagged, q.domain_name, q.skill_name, q.difficulty, COALESCE(q.item_type, '') FROM attempt_questions aq JOIN questions q ON q.external_id=aq.external_id JOIN attempt_modules am ON am.id=aq.attempt_module_id JOIN attempts a ON a.id=am.attempt_id WHERE am.id=? AND aq.position=? AND a.user_id=?"

  let question_states =
    (t2 int int ->* t4 int int int (option string))
      "SELECT aq.position, aq.flagged, CASE WHEN aq.answer IS NULL OR TRIM(aq.answer)='' THEN 0 ELSE 1 END, aq.answer FROM attempt_questions aq JOIN attempt_modules am ON am.id=aq.attempt_module_id JOIN attempts a ON a.id=am.attempt_id WHERE am.id=? AND a.user_id=? ORDER BY aq.position"

  let save_answer =
    (t3 (option string) int int ->. unit)
      "UPDATE attempt_questions SET answer=? WHERE id=? AND attempt_module_id=?"

  let save_flag =
    (t3 int int int ->. unit)
      "UPDATE attempt_questions SET flagged=? WHERE id=? AND attempt_module_id=?"

  let lock_module =
    (t3 int64 int int ->. unit)
      "UPDATE attempt_modules SET status='grading_pending', submitted_at=? WHERE id=? AND attempt_id=? AND status IN ('active','pending','grading_pending')"

  let grading_questions =
    (int ->* t4 int string (option string) string)
      "SELECT aq.id, aq.external_id, aq.answer, q.section FROM attempt_questions aq JOIN questions q ON q.external_id=aq.external_id WHERE aq.attempt_module_id=? ORDER BY aq.position"

  let set_question_grade =
    (t2 int int ->. unit) "UPDATE attempt_questions SET is_correct=? WHERE id=?"

  let upsert_progress =
    (t4 int string string int64 ->. unit)
      "INSERT INTO user_question_progress(user_id, external_id, status, attempts_count, last_attempted_at) VALUES (?, ?, ?, 1, ?) ON CONFLICT(user_id, external_id) DO UPDATE SET status=CASE WHEN user_question_progress.status='done' OR excluded.status='done' THEN 'done' ELSE excluded.status END, attempts_count=user_question_progress.attempts_count+1, last_attempted_at=excluded.last_attempted_at"

  let mark_module_submitted =
    (t2 int64 int ->. unit)
      "UPDATE attempt_modules SET status='submitted', submitted_at=COALESCE(submitted_at, ?) WHERE id=?"

  let remaining_modules =
    (int ->! int)
      "SELECT COUNT(*) FROM attempt_modules WHERE attempt_id=? AND status <> 'submitted'"

  let complete_attempt =
    (t2 int64 int ->. unit)
      "UPDATE attempts SET status='completed', completed_at=? WHERE id=?"

  let mark_attempt_in_progress =
    (int ->. unit) "UPDATE attempts SET status='in_progress' WHERE id=?"

  let mark_attempt_pending =
    (int ->. unit) "UPDATE attempts SET status='grading_pending' WHERE id=?"

  let expired_or_pending_modules =
    (int64 ->* t11 int int string int string int (option int64) (option int64)
       (option int64) int int)
      "SELECT am.id, am.attempt_id, am.module_code, am.duration_seconds, am.status, am.relaxed_blueprint, am.started_at, am.deadline_at, am.submitted_at, (SELECT COUNT(*) FROM attempt_questions aq WHERE aq.attempt_module_id=am.id), a.user_id FROM attempt_modules am JOIN attempts a ON a.id=am.attempt_id WHERE (am.status='active' AND am.deadline_at <= ?) OR am.status='grading_pending' ORDER BY COALESCE(am.deadline_at, 0)"

  let module_section_counts =
    (int ->* t3 string int int)
      "SELECT q.section, SUM(CASE WHEN aq.is_correct=1 THEN 1 ELSE 0 END), COUNT(*) FROM attempt_questions aq JOIN questions q ON q.external_id=aq.external_id JOIN attempt_modules am ON am.id=aq.attempt_module_id WHERE am.attempt_id=? AND am.status='submitted' GROUP BY q.section"

  let upsert_score =
    (t7 int string int int (option int) (option int) (option int) ->. unit)
      "INSERT INTO attempt_scores(attempt_id, section, correct_count, total_count, low_score, high_score, estimate) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT(attempt_id, section) DO UPDATE SET correct_count=excluded.correct_count, total_count=excluded.total_count, low_score=excluded.low_score, high_score=excluded.high_score, estimate=excluded.estimate"

  let scores =
    (int ->* t6 string int int (option int) (option int) (option int))
      "SELECT section, correct_count, total_count, low_score, high_score, estimate FROM attempt_scores WHERE attempt_id=? ORDER BY section"

  let breakdown =
    (int ->* t5 string string string int int)
      "SELECT q.section, q.domain_name, q.difficulty, SUM(CASE WHEN aq.is_correct=1 THEN 1 ELSE 0 END), COUNT(*) FROM attempt_questions aq JOIN questions q ON q.external_id=aq.external_id JOIN attempt_modules am ON am.id=aq.attempt_module_id WHERE am.attempt_id=? AND am.status='submitted' GROUP BY q.section, q.domain_name, q.difficulty ORDER BY q.section, q.domain_name, q.difficulty"

  let review_questions =
    (t2 int int ->* t10 int string string int (option string) (option int) string string string string)
      "SELECT aq.id, aq.external_id, q.question_id, aq.position, aq.answer, aq.is_correct, q.domain_name, q.skill_name, q.difficulty, am.module_code FROM attempt_questions aq JOIN questions q ON q.external_id=aq.external_id JOIN attempt_modules am ON am.id=aq.attempt_module_id JOIN attempts a ON a.id=am.attempt_id WHERE a.id=? AND a.user_id=? AND am.status='submitted' AND (aq.is_correct=0 OR aq.is_correct IS NULL) ORDER BY am.sequence, aq.position"
end

type t = { connection : Caqti_lwt.connection; lock : Lwt_mutex.t }

type user = { id : int; username : string }
type session_user = { user : user; csrf_token : string }

type attempt_summary = {
  id : int;
  status : string;
  selected_modules : string;
  created_at : int64;
  completed_at : int64 option;
  module_count : int;
}

type attempt = {
  id : int;
  status : string;
  selected_modules : string;
  created_at : int64;
  completed_at : int64 option;
}

type attempt_module = {
  id : int;
  attempt_id : int;
  kind : module_kind;
  duration_seconds : int;
  status : string;
  relaxed_blueprint : bool;
  started_at : int64 option;
  deadline_at : int64 option;
  submitted_at : int64 option;
  question_count : int;
}

type assigned_question = {
  id : int;
  external_id : string;
  question_id : string;
  position : int;
  answer : string option;
  flagged : bool;
  domain_name : string;
  skill_name : string;
  difficulty : difficulty;
  item_type : item_type option;
}

type question_state = {
  position : int;
  flagged : bool;
  answered : bool;
  answer : string option;
}

type grading_question = {
  id : int;
  external_id : string;
  answer : string option;
  section : section;
}

type score_row = {
  section : section;
  correct : int;
  total : int;
  range : score_range option;
}

type breakdown_row = {
  section : section;
  domain : string;
  difficulty : difficulty;
  correct : int;
  total : int;
}

type review_question = {
  id : int;
  external_id : string;
  question_id : string;
  position : int;
  answer : string option;
  is_correct : bool option;
  domain : string;
  skill : string;
  difficulty : difficulty;
  module_kind : module_kind;
}

let now () = Unix.gettimeofday () |> Int64.of_float
let or_fail result = Caqti_lwt.or_fail result

let with_connection db f =
  Lwt_mutex.with_lock db.lock (fun () -> f db.connection >>= or_fail)

let exec_all (module Db : Caqti_lwt.CONNECTION) requests =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | request :: rest ->
        Db.exec request () >>= (function Ok () -> loop rest | Error _ as error -> Lwt.return error)
  in
  loop requests

let connect path =
  let absolute = if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path in
  let directory = Filename.dirname absolute in
  if not (Sys.file_exists directory) then Unix.mkdir directory 0o750;
  let uri = Uri.of_string ("sqlite3://" ^ absolute ^ "?create=true&write=true") in
  Caqti_lwt_unix.connect uri >>= function
  | Error error -> Lwt.fail (Caqti_error.Exn error)
  | Ok connection ->
      let db = { connection; lock = Lwt_mutex.create () } in
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.pragma_foreign_keys () >>= or_fail >>= fun () ->
      Db.find Q.pragma_busy_timeout () >>= or_fail >>= fun _ ->
      Db.find Q.pragma_wal () >>= or_fail >>= fun _ ->
      exec_all (module Db) Q.migrations >>= or_fail >|= fun () -> db

let create_user db ~username ~password_hash =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.with_transaction (fun () ->
          Db.exec Q.insert_user (username, password_hash, now ()) >>= function
          | Error _ as error -> Lwt.return error
          | Ok () -> Db.find Q.last_insert_id () >|= Result.map (fun id -> { id; username })))

let find_user_auth db username =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.find_opt Q.user_auth username)

let create_session db ~user_id ~token_hash ~csrf_token ~expires_at =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.insert_session (token_hash, user_id, csrf_token, expires_at, now ()))

let find_session db token_hash =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.find_opt Q.session_user (token_hash, now ()) >|= Result.map (Option.map (fun (id, username, csrf_token) -> { user = { id; username }; csrf_token })))

let delete_session db token_hash =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.delete_session token_hash)

let cleanup_sessions db =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.delete_expired_sessions (now ()))

let upsert_metadata db questions =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.with_transaction (fun () ->
          let rec loop = function
            | [] -> Lwt.return (Ok ())
            | (q : question_metadata) :: rest ->
                let params =
                  ( q.external_id,
                    q.question_id,
                    section_to_string q.section,
                    difficulty_to_string q.difficulty,
                    q.domain_code,
                    q.domain_name,
                    q.skill_code,
                    q.skill_name,
                    Option.map item_type_to_string q.item_type,
                    q.source_updated_at,
                    q.synced_at )
                in
                Db.exec Q.upsert_question params >>= function
                | Ok () -> loop rest
                | Error _ as error -> Lwt.return error
          in
          loop questions))

let metadata_synced_at db =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.find_opt Q.metadata_synced_at ())

let metadata_of_tuple
    (external_id, question_id, section, difficulty, domain_code, domain_name,
     skill_code, skill_name, item_type, source_updated_at, synced_at) =
  match section_of_string section, difficulty_of_string difficulty with
  | Some section, Some difficulty ->
      Some
        {
          external_id;
          question_id;
          section;
          difficulty;
          domain_code;
          domain_name;
          skill_code;
          skill_name;
          item_type = Option.bind item_type item_type_of_string;
          source_updated_at;
          synced_at;
        }
  | _ -> None

let eligible_questions db user_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.eligible_questions user_id >|= Result.map (List.filter_map metadata_of_tuple))

let update_item_type db external_id item_type =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.update_item_type (item_type_to_string item_type, external_id))

let create_attempt db ~user_id (assignments : module_assignment list) =
  let modules = List.map (fun (assignment : module_assignment) -> module_to_string assignment.kind) assignments in
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.with_transaction (fun () ->
          Db.exec Q.insert_attempt (user_id, "in_progress", String.concat "," modules, now ()) >>= function
          | Error _ as error -> Lwt.return error
          | Ok () ->
              Db.find Q.last_insert_id () >>= function
              | Error _ as error -> Lwt.return error
              | Ok attempt_id ->
                  let rec add_modules sequence = function
                    | [] -> Lwt.return (Ok attempt_id)
                    | (assignment : module_assignment) :: rest ->
                        Db.exec Q.insert_attempt_module
                          ( attempt_id,
                            module_to_string assignment.kind,
                            sequence,
                            module_duration_seconds assignment.kind,
                            "pending",
                            if assignment.relaxed_blueprint then 1 else 0 )
                        >>= function
                        | Error _ as error -> Lwt.return error
                        | Ok () ->
                            Db.find Q.last_insert_id () >>= function
                            | Error _ as error -> Lwt.return error
                            | Ok module_id ->
                                let rec add_questions position = function
                                  | [] -> add_modules (sequence + 1) rest
                                  | (question : question_metadata) :: questions ->
                                      Db.exec Q.insert_attempt_question
                                        (module_id, question.external_id, position)
                                      >>= function
                                      | Ok () -> add_questions (position + 1) questions
                                      | Error _ as error -> Lwt.return error
                                in
                                add_questions 1 assignment.questions
                  in
                  add_modules 1 assignments))

let list_attempts db user_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.list_attempts user_id
      >|= Result.map (List.map (fun (id, status, selected_modules, created_at, completed_at, module_count) ->
              { id; status; selected_modules; created_at; completed_at; module_count })))

let get_attempt db ~user_id attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.find_opt Q.owned_attempt (attempt_id, user_id)
      >|= Result.map (Option.map (fun (id, status, selected_modules, created_at, completed_at) ->
              { id; status; selected_modules; created_at; completed_at })))

let delete_attempt db ~user_id attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.delete_attempt (attempt_id, user_id))

let module_of_row attempt_id
    (id, module_code, _sequence, duration_seconds, status, relaxed, started_at,
     deadline_at, submitted_at, question_count) =
  Option.map
    (fun kind ->
      { id; attempt_id; kind; duration_seconds; status; relaxed_blueprint = relaxed <> 0;
        started_at; deadline_at; submitted_at; question_count })
    (module_of_string module_code)

let get_attempt_modules db attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.attempt_modules attempt_id
      >|= Result.map (List.filter_map (module_of_row attempt_id)))

let get_module db ~user_id ~attempt_id module_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.find_opt Q.owned_module (module_id, attempt_id, user_id)
      >|= Result.map (fun row -> Option.bind row (fun (id, attempt_id, module_code, duration_seconds, status,
                                                       relaxed, started_at, deadline_at, submitted_at, question_count) ->
              Option.map (fun kind ->
                  { id; attempt_id; kind; duration_seconds; status; relaxed_blueprint=relaxed<>0;
                    started_at; deadline_at; submitted_at; question_count })
                (module_of_string module_code))))

let start_module db module_ =
  match module_.status, module_.started_at, module_.deadline_at with
  | "pending", _, _ ->
      let started_at = now () in
      let deadline_at = Int64.add started_at (Int64.of_int module_.duration_seconds) in
      with_connection db (fun connection ->
          let (module Db : Caqti_lwt.CONNECTION) = connection in
          Db.exec Q.start_module (started_at, deadline_at, module_.id))
      >|= fun () -> { module_ with status="active"; started_at=Some started_at; deadline_at=Some deadline_at }
  | _ -> Lwt.return module_

let assigned_of_tuple (id, external_id, question_id, position, answer, flagged,
                       domain_name, skill_name, difficulty, item_type) =
  match difficulty_of_string difficulty with
  | None -> None
  | Some difficulty ->
      Some { id; external_id; question_id; position; answer; flagged=flagged<>0;
             domain_name; skill_name; difficulty;
             item_type=item_type_of_string item_type }

let get_question db ~user_id ~module_id position =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.find_opt Q.question_for_test (module_id, position, user_id)
      >|= Result.map (fun row -> Option.bind row assigned_of_tuple))

let question_states db ~user_id ~module_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.question_states (module_id, user_id)
      >|= Result.map (List.map (fun (position, flagged, answered, answer) ->
              { position; flagged=flagged<>0; answered=answered<>0; answer })))

let save_answer db ~module_id ~question_id answer =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.save_answer (answer, question_id, module_id))

let save_flag db ~module_id ~question_id flagged =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.exec Q.save_flag ((if flagged then 1 else 0), question_id, module_id))

let lock_module_for_grading db (module_ : attempt_module) =
  let submitted_at = now () in
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.with_transaction (fun () ->
          Db.exec Q.lock_module (submitted_at, module_.id, module_.attempt_id) >>= function
          | Error _ as error -> Lwt.return error
          | Ok () -> Db.exec Q.mark_attempt_pending module_.attempt_id))

let grading_questions db module_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.grading_questions module_id
      >|= Result.map (List.filter_map (fun (id, external_id, answer, section) ->
              Option.map (fun section -> { id; external_id; answer; section })
                (section_of_string section))))

let rec exec_grades (module Db : Caqti_lwt.CONNECTION) user_id timestamp = function
  | [] -> Lwt.return (Ok ())
  | ((question : grading_question), correct) :: rest ->
      Db.exec Q.set_question_grade ((if correct then 1 else 0), question.id) >>= function
      | Error _ as error -> Lwt.return error
      | Ok () ->
          let status =
            if correct then "done"
            else match question.answer with
              | Some value when String.trim value <> "" -> "incorrect"
              | _ -> "skipped"
          in
          Db.exec Q.upsert_progress (user_id, question.external_id, status, timestamp) >>= function
          | Error _ as error -> Lwt.return error
          | Ok () -> exec_grades (module Db) user_id timestamp rest

let apply_grades db ~user_id (module_ : attempt_module) grades =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.with_transaction (fun () ->
          let timestamp = now () in
          exec_grades (module Db) user_id timestamp grades >>= function
          | Error _ as error -> Lwt.return error
          | Ok () ->
              Db.exec Q.mark_module_submitted (timestamp, module_.id) >>= function
              | Error _ as error -> Lwt.return error
              | Ok () ->
                  Db.find Q.remaining_modules module_.attempt_id >>= function
                  | Error _ as error -> Lwt.return error
                  | Ok 0 -> Db.exec Q.complete_attempt (timestamp, module_.attempt_id)
                  | Ok _ -> Db.exec Q.mark_attempt_in_progress module_.attempt_id))

let expired_or_pending_modules db =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.expired_or_pending_modules (now ())
      >|= Result.map (List.filter_map (fun (id, attempt_id, module_code, duration_seconds,
                                           status, relaxed, started_at, deadline_at,
                                           submitted_at, question_count, user_id) ->
              Option.map
                (fun kind ->
                  ( user_id,
                    { id; attempt_id; kind; duration_seconds; status;
                      relaxed_blueprint=relaxed<>0; started_at; deadline_at;
                      submitted_at; question_count } ))
                (module_of_string module_code))))

let section_counts db attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.module_section_counts attempt_id
      >|= Result.map (List.filter_map (fun (section, correct, total) ->
              Option.map (fun section -> (section, correct, total)) (section_of_string section))))

let save_scores db attempt_id rows =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.with_transaction (fun () ->
          let rec loop = function
            | [] -> Lwt.return (Ok ())
            | (section, correct, total, range) :: rest ->
                let low, high, estimate = match range with
                  | None -> None, None, None
                  | Some range -> Some range.low, Some range.high, Some range.estimate
                in
                Db.exec Q.upsert_score
                  (attempt_id, section_to_string section, correct, total, low, high, estimate)
                >>= function Ok () -> loop rest | Error _ as error -> Lwt.return error
          in loop rows))

let scores db attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.scores attempt_id
      >|= Result.map (List.filter_map (fun (section, correct, total, low, high, estimate) ->
              match section_of_string section with
              | None -> None
              | Some section ->
                  let range = match low, high, estimate with
                    | Some low, Some high, Some estimate -> Some { low; high; estimate }
                    | _ -> None
                  in Some { section; correct; total; range })))

let breakdown db attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.breakdown attempt_id
      >|= Result.map (List.filter_map (fun (section, domain, difficulty, correct, total) ->
              match section_of_string section, difficulty_of_string difficulty with
              | Some section, Some difficulty -> Some { section; domain; difficulty; correct; total }
              | _ -> None)))

let review_questions db ~user_id attempt_id =
  with_connection db (fun connection ->
      let (module Db : Caqti_lwt.CONNECTION) = connection in
      Db.collect_list Q.review_questions (attempt_id, user_id)
      >|= Result.map (List.filter_map (fun (id, external_id, question_id,
                                           position, answer, is_correct, domain,
                                           skill, difficulty, module_code) ->
              match difficulty_of_string difficulty, module_of_string module_code with
              | Some difficulty, Some module_kind ->
                  Some { id; external_id; question_id; position; answer;
                         is_correct=Option.map ((<>) 0) is_correct; domain; skill;
                         difficulty; module_kind }
              | _ -> None)))
