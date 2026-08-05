let env name fallback = Sys.getenv_opt name |> Option.value ~default:fallback

let () =
  let database_path = env "SAT_DB_PATH" "./var/sat.db" in
  let interface = env "SAT_HOST" "127.0.0.1" in
  let port = env "SAT_PORT" "8080" |> int_of_string_opt |> Option.value ~default:8080 in
  let db = Lwt_main.run (Sat.Db.connect database_path) in
  Lwt_main.run (Sat.Db.cleanup_sessions db);
  (match Lwt_main.run (Sat.App.refresh_metadata db) with
   | Ok `Fresh -> Printf.printf "Question metadata cache is current.\n%!"
   | Ok (`Updated count) -> Printf.printf "Refreshed %d question metadata records.\n%!" count
   | Error message ->
       Printf.eprintf "Question metadata refresh failed; using stale cache if available: %s\n%!" message);
  Lwt.async (fun () -> Sat.App.expiry_worker db);
  Printf.printf "SAT Revision is available at http://%s:%d\n%!" interface port;
  Dream.run ~interface ~port ~greeting:false (Sat.App.handler db)
