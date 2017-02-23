open Core.Std
open Async.Std
open Log.Global

open Telegram
open Telegram_async

let main () =
  set_level `Debug ;
  Sys.home_directory () >>= fun homedir ->
  Reader.file_lines Filename.(concat homedir ".telegram") >>= fun lines ->
  set_token @@ List.hd_exn lines ;
  getMe () >>= fun { id; first; last; username } ->
  printf "I'm %s (%d)" first id ;
  let updates = updates ~log:(Lazy.force log)
      ~timeout:Time_ns.Span.(of_int_sec 300) () in
  Pipe.iter updates ~f:begin fun update ->
    let json = Json.construct Update.encoding update in
    printf "%s" @@ Yojson.Safe.to_string json ;
    Deferred.unit
  end

let () =
  don't_wait_for @@ main () ;
  never_returns @@ Scheduler.go ()
