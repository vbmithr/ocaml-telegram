open Core.Std
open Async.Std
open Log.Global

open Telegram
open Telegram_async

let buf = Bi_outbuf.create 4096

let process_cmd ~message_id chat = function
| "start" -> Deferred.unit
| "help" -> Deferred.unit
| cmd ->
    let message_id = match chat.Chat.kind with
    | Chat.Private -> None
    | _ -> None in
    let text = "No such command " ^ cmd in
    sendMessage
      ~log:(Lazy.force log)
      ~buf
      ~chat_id:chat.Chat.id
      ?reply_to_message_id:message_id
      ~text () >>= fun _ ->
    Deferred.unit

let main () =
  set_level `Debug ;
  Sys.home_directory () >>= fun homedir ->
  Reader.file_lines Filename.(concat homedir ".telegram") >>= fun lines ->
  init ~user_agent:"ocaml-telegram-testbot" ~token:(List.hd_exn lines) ;
  getMe () >>= function
  | Error _ -> Deferred.unit
  | Ok { id; first; last; username } ->
      printf "I'm %s (%d)" first id ;
      let updates = updates ~log:(Lazy.force log)
          ~timeout:Time_ns.Span.(of_int_sec 300) () in
      Pipe.iter updates ~f:begin fun ({ id ; update } as update_msg) ->
        begin match update with
        | Message { id ; chat ; text ; entities } ->
            let entities = Entity.extract ~text entities in
            Deferred.List.iter ~how:`Sequential entities ~f:begin function
            | Bot_command, cmd ->
                process_cmd ~message_id:id chat cmd
            | _ -> Deferred.unit
            end
        | _ -> Deferred.unit
        end >>= fun () ->
        let json = Json.construct Update.encoding update_msg in
        debug "%s" @@ Yojson.Safe.to_string json ;
        Deferred.unit
      end

let () =
  don't_wait_for @@ main () ;
  never_returns @@ Scheduler.go ()
