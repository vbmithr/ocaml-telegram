open Core
open Async
open Cohttp_async
module Json = Json_encoding.Make(Json_repr.Yojson)

open Telegram

let mk_uri ~token ~meth_name =
  Uri.of_string @@
  "https://api.telegram.org/bot" ^ token ^ "/" ^ meth_name

type meth =
  | GetMe
  | GetUpdates
  | SendMessage

type uris = {
  getMe : Uri.t ;
  getUpdates : Uri.t ;
  sendMessage : Uri.t ;
}

let agent = ref "ocaml-telegram"
let uris = ref None

let uri_of_meth meth = match !uris, meth with
| None, _ -> invalid_arg "token not set"
| Some uris, GetMe -> uris.getMe
| Some uris, GetUpdates -> uris.getUpdates
| Some uris, SendMessage -> uris.sendMessage

let init ~user_agent ~token =
  agent := user_agent ;
  uris := Some {
    getMe = mk_uri ~token ~meth_name:"getMe" ;
    getUpdates = mk_uri ~token ~meth_name:"getUpdates" ;
    sendMessage = mk_uri ~token ~meth_name:"sendMessage" ;
  }

type rpc_error = {
  code : int ;
  message : string ;
}

let result_encoding encoding =
  let open Json_encoding in
  conv
    (function
    | Ok result -> (true, "", 0, result)
    | Error { code ; message } -> (false, message, code, None))
    (fun (ok, message, code, result) ->
       if ok then Ok result else Error { code ; message })
    (obj4
       (req "ok" bool)
       (dft "description" string "")
       (dft "error_code" int 0)
       (opt "result" encoding))

let headers =
  Cohttp.Header.of_list [
    "user-agent", !agent ;
    "content-type", "application/json" ;
  ]

let getMe () =
  Client.get (uri_of_meth GetMe) >>= fun (resp, body) ->
  Body.to_string body >>| fun body_str ->
  let body_json = Yojson.Safe.from_string body_str in
  match Json.destruct (result_encoding User.encoding) body_json with
  | Ok (Some user) -> Ok user
  | Ok None -> invalid_arg "Telegram API"
  | Error err -> Error err

let sendMessage ?log ?(buf=Bi_outbuf.create 4096)
  ?parse_mode ?disable_web_page_preview
  ?disable_notification ?reply_to_message_id
  ?reply_markup ~chat_id ~text () =
  let post_data =
    Message.Send.create
      ?parse_mode ?disable_web_page_preview
      ?disable_notification ?reply_to_message_id
      ?reply_markup ~chat_id ~text () in
  let post_data = Json.construct Message.Send.encoding post_data in
  let body_str = Yojson.Safe.to_string ~buf post_data in
  let body = Body.of_string body_str in
  Option.iter log ~f:(fun log -> Log.debug log "POST: %s" body_str) ;
  Client.post ~headers ~body (uri_of_meth SendMessage) >>= fun (resp, body) ->
  Body.to_string body >>| fun body_str ->
  let body_json = Yojson.Safe.from_string body_str in
  match Json.destruct (result_encoding Message.encoding) body_json with
  | Ok (Some message) -> Ok message
  | Ok None -> invalid_arg "Telegram API"
  | Error err -> Error err

let getUpdates ?log ?(buf=Bi_outbuf.create 4096) ?offset ?limit ?timeout ?kinds () =
  let timeout = Option.map timeout ~f:Time_ns.Span.to_int_sec in
  let post_data = Update.Get.create ?offset ?limit ?timeout ?kinds () in
  let post_data = Json.construct Update.Get.encoding post_data in
  let body_str = Yojson.Safe.to_string ~buf post_data in
  let body = Body.of_string body_str in
  Option.iter log ~f:(fun log -> Log.debug log "POST: %s" body_str) ;
  Client.post ~headers ~body (uri_of_meth GetUpdates) >>| fun (resp, body) ->
  Pipe.map' (Body.to_pipe body) ~f:begin fun msgs ->
    let updates = Queue.create () in
    Queue.iter msgs ~f:begin fun msg ->
      try
        match Json.destruct (result_encoding (Json_encoding.list Update.encoding))
                (Yojson.Safe.from_string ~buf msg) with
        | Ok (Some ups) -> Queue.enqueue_all updates ups
        | _ -> ()
      with Json_encoding.Cannot_destruct _ ->
        Option.iter log ~f:(fun log -> Log.error log "Destruct error: %s" msg)
    end ;
    Deferred.return updates
  end

let updates ?log ?(interrupt=Deferred.never ()) ?timeout () =
  let updates_r, updates_w = Pipe.create () in
  let rec inner offset =
    getUpdates ?log ~offset ?timeout () >>= fun updates ->
    Pipe.fold updates ~init:offset ~f:begin fun _off update ->
      Pipe.write updates_w update >>| fun () ->
      update.id
    end >>= fun latest_update_id ->
    Deferred.any_unit [
      interrupt ;
      inner (if latest_update_id = 0 then 0 else succ latest_update_id)
    ] in
  don't_wait_for @@ Monitor.protect
    (fun () -> inner 0)
    ~finally:(fun () -> Pipe.close updates_w; Deferred.unit) ;
  updates_r
