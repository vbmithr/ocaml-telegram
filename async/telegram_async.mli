open Core
open Async
open Telegram

module Json : sig
  val construct : 't Json_encoding.encoding -> 't -> Json_repr.Yojson.value
  val destruct : 't Json_encoding.encoding -> Json_repr.Yojson.value -> 't
  val custom :
    ('t -> Json_repr.Yojson.value) ->
    (Json_repr.Yojson.value -> 't) ->
    schema:Json_schema.schema -> 't Json_encoding.encoding
end

type rpc_error = {
  code : int ;
  message : string ;
}

val init : user_agent:string -> token:string -> unit

val getMe : unit -> (User.t, rpc_error) Result.t Deferred.t

val sendMessage :
  ?log:Log.t ->
  ?buf:Bi_outbuf.t ->
  ?parse_mode:Message.Send.parse_mode ->
  ?disable_web_page_preview:bool ->
  ?disable_notification:bool ->
  ?reply_to_message_id:int ->
  ?reply_markup:Json_repr.ezjsonm ->
  chat_id:int ->
  text:string -> unit -> (Message.t, rpc_error) Result.t Deferred.t

val getUpdates :
  ?log:Log.t ->
  ?buf:Bi_outbuf.t ->
  ?offset:int ->
  ?limit:int ->
  ?timeout:Time_ns.Span.t ->
  ?kinds:Update.Get.kind list -> unit -> Update.t Pipe.Reader.t Deferred.t

val updates :
  ?log:Log.t ->
  ?interrupt:unit Deferred.t ->
  ?timeout:Time_ns.Span.t ->
  unit ->
  Update.t Pipe.Reader.t
