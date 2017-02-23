open Core.Std
open Async.Std
open Telegram

module Json : sig
  val construct : 't Json_encoding.encoding -> 't -> Json_repr.Yojson.value
  val destruct : 't Json_encoding.encoding -> Json_repr.Yojson.value -> 't
  val custom :
    ('t -> Json_repr.Yojson.value) ->
    (Json_repr.Yojson.value -> 't) ->
    schema:Json_schema.schema -> 't Json_encoding.encoding
end

exception TelegramError of int * string

val set_token : string -> unit
val getMe : unit -> User.t Deferred.t
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
