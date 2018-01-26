(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Telegram Bot API

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Telegram} *)

module User : sig
  type t = {
    id : int ;
    first : string ;
    last : string ;
    username : string ;
    is_bot : bool ;
  }

  val encoding : t Json_encoding.encoding
end

module Chat : sig
  type kind = Private | Group | Supergroup | Channel

  val kind_encoding : kind Json_encoding.encoding

  type t = {
    id : int ;
    kind : kind ;
    title : string ;
    username : string ;
    first : string ;
    last : string ;
    all_admin : bool ;
  }

  val encoding : t Json_encoding.encoding
end

module Entity : sig
  type kind =
    | Mention
    | Hashtag
    | Bot_command
    | Url
    | Email
    | Bold
    | Italic
    | Code
    | Pre
    | Text_link of Uri.t
    | Text_mention of User.t

  type t = {
    kind : kind ;
    offset : int ;
    length : int ;
  }

  val extract : text:string -> t list -> (kind * string) list

  val encoding : t Json_encoding.encoding
end

module Audio : sig
  type t = {
    id : string ;
    duration : int ;
    performer : string ;
    title : string ;
    mime : string ;
    size : int ;
  }

  val encoding : t Json_encoding.encoding
end

module PhotoSize : sig
  type t = {
    id : string ;
    width : int ;
    height : int ;
    size : int ;
  }

  val encoding : t Json_encoding.encoding
end

module Document : sig
  type t = {
    id : string ;
    thumb : PhotoSize.t option ;
    filename : string ;
    mime : string ;
    size : int ;
  }

  val encoding : t Json_encoding.encoding
end

module Contact : sig
  type t = {
    phone : string ;
    first : string ;
    last : string ;
    id : int ;
  }

  val encoding : t Json_encoding.encoding
end

module Location : sig
  type t = {
    longitude : float ;
    latitude : float ;
  }

  val encoding : t Json_encoding.encoding
end

module Venue : sig
  type t = {
    location : Location.t ;
    title : string ;
    address : string ;
    foursquare : string ;
  }

  val encoding : t Json_encoding.encoding
end

module Message : sig
  type t = {
    id : int ;
    from : User.t option ;
    date : Ptime.t ;
    chat : Chat.t ;
    fw_from : User.t option ;
    fw_from_chat : Chat.t option ;
    fw_from_msg_id : int ;
    fw_date : Ptime.t ;
    reply_to_msg : Json_repr.ezjsonm option ;
    edit_date : Ptime.t ;
    text : string ;
    entities : Entity.t list ;
    audio : Audio.t option ;
    document : Document.t option ;
    game : Json_repr.ezjsonm option ; (* FIXME *)
    photo : PhotoSize.t list ;
    sticker : Json_repr.ezjsonm option ; (* FIXME *)
    video : Json_repr.ezjsonm option ; (* FIXME *)
    voice : Json_repr.ezjsonm option ; (* FIXME *)
    caption : string ;
    contact : Contact.t option ;
    location : Location.t option ;
    venue : Venue.t option ;
    new_chat_member : User.t option ;
    left_chat_member : User.t option ;
    new_chat_title : string ;
    new_chat_photo : PhotoSize.t list ;
    delete_chat_photo : bool ;
    group_chat_created : bool ;
    supergroup_chat_created : bool ;
    channel_chat_created : bool ;
    migrate_to_chat_id : int ;
    migrate_from_chat_id : int ;
    pinned_msg : Json_repr.ezjsonm option ;
  }

  val encoding : t Json_encoding.encoding

  module Send : sig
    type parse_mode = Markdown | Html

    type t = {
      chat_id : int ;
      text : string ;
      parse_mode : parse_mode option ;
      disable_web_page_preview : bool ;
      disable_notification : bool ;
      reply_to_message_id : int ;
      reply_markup : Json_repr.ezjsonm option ;
    }

    val create :
      ?parse_mode:parse_mode ->
      ?disable_web_page_preview:bool ->
      ?disable_notification:bool ->
      ?reply_to_message_id:int ->
      ?reply_markup:Json_repr.ezjsonm ->
      chat_id:int ->
      text:string -> unit -> t

    val encoding : t Json_encoding.encoding
  end
end

module InlineQuery : sig
  type t = {
    id : string ;
    from : User.t ;
    location : Location.t option ;
    query : string ;
    offset : string ;
  }

  val encoding : t Json_encoding.encoding
end

module ChosenInlineResult : sig
  type t = {
    id : string ;
    from : User.t ;
    location : Location.t option ;
    inline_msg_id : string ;
    query : string ;
  }

  val encoding : t Json_encoding.encoding
end

module Update : sig
  module Get : sig
    type kind =
      | Message
      | Edited_message
      | Channel_post
      | Edited_channel_post
      | Inline_query
      | Chosen_inline_result
      | Callback_query

    val kind_encoding : kind Json_encoding.encoding

    type t = {
      offset : int option ;
      limit : int ;
      timeout : int ;
      kinds : kind list ;
    }

    val encoding : t Json_encoding.encoding
    val create :
      ?offset:int -> ?limit:int ->
      ?timeout:int -> ?kinds:kind list -> unit -> t
  end

  type update =
    | Message of Message.t
    | Edited_message of Message.t
    | Channel_post of Message.t
    | Edited_channel_post of Message.t
    | Inline_query of InlineQuery.t
    | Chosen_inline_result of ChosenInlineResult.t
    | Callback_query of Json_repr.ezjsonm

  type t = {
    id : int ;
    update : update ;
  }

  val encoding : t Json_encoding.encoding
end


(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
