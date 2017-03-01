(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Option = struct
  let value_exn ?default ?msg opt =
    match default, opt with
    | _, Some s -> s
    | Some dft, None -> dft
    | _ -> begin match msg with
      | None -> failwith "Option.value_exn"
      | Some msg -> failwith msg
      end

  let map v ~f = match v with
  | None -> None
  | Some v -> Some (f v)
end

let uri_encoding =
  let open Json_encoding in
  conv Uri.to_string Uri.of_string string

let int63_encoding =
  let open Json_encoding in
  ranged_int ~maximum:max_int ~minimum:min_int "int63"

let ptime_encoding =
  let open Json_encoding in
  let open Ptime in
  conv
    (fun pt -> to_span pt |>
               Span.to_int_s |>
               Option.value_exn ?default:None ~msg:"ptime_encoding")
    (fun t -> Span.of_int_s t |> of_span |>
              Option.value_exn ?default:None ~msg:"ptime_encoding")
    int63_encoding

module User = struct
  type t = {
    id : int ;
    first : string ;
    last : string ;
    username : string ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; first ; last ; username } ->
         (id, first, last, username))
      (fun (id, first, last, username) ->
         { id ; first ; last ; username })
      (obj4
         (req "id" int)
         (req "first_name" string)
         (dft "last_name" string "")
         (dft "username" string ""))
end

module Chat = struct
  type kind = Private | Group | Supergroup | Channel

  let kind_encoding =
    let open Json_encoding in
    string_enum ["private", Private ; "group", Group ;
                 "supergroup", Supergroup ; "channel", Channel]

  type t = {
    id : int ;
    kind : kind ;
    title : string ;
    username : string ;
    first : string ;
    last : string ;
    all_admin : bool ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; kind ; title ; username ; first ; last ; all_admin } ->
         (id, kind, title, username, first, last, all_admin))
      (fun (id, kind, title, username, first, last, all_admin) ->
         { id ; kind ; title ; username ; first ; last ; all_admin })
      (obj7
         (req "id" int63_encoding)
         (req "type" kind_encoding)
         (dft "title" string "")
         (dft "username" string "")
         (dft "first_name" string "")
         (dft "last_name" string "")
         (dft "all_members_are_administrators" bool false))
end

module Entity = struct
  module Kind = struct
    type t =
      | Mention
      | Hashtag
      | Bot_command
      | Url
      | Email
      | Bold
      | Italic
      | Code
      | Pre
      | Text_link
      | Text_mention

    let encoding =
      let open Json_encoding in
      string_enum [
        "mention", Mention ;
        "hashtag", Hashtag ;
        "bot_command", Bot_command ;
        "url", Url ;
        "email", Email ;
        "bold", Bold ;
        "italic", Italic ;
        "code", Code ;
        "pre", Pre ;
        "text_link", Text_link ;
        "text_mention", Text_mention ;
      ]
  end

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

  let enum_kind_of_kind = function
  | Mention -> Kind.Mention
  | Hashtag -> Hashtag
  | Bot_command -> Bot_command
  | Url -> Url
  | Email -> Email
  | Bold -> Bold
  | Italic -> Italic
  | Code -> Code
  | Pre -> Pre
  | Text_link _ -> Text_link
  | Text_mention _ -> Text_mention

  let kind_of_enum_kind = function
  | Kind.Mention -> Mention
  | Hashtag -> Hashtag
  | Bot_command -> Bot_command
  | Url -> Url
  | Email -> Email
  | Bold -> Bold
  | Italic -> Italic
  | Code -> Code
  | Pre -> Pre
  | _ -> invalid_arg "kind_of_enum_kind"

  let text_link uri = Text_link uri
  let text_mention user = Text_mention user

  type t = {
    kind : kind ;
    offset : int ;
    length : int ;
  }

  let extract ~text entities =
    ListLabels.map entities ~f:begin fun { kind ; offset ; length } ->
      kind, String.sub text offset length
    end

  let encoding =
    let open Json_encoding in
    conv
      (fun { kind ; offset ; length } ->
         let url, user = match kind with
         | Text_link url -> Some url, None
         | Text_mention user -> None, Some user
         | _ -> None, None in
         let enum_kind = enum_kind_of_kind kind in
         (enum_kind, offset, length, url, user))
      (fun (enum_kind, offset, length, url, user) ->
         let kind = match enum_kind with
         | Text_link -> text_link (Option.value_exn url)
         | Text_mention -> text_mention (Option.value_exn user)
         | enum_kind -> kind_of_enum_kind enum_kind in
         { kind ; offset ; length })
      (obj5
         (req "type" Kind.encoding)
         (req "offset" int)
         (req "length" int)
         (opt "url" uri_encoding)
         (opt "user" User.encoding))
end

module Audio = struct
  type t = {
    id : string ;
    duration : int ;
    performer : string ;
    title : string ;
    mime : string ;
    size : int ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; duration ; performer ; title ; mime ; size } ->
         (id, duration, performer, title, mime, size))
      (fun (id, duration, performer, title, mime, size) ->
         { id ; duration ; performer ; title ; mime ; size })
      (obj6
         (req "file_id" string)
         (req "duration" int)
         (dft "performer" string "")
         (dft "title" string "")
         (dft "mime_type" string "")
         (dft "file_size" int 0)
      )
end

module PhotoSize = struct
  type t = {
    id : string ;
    width : int ;
    height : int ;
    size : int ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; width ; height ; size } ->
         (id, width, height, size))
      (fun (id, width, height, size) ->
         { id ; width ; height ; size })
      (obj4
         (req "file_id" string)
         (req "width" int)
         (req "height" int)
         (dft "file_size" int 0)
      )
end

module Document = struct
  type t = {
    id : string ;
    thumb : PhotoSize.t option ;
    filename : string ;
    mime : string ;
    size : int ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; thumb ; filename ; mime ; size } ->
         (id, thumb, filename, mime, size))
      (fun (id, thumb, filename, mime, size) ->
         { id ; thumb ; filename ; mime ; size })
      (obj5
         (req "file_id" string)
         (opt "thumb" PhotoSize.encoding)
         (dft "file_name" string "")
         (dft "mime_type" string "")
         (dft "file_size" int 0))
end

module Location = struct
  type t = {
    longitude : float ;
    latitude : float ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { longitude ; latitude } ->
         (longitude, latitude))
      (fun (longitude, latitude) ->
      { longitude ; latitude })
      (obj2
         (req "longitude" float)
         (req "latitude" float))
end

module Contact = struct
  type t = {
    phone : string ;
    first : string ;
    last : string ;
    id : int ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { phone ; first ; last ; id } ->
         (phone, first, last, id))
      (fun (phone, first, last, id) ->
      { phone ; first ; last ; id })
      (obj4
         (req "phone_number" string)
         (req "first_name" string)
         (dft "last_name" string "")
         (dft "user_id" int 0))
end

module Venue = struct
  type t = {
    location : Location.t ;
    title : string ;
    address : string ;
    foursquare : string ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { location ; title ; address ; foursquare } ->
      (location, title, address, foursquare))
      (fun (location, title, address, foursquare) ->
      { location ; title ; address ; foursquare })
      (obj4
         (req "location" Location.encoding)
         (req "title" string)
         (req "address" string)
         (dft "foursquare_id" string ""))
end

module Message = struct
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

  let rec encoding =
    let open Json_encoding in
    let header_obj =
      (obj10
         (req "message_id" int)
         (opt "from" User.encoding)
         (req "date" ptime_encoding)
         (req "chat" Chat.encoding)
         (opt "forward_from" User.encoding)
         (opt "forward_from_chat" Chat.encoding)
         (dft "forward_from_message_id" int 0)
         (dft "forward_date" ptime_encoding Ptime.epoch)
         (opt "reply_to_message" any_ezjson_value)
         (dft "edit_date" ptime_encoding Ptime.epoch)) in
    let content_obj1 =
      (obj10
         (dft "text" string "")
         (dft "entities" (list Entity.encoding) [])
         (opt "audio" Audio.encoding)
         (opt "document" Document.encoding)
         (opt "game" any_ezjson_value) (* FIXME *)
         (dft "photo" (list PhotoSize.encoding) [])
         (opt "sticker" any_ezjson_value) (* FIXME *)
         (opt "video" any_ezjson_value) (* FIXME *)
         (opt "voice" any_ezjson_value) (* FIXME *)
         (dft "caption" string "")) in
    let content_obj2 =
      (obj3
         (opt "contact" Contact.encoding)
         (opt "location" Location.encoding)
         (opt "venue" Venue.encoding)) in
    let chat_obj =
      (obj10
         (opt "new_chat_member" User.encoding)
         (opt "left_chat_member" User.encoding)
         (dft "new_chat_title" string "")
         (dft "new_chat_photo" (list PhotoSize.encoding) [])
         (dft "delete_chat_photo" bool false)
         (dft "group_chat_created" bool false)
         (dft "supergroup_chat_created" bool false)
         (dft "channel_chat_created" bool false)
         (dft "migrate_to_chat_id" int 0)
         (dft "migrate_from_chat_id" int 0)) in
    let misc_obj =
      (obj1
         (opt "pinned_message" any_ezjson_value)) in
    let obj =
      merge_objs header_obj
        (merge_objs content_obj1
           (merge_objs content_obj2
              (merge_objs chat_obj misc_obj))) in
    conv
      (fun { id ; from ; date ; chat ; fw_from ;
             fw_from_chat ; fw_from_msg_id ;
             fw_date ; reply_to_msg ; edit_date ;
             text ; entities ; audio ; document ;
             game ; photo ; sticker ; video ; voice ;
             caption ; contact ; location ; venue ;
             new_chat_member ; left_chat_member ;
             new_chat_title ; new_chat_photo ;
             delete_chat_photo ; group_chat_created ;
             supergroup_chat_created ; channel_chat_created ;
             migrate_to_chat_id ; migrate_from_chat_id ;
             pinned_msg ; } ->
        ((id, from, date, chat, fw_from, fw_from_chat, fw_from_msg_id,
         fw_date, reply_to_msg, edit_date
        ),
        ((text, entities, audio, document, game, photo, sticker, video,
         voice, caption),
        ((contact, location, venue),
        ((new_chat_member, left_chat_member, new_chat_title,
         new_chat_photo, delete_chat_photo, group_chat_created,
         supergroup_chat_created, channel_chat_created,
         migrate_to_chat_id, migrate_from_chat_id),
         ((pinned_msg))))))
      )
      (fun ((id, from, date, chat, fw_from, fw_from_chat, fw_from_msg_id,
         fw_date, reply_to_msg, edit_date
        ),
        ((text, entities, audio, document, game, photo, sticker, video,
         voice, caption),
        ((contact, location, venue),
        ((new_chat_member, left_chat_member, new_chat_title,
         new_chat_photo, delete_chat_photo, group_chat_created,
         supergroup_chat_created, channel_chat_created,
         migrate_to_chat_id, migrate_from_chat_id),
         ((pinned_msg)))))) ->
        { id ; from ; date ; chat ; fw_from ;
          fw_from_chat ; fw_from_msg_id ;
          fw_date ; reply_to_msg ; edit_date ;
          text ; entities ; audio ; document ;
          game ; photo ; sticker ; video ; voice ;
          caption ; contact ; location ; venue ;
          new_chat_member ; left_chat_member ;
          new_chat_title ; new_chat_photo ;
          delete_chat_photo ; group_chat_created ;
          supergroup_chat_created ; channel_chat_created ;
          migrate_to_chat_id ; migrate_from_chat_id ;
          pinned_msg ; })
      obj

  module Send = struct
    type parse_mode = Markdown | Html

    let parse_mode_to_string = function
    | Markdown -> "Markdown"
    | Html -> "HTML"

    let parse_mode_of_string = function
    | "Markdown" -> Some Markdown
    | "HTML" -> Some Html
    | _ -> None

    let parse_mode_encoding =
      let open Json_encoding in
      conv
        parse_mode_to_string
        (fun s -> match parse_mode_of_string s with
          | Some pm -> pm
          | None -> invalid_arg "parse_mode_encoding")
        string

    type t = {
      chat_id : int ;
      text : string ;
      parse_mode : parse_mode option ;
      disable_web_page_preview : bool ;
      disable_notification : bool ;
      reply_to_message_id : int ;
      reply_markup : Json_repr.ezjsonm option ;
    }

    let create ?parse_mode ?(disable_web_page_preview=false)
        ?(disable_notification=false) ?(reply_to_message_id=0)
        ?reply_markup ~chat_id ~text () =
      { chat_id ; text ; parse_mode ; disable_web_page_preview ;
        disable_notification ; reply_to_message_id ; reply_markup }

    let encoding =
      let open Json_encoding in
      conv
        (fun { chat_id ; text ; parse_mode ; disable_web_page_preview ;
               disable_notification ; reply_to_message_id ; reply_markup } ->
          (chat_id, text, parse_mode, disable_web_page_preview,
           disable_notification, reply_to_message_id, reply_markup))
        (fun (chat_id, text, parse_mode, disable_web_page_preview,
              disable_notification, reply_to_message_id, reply_markup) ->
          { chat_id ; text ; parse_mode ; disable_web_page_preview ;
            disable_notification ; reply_to_message_id ; reply_markup })
        (obj7
           (req "chat_id" int63_encoding)
           (req "text" string)
           (opt "parse_mode" parse_mode_encoding)
           (dft "disable_web_page_preview" bool false)
           (dft "disable_notification" bool false)
           (dft "reply_to_message_id" int 0)
           (opt "reply_markup" any_ezjson_value))
  end
end

module InlineQuery = struct
  type t = {
    id : string ;
    from : User.t ;
    location : Location.t option ;
    query : string ;
    offset : string ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; from ; location ; query ; offset } ->
         (id, from, location, query, offset))
      (fun (id, from, location, query, offset) ->
      { id ; from ; location ; query ; offset })
      (obj5
         (req "id" string)
         (req "from" User.encoding)
         (opt "location" Location.encoding)
         (req "query" string)
         (req "offset" string))
end

module ChosenInlineResult = struct
  type t = {
    id : string ;
    from : User.t ;
    location : Location.t option ;
    inline_msg_id : string ;
    query : string ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; from ; location ; inline_msg_id ; query } ->
         (id, from, location, inline_msg_id, query))
      (fun (id, from, location, inline_msg_id, query) ->
         { id ; from ; location ; inline_msg_id ; query } )
      (obj5
         (req "id" string)
         (req "user" User.encoding)
         (opt "location" Location.encoding)
         (dft "inline_message_id" string "")
         (req "query" string))
end

module Update = struct
  module Get = struct
    type kind =
      | Message
      | Edited_message
      | Channel_post
      | Edited_channel_post
      | Inline_query
      | Chosen_inline_result
      | Callback_query

    let kind_encoding =
      let open Json_encoding in
      string_enum [
        "message", Message ;
        "edited_message", Edited_message ;
        "channel_post", Channel_post ;
        "edited_channel_post", Edited_channel_post ;
        "inline_query", Inline_query ;
        "chosen_inline_query", Chosen_inline_result ;
        "callback_query", Callback_query ;
      ]

    type t = {
      offset : int option ;
      limit : int ;
      timeout : int ;
      kinds : kind list option ;
    }

    let create ?offset ?(limit=100) ?(timeout=0) ?kinds () =
      { offset ; limit ; timeout ; kinds }

    let encoding =
      let open Json_encoding in
      conv
        (fun { offset ; limit ; timeout ; kinds } ->
        (offset, limit, timeout, kinds))
        (fun (offset, limit, timeout, kinds) ->
           { offset ; limit ; timeout ; kinds })
        (obj4
           (opt "offset" int)
           (dft "limit" int 100)
           (dft "timeout" int 0)
           (opt "kinds" (list kind_encoding)))
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

  let encoding =
    let open Json_encoding in
    let with_update_id encoding =
      merge_objs (obj1 (req "update_id" int)) encoding in
    union [
      case (with_update_id ((obj1 (req "message" Message.encoding))))
        (function { id ; update = Message m } -> Some (id, m) | _ -> None)
        (fun (id, m) -> { id ; update = Message m })
      ;
      case (with_update_id ((obj1 (req "edited_message" Message.encoding))))
        (function { id ; update = Edited_message m } -> Some (id, m) | _ -> None)
        (fun (id, m) -> { id ; update = Edited_message m })
      ;
      case (with_update_id ((obj1 (req "channel_post" Message.encoding))))
        (function { id ; update = Channel_post m } -> Some (id, m) | _ -> None)
        (fun (id, m) -> { id ; update = Channel_post m })
      ;
      case (with_update_id ((obj1 (req "edited_channel_post" Message.encoding))))
        (function { id ; update = Edited_channel_post m } -> Some (id, m) | _ -> None)
        (fun (id, m) -> { id ; update = Edited_channel_post m })
      ;
      case (with_update_id (obj1 (req "inline_query" InlineQuery.encoding)))
        (function { id ; update = Inline_query q } -> Some (id, q) | _ -> None)
        (fun (id, q) -> { id ; update = Inline_query q })
      ;
      case (with_update_id (obj1 (req "chosen_inline_result" ChosenInlineResult.encoding)))
        (function { id ; update = Chosen_inline_result r } -> Some (id, r) | _ -> None)
        (fun (id, r) -> { id ; update = Chosen_inline_result r })
      ;
      case (with_update_id (obj1 (req "callback_query" any_ezjson_value)))
        (function { id ; update = Callback_query q } -> Some (id, q) | _ -> None)
        (fun (id, q) -> { id ; update = Callback_query q })
      ;
    ]
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
