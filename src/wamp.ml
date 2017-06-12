(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

module MsgType = struct
  type t =
    | HELLO
    | WELCOME
    | ABORT
    | GOODBYE
    | ERROR
    | PUBLISH
    | PUBLISHED
    | SUBSCRIBE
    | SUBSCRIBED
    | UNSUBSCRIBE
    | UNSUBSCRIBED
    | EVENT

  let of_enum = function
  | 1 -> Some HELLO
  | 2 -> Some WELCOME
  | 3 -> Some ABORT
  | 6 -> Some GOODBYE
  | 8 -> Some ERROR
  | 16 -> Some PUBLISH
  | 17 -> Some PUBLISHED
  | 32 -> Some SUBSCRIBE
  | 33 -> Some SUBSCRIBED
  | 34 -> Some UNSUBSCRIBE
  | 35 -> Some UNSUBSCRIBED
  | 36 -> Some EVENT
  | _ -> None

  let to_enum = function
  | HELLO -> 1
  | WELCOME -> 2
  | ABORT -> 3
  | GOODBYE -> 6
  | ERROR -> 8
  | PUBLISH -> 16
  | PUBLISHED -> 17
  | SUBSCRIBE -> 32
  | SUBSCRIBED -> 33
  | UNSUBSCRIBE -> 34
  | UNSUBSCRIBED -> 35
  | EVENT -> 36
end

module Role = struct
  type t =
    | Subscriber
    | Publisher

  let to_string = function
  | Subscriber -> "subscriber"
  | Publisher -> "publisher"
end

module Element = struct
  type arr = t list
  and dict = (string * t) list

  and t =
    | Int of int
    | String of string
    | Bool of bool
    | Dict of dict
    | List of arr

  let pp ppf t =
    Format.fprintf ppf ""
end

module type BACKEND = sig
  type repr

  val of_repr : repr -> Element.t
  val to_repr : Element.t -> repr
end

module type S = sig
  open Element

  type repr
  type t =
    | Hello of { realm: Uri.t; details: dict }
    | Welcome of { id: int; details: dict }
    | Abort of { details: dict; reason: Uri.t }
    | Goodbye of { details: dict; reason: Uri.t }
    | Error of { reqtype: int; reqid: int; details: dict; error: Uri.t; args: arr; kwArgs: dict }
    | Publish of { reqid: int; options: dict; topic: Uri.t; args: arr; kwArgs: dict }
    | Published of { reqid: int; id: int }
    | Subscribe of { reqid: int; options: dict; topic: Uri.t }
    | Subscribed of { reqid: int; id: int }
    | Unsubscribe of { reqid: int; id: int }
    | Unsubscribed of int
    | Event of { subid: int; pubid: int; details: dict; args: arr; kwArgs: dict }

  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val of_repr : repr -> (t, string) Result.result
  val to_repr : t -> repr

  val hello : realm:Uri.t -> details:dict -> t
  val welcome : id:int -> details:dict -> t
  val abort : details:dict -> reason:Uri.t -> t
  val goodbye : details:dict -> reason:Uri.t -> t
  val error :
    reqtype:int -> reqid:int -> details:dict ->
    error:Uri.t -> args:arr -> kwArgs:dict -> t
  val publish :
    reqid:int -> options:dict -> topic:Uri.t ->
    args:arr -> kwArgs:dict -> t
  val published : reqid:int -> id:int -> t
  val subscribe : reqid:int -> options:dict -> topic:Uri.t -> t
  val subscribed : reqid:int -> id:int -> t
  val unsubscribe : reqid:int -> id:int -> t
  val unsubscribed : reqid:int -> t
  val event :
    subid:int -> pubid:int -> details:dict ->
    args:arr -> kwArgs:dict -> t

  module EZ : sig
    val hello : Uri.t -> Role.t list -> t
    val subscribe : ?reqid:int -> ?options: dict -> Uri.t -> int * t
  end
end

module Make (B: BACKEND) = struct
  open Element

  type t =
    | Hello of { realm: Uri.t; details: dict }
    | Welcome of { id: int; details: dict }
    | Abort of { details: dict; reason: Uri.t }
    | Goodbye of { details: dict; reason: Uri.t }
    | Error of { reqtype: int; reqid: int; details: dict; error: Uri.t; args: arr; kwArgs: dict }
    | Publish of { reqid: int; options: dict; topic: Uri.t; args: arr; kwArgs: dict }
    | Published of { reqid: int; id: int }
    | Subscribe of { reqid: int; options: dict; topic: Uri.t }
    | Subscribed of { reqid: int; id: int }
    | Unsubscribe of { reqid: int; id: int }
    | Unsubscribed of int
    | Event of { subid: int; pubid: int; details: dict; args: arr; kwArgs: dict }

  let pp ppf t =
    Format.fprintf ppf ""

  let show t =
    Format.asprintf "%a" pp t

  let hello ~realm ~details = Hello { realm ; details }
  let welcome ~id ~details = Welcome { id ; details }
  let abort ~details ~reason = Abort { details ; reason }
  let goodbye ~details ~reason = Goodbye { details ; reason }
  let error ~reqtype ~reqid ~details ~error ~args ~kwArgs =
    Error { reqtype ; reqid ; details ; error ; args ; kwArgs }
  let publish ~reqid ~options ~topic ~args ~kwArgs =
    Publish { reqid ; options ; topic ; args ; kwArgs }
  let published ~reqid ~id = Published { reqid ; id }
  let subscribe ~reqid ~options ~topic = Subscribe { reqid ; options ; topic }
  let subscribed ~reqid ~id = Subscribed { reqid ; id }
  let unsubscribe ~reqid ~id = Unsubscribe { reqid ; id }
  let unsubscribed ~reqid = Unsubscribed reqid
  let event ~subid ~pubid ~details ~args ~kwArgs =
    Event { subid ; pubid ; details ; args ; kwArgs }

  let remaining_args = function
  | [List args] -> args, []
  | [List args; Dict kwArgs] -> args, kwArgs
  | _ -> [], []

  let of_repr repr = match B.of_repr repr with
  | List (Int typ :: content) -> begin match MsgType.of_enum typ with
    | None ->
        Result.Error Printf.(sprintf "Wamp.Make(_).parse: invalid msg type %d" typ)
    | Some HELLO -> begin match content with
      | [String uri; Dict details] ->
          let realm = Uri.of_string uri in
          Ok (hello ~realm ~details)
      | _ -> Error "msg_of_msgpck: HELLO"
      end
    | Some WELCOME -> begin match content with
      | [Int id; Dict details] ->
          Ok (welcome ~id ~details)
      | _ -> Error "msg_of_msgpck: WELCOME"
      end
    | Some ABORT -> begin match content with
      | [Dict details; String reason] ->
          let reason = Uri.of_string reason in
          Ok (abort ~details ~reason)
      | _ -> Error "msg_of_msgpck: ABORT"
      end
    | Some GOODBYE -> begin
        match content with
        | [Dict details; String reason] ->
            let reason = Uri.of_string reason in
            Ok (goodbye ~details ~reason)
        | _ -> Error "msg_of_msgpck: GOODBYE"
      end
    | Some ERROR -> begin
        match content with
        | Int reqtype :: Int reqid :: Dict details :: String uri :: tl ->
            let uri = Uri.of_string uri in
            let args, kwArgs = remaining_args tl in
            Ok (error ~reqtype ~reqid ~details ~error:uri ~args ~kwArgs)
        | _ -> Error "msg_of_msgpck: ERROR"
      end
    | Some PUBLISH -> begin
        match content with
        | Int reqid :: Dict options :: String topic :: tl ->
            let topic = Uri.of_string topic in
            let args, kwArgs = remaining_args tl in
            Ok (publish ~reqid ~options ~topic ~args ~kwArgs)
        | _ -> Error "msg_of_msgpck: PUBLISH"
      end
    | Some PUBLISHED -> begin
        match content with
        | [Int reqid; Int id] ->
            Ok (published ~reqid ~id)
        | _ -> Error "msg_of_msgpck: PUBLISHED"
      end
    | Some SUBSCRIBE -> begin
        match content with
        | [Int reqid; Dict options; String topic] ->
            let topic = Uri.of_string topic in
            Ok (subscribe reqid options topic)
        | _ -> Error "msg_of_msgpck: PUBLISH"
      end
    | Some SUBSCRIBED -> begin
        match content with
        | [Int reqid; Int id] ->
            Ok (subscribed ~reqid ~id)
        | _ -> Error "msg_of_msgpck: SUBSCRIBED"
      end
    | Some UNSUBSCRIBE -> begin
        match content with
        | [Int reqid; Int id] ->
            Ok (unsubscribe ~reqid ~id)
        | _ -> Error "msg_of_msgpck: UNSUBSCRIBE"
      end
    | Some UNSUBSCRIBED -> begin
        match content with
        | [Int reqid] -> Ok (unsubscribed reqid)
        | _ -> Error "msg_of_msgpck: UNSUBSCRIBED"
      end
    | Some EVENT -> begin
        match content with
        | Int subid :: Int pubid :: Dict details :: tl ->
            let args, kwArgs = remaining_args tl in
            Ok (event ~subid ~pubid ~details ~args ~kwArgs)
        | _ -> Error "msg_of_msgpck: EVENT"
      end
    end
  | msg -> Error "msg_of_msgpck: msg must be a List"

  let to_element = function
  | Hello { realm; details } ->
      List [Int (MsgType.to_enum HELLO); String (Uri.to_string realm); Dict details]
  | Welcome { id; details } ->
      List [Int (MsgType.to_enum WELCOME); Int id; Dict details ]
  | Abort { details; reason } ->
      List [Int (MsgType.to_enum ABORT); Dict details; String (Uri.to_string reason) ]
  | Goodbye { details; reason } ->
      List [Int (MsgType.to_enum GOODBYE); Dict details; String (Uri.to_string reason) ]
  | Error { reqtype; reqid; details; error; args; kwArgs } ->
      List [Int (MsgType.to_enum ERROR); Int reqtype; Int reqid; Dict details; String (Uri.to_string error); List args; Dict kwArgs]
  | Publish { reqid; options; topic; args; kwArgs } ->
      List [Int (MsgType.to_enum PUBLISH); Int reqid; Dict options; String (Uri.to_string topic); List args; Dict kwArgs]
  | Published { reqid; id } ->
      List [Int (MsgType.to_enum PUBLISHED); Int reqid; Int id]
  | Subscribe { reqid; options; topic } ->
      List [Int (MsgType.to_enum SUBSCRIBE); Int reqid; Dict options; String (Uri.to_string topic)]
  | Subscribed { reqid; id } ->
      List [Int (MsgType.to_enum SUBSCRIBED); Int reqid; Int id]
  | Unsubscribe { reqid; id } ->
      List [Int (MsgType.to_enum UNSUBSCRIBE); Int reqid; Int id]
  | Unsubscribed reqid ->
      List [Int (MsgType.to_enum UNSUBSCRIBED); Int reqid]
  | Event { subid; pubid; details; args; kwArgs } ->
      List [Int (MsgType.to_enum EVENT); Int subid; Int pubid; Dict details; List args; Dict kwArgs]

  let to_repr t = B.to_repr (to_element t)

  module EZ = struct
    let hello realm roles =
      let roles =
        Dict (ListLabels.map roles ~f:(fun r -> Role.to_string r, Dict [])) in
      hello ~realm ~details:["roles", roles]

    let subscribe ?(reqid=Random.bits ()) ?(options=[]) topic =
      reqid, (subscribe reqid options topic)
  end
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

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
