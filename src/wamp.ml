module Uri = struct
  include Uri
  let of_yojson = function
    | `String uri -> begin
        try Ok (Uri.of_string uri) with exn -> Error "Uri.of_yojson"
      end
    | #Yojson.Safe.json -> Error "Uri.of_yojson"

  let to_yojson uri = `String (Uri.to_string uri)
end

module WList = struct
  type t = Yojson.Safe.json list
  let of_yojson = function
    | `List l -> Ok l
    | #Yojson.Safe.json -> Error "List.of_yojson"

  let to_yojson l = `List l
end

module Dict = struct
  type t = (string * Yojson.Safe.json) list
  let of_yojson = function
    | `Assoc dict -> Ok dict
    | #Yojson.Safe.json -> Error "Dict.of_yojson"

  let to_yojson dict = `Assoc dict
end

type msgtyp =
  | HELLO [@value 1]
  | WELCOME
  | ABORT
  | GOODBYE [@value 6]
  | ERROR [@value 8]
  | PUBLISH [@value 16]
  | PUBLISHED
  | SUBSCRIBE [@value 32]
  | SUBSCRIBED
  | UNSUBSCRIBE
  | UNSUBSCRIBED
  | EVENT [@@deriving enum]

type hello_t = { realm: Uri.t; details: Dict.t } [@@deriving create]
type welcome_t = { id: int; details: Dict.t } [@@deriving create]
type details_reason_t = { details: Dict.t; reason: Uri.t } [@@deriving create]
type goodbye_t = { details: Dict.t; reason: Uri.t } [@@deriving create]
type error_t = { reqtype: int; reqid: int; details: Dict.t; error: Uri.t; args: WList.t; kwArgs: Dict.t } [@@deriving create]
type publish_t = { reqid: int; options: Dict.t; topic: Uri.t; args: WList.t; kwArgs: Dict.t } [@@deriving create]
type ack_t = { reqid: int; id: int } [@@deriving create]
type subscribe_t = { reqid: int; options: Dict.t; topic: Uri.t } [@@deriving create]
type event_t = { subid: int; pubid: int; details: Dict.t; args: WList.t; kwArgs: Dict.t } [@@deriving create]

type msg =
  | Hello of hello_t
  | Welcome of welcome_t
  | Abort of details_reason_t
  | Goodbye of details_reason_t
  | Error of error_t
  | Publish of publish_t
  | Published of ack_t
  | Subscribe of subscribe_t
  | Subscribed of ack_t
  | Unsubscribe of ack_t
  | Unsubscribed of int
  | Event of event_t

let ok_or_failwith = function
  | Ok v -> v
  | Error msg -> failwith msg

let remaining_args = function
  | [`List args] -> args, []
  | [`List args; `Assoc kwArgs] -> args, kwArgs
  | _ -> [], []

let msg_of_yojson = function
  | `List ((`Int typ) :: content) -> begin
      match msgtyp_of_enum typ with
      | None -> Result.Error Printf.(sprintf "msg_of_json: invalid msg type %d" typ)
      | Some HELLO -> begin
          match content with
          | [`String uri; `Assoc details] ->
            let realm = Uri.of_string uri in
            Ok (Hello (create_hello_t ~realm ~details ()))
          | _ -> Result.Error "msg_of_yojson: HELLO"
        end
      | Some WELCOME -> begin
          match content with
          | [`Int id; `Assoc details] ->
            Ok (Welcome (create_welcome_t ~id ~details ()))
          | _ -> Result.Error "msg_of_yojson: WELCOME"
        end
      | Some ABORT -> begin
          match content with
          | [`Assoc details; `String reason] ->
            let reason = Uri.of_string reason in
            Ok (Abort (create_details_reason_t ~details ~reason ()))
          | _ -> Result.Error "msg_of_yojson: ABORT"
        end
      | Some GOODBYE -> begin
          match content with
          | [`Assoc details; `String reason] ->
            let reason = Uri.of_string reason in
            Ok (Goodbye (create_details_reason_t ~details ~reason ()))
          | _ -> Result.Error "msg_of_yojson: GOODBYE"
        end
      | Some ERROR -> begin
          match content with
          | `Int reqtype :: `Int reqid :: `Assoc details :: `String uri :: tl ->
            let uri = Uri.of_string uri in
            let args, kwArgs = remaining_args tl in
            Ok (Error (create_error_t reqtype reqid details uri args kwArgs ()))
          | _ -> Result.Error "msg_of_yojson: ERROR"
        end
      | Some PUBLISH -> begin
          match content with
          | `Int reqid :: `Assoc options :: `String topic :: tl ->
            let topic = Uri.of_string topic in
            let args, kwArgs = remaining_args tl in
            Ok (Publish (create_publish_t reqid options topic args kwArgs ()))
          | _ -> Result.Error "msg_of_yojson: PUBLISH"
        end
      | Some PUBLISHED -> begin
          match content with
          | [`Int reqid; `Int id] -> Ok (Published (create_ack_t ~reqid ~id ()))
          | _ -> Result.Error "msg_of_yojson: PUBLISHED"
        end
      | Some SUBSCRIBE -> begin
          match content with
          | [`Int reqid; `Assoc options; `String topic] ->
            let topic = Uri.of_string topic in
            Ok (Subscribe (create_subscribe_t reqid options topic ()))
          | _ -> Result.Error "msg_of_yojson: PUBLISH"
        end
      | Some SUBSCRIBED -> begin
          match content with
          | [`Int reqid; `Int id] -> Ok (Subscribed (create_ack_t ~reqid ~id ()))
          | _ -> Result.Error "msg_of_yojson: SUBSCRIBED"
        end
      | Some UNSUBSCRIBE -> begin
          match content with
          | [`Int reqid; `Int id] -> Ok (Unsubscribe (create_ack_t ~reqid ~id ()))
          | _ -> Result.Error "msg_of_yojson: UNSUBSCRIBE"
        end
      | Some UNSUBSCRIBED -> begin
          match content with
          | [`Int reqid] -> Ok (Unsubscribed reqid)
          | _ -> Result.Error "msg_of_yojson: UNSUBSCRIBED"
        end
      | Some EVENT -> begin
          match content with
          | `Int subid :: `Int pubid :: `Assoc details :: tl ->
            let args, kwArgs = remaining_args tl in
            Ok (Event (create_event_t subid pubid details args kwArgs ()))
          | _ -> Result.Error "msg_of_yojson: EVENT"
        end
    end
  | #Yojson.Safe.json as json -> Result.Error Yojson.Safe.(to_string json)

let msg_to_yojson = function
  | Hello { realm; details } ->
    `List [`Int (msgtyp_to_enum HELLO); `String (Uri.to_string realm); `Assoc details]
  | Welcome { id; details } ->
    `List [`Int (msgtyp_to_enum WELCOME); `Int id; `Assoc details ]
  | Abort { details; reason } ->
    `List [`Int (msgtyp_to_enum ABORT); `Assoc details; `String (Uri.to_string reason) ]
  | Goodbye { details; reason } ->
    `List [`Int (msgtyp_to_enum GOODBYE); `Assoc details; `String (Uri.to_string reason) ]
  | Error { reqtype; reqid; details; error; args; kwArgs } ->
    `List [`Int (msgtyp_to_enum ERROR); `Int reqtype; `Int reqid; `Assoc details; `String (Uri.to_string error); `List args; `Assoc kwArgs]
  | Publish { reqid; options; topic; args; kwArgs } ->
    `List [`Int (msgtyp_to_enum PUBLISH); `Int reqid; `Assoc options; `String (Uri.to_string topic); `List args; `Assoc kwArgs]
  | Published { reqid; id } ->
    `List [`Int (msgtyp_to_enum PUBLISHED); `Int reqid; `Int id]
  | Subscribe { reqid; options; topic } ->
    `List [`Int (msgtyp_to_enum SUBSCRIBE); `Int reqid; `Assoc options; `String (Uri.to_string topic)]
  | Subscribed { reqid; id } ->
    `List [`Int (msgtyp_to_enum SUBSCRIBED); `Int reqid; `Int id]
  | Unsubscribe { reqid; id } ->
    `List [`Int (msgtyp_to_enum UNSUBSCRIBE); `Int reqid; `Int id]
  | Unsubscribed reqid ->
    `List [`Int (msgtyp_to_enum UNSUBSCRIBED); `Int reqid]
  | Event { subid; pubid; details; args; kwArgs } ->
    `List [`Int (msgtyp_to_enum EVENT); `Int subid; `Int pubid; `Assoc details; `List args; `Assoc kwArgs]

type role =
  | Subscriber
  | Publisher

let show_role = function
  | Subscriber -> "subscriber"
  | Publisher -> "publisher"

let hello realm roles =
  let roles = ListLabels.map roles ~f:(fun r -> show_role r, `Assoc []) in
  Hello (create_hello_t ~realm ~details:["roles", `Assoc roles] ())

let subscribe ?(reqid=Random.bits ()) ?(options=[]) topic =
  reqid, Subscribe (create_subscribe_t reqid options topic ())
