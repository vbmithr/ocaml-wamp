type error =
  | Invalid_uri
  | No_such_procedure
  | Procedure_already_exists
  | No_such_registration
  | No_such_subscription
  | Invalid_argument
  | System_shutdown
  | Close_realm
  | Goodbye_and_out
  | Not_authorized
  | Authorization_failed
  | No_such_realm
  | No_such_role

val show_error : error -> string

type msgtype =
  | Hello [@value 1]
  | Welcome
  | Abort
  | Goodbye [@value 6]
  | Error [@value 8]
  | Publish [@value 16]
  | Published
  | Subscribe [@value 32]
  | Subscribed
  | Unsubscribe
  | Unsubscribed
  | Event
  | Call [@value 48]
  | Result [@value 50]
  | Register [@value 64]
  | Registered
  | Unregister
  | Unregistered
  | Invocation
  | Yield [@value 70]
  [@@deriving show,enum]

type msg = {
  typ: msgtype;
  content: Yojson.Safe.json list;
} [@@deriving create]

val json_of_msg : msg -> Yojson.Safe.json
val msg_of_json : Yojson.Safe.json -> msg

type role = Subscriber | Publisher

val hello: ?roles:role list -> realm:string -> unit -> msg

val subscribe: ?id:int -> ?options:Yojson.Safe.json -> topic:string -> unit -> int * msg

val subscribed_of_msg : ?request_id:int -> msg -> int
(** [subscribed_of_msg ?request_id msg] parse a Subscribed-message and
    returns the subscription id. If [request_id] is provided, it will
    be checked against the request_id found in [msg]. *)

type event = {
  sub_id: int;
  pub_id: int;
  details: (string * Yojson.Safe.json) list;
  args: Yojson.Safe.json list;
  argsKw: (string * Yojson.Safe.json) list
}

val event_of_msg : msg -> event
