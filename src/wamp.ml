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

let show_error = function
  | Invalid_uri -> "wamp.error.invalid_uri"
  | No_such_procedure -> "wamp.error.no_such_procedure"
  | Procedure_already_exists -> "wamp.error.procedure_already_exists"
  | No_such_registration -> "wamp.error.no_such_registration"
  | No_such_subscription -> "wamp.error.no_such_subscription"
  | Invalid_argument -> "wamp.error.invalid_argument"
  | System_shutdown -> "wamp.error.system_shutdown"
  | Close_realm -> "wamp.error.close_realm"
  | Goodbye_and_out -> "wamp.error.goodbye_and_out"
  | Not_authorized -> "wamp.error.not_authorized"
  | Authorization_failed -> "wamp.error.authorization_failed"
  | No_such_realm -> "wamp.error.no_such_realm"
  | No_such_role -> "wamp.error.no_such_role"

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

let msg_of_json = function
  | `List ((`Int typ) :: content) -> begin
    match msgtype_of_enum typ with
    | None -> invalid_arg Printf.(sprintf "msg_of_json: invalid msg type %d" typ)
    | Some typ -> create_msg ~typ ~content ()
    end
  | #Yojson.Safe.json as json ->
    invalid_arg Yojson.Safe.(to_string json)

let json_of_msg { typ; content } = `List (`Int (msgtype_to_enum typ) :: content)

type role =
  | Subscriber
  | Publisher

let show_role = function
  | Subscriber -> "subscriber"
  | Publisher -> "publisher"

let hello ?(roles=[Subscriber]) ~realm () =
  let roles = ListLabels.map roles ~f:(fun r -> show_role r, `Assoc []) in
  create_msg ~typ:Hello ~content:[`String realm; `Assoc ["roles", `Assoc roles]] ()

let subscribe ?(id=Random.bits ()) ?(options=`Assoc []) ~topic () =
  id, create_msg ~typ:Subscribe ~content:[`Int id; options; `String topic] ()

let subscribed_of_msg ?request_id { typ; content } = match typ with
  | Subscribed -> begin match content with
      | [`Int req_id; `Int sub_id] -> begin
          match request_id with
          | None -> sub_id
          | Some req_id' ->
            if req_id = req_id' then sub_id
            else failwith "subscribed_of_msg: request id mismatch"
        end
      | _ -> invalid_arg "subscribed_of_msg: wrong content"
    end
  | _ -> invalid_arg "subscribed_of_msg: wrong msg type"

type event = {
  sub_id: int;
  pub_id: int;
  details: (string * Yojson.Safe.json) list;
  args: Yojson.Safe.json list [@default []];
  argsKw: (string * Yojson.Safe.json) list [@default []];
} [@@deriving create]

let event_of_msg { typ; content } = match typ with
  | Event -> begin match content with
      | [ `Int sub_id; `Int pub_id; `Assoc details ] ->
        create_event ~sub_id ~pub_id ~details ()
      | [ `Int sub_id; `Int pub_id; `Assoc details; `List args ] ->
        create_event ~sub_id ~pub_id ~details ~args ()
      | [ `Int sub_id; `Int pub_id; `Assoc details; `List args; `Assoc argsKw ] ->
        create_event ~sub_id ~pub_id ~details ~args ~argsKw ()
      | _ ->
        invalid_arg Printf.(sprintf "event_of_msg: wrong content %s" Yojson.Safe.(to_string (`List content)))
    end
  | _ -> invalid_arg "event_of_msg: wrong msg type"
