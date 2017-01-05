(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

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
  | EVENT

let msgtyp_of_enum = function
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

let msgtyp_to_enum = function
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

type 'a dict = (string * 'a) list

type 'a hello = { realm: Uri.t; details: 'a dict }
type 'a welcome = { id: int; details: 'a dict }
type 'a details_reason = { details: 'a dict; reason: Uri.t }
type 'a goodbye = { details: 'a dict; reason: Uri.t }
type 'a error = { reqtype: int; reqid: int; details: 'a dict; error: Uri.t; args: 'a list; kwArgs: 'a dict }
type 'a publish = { reqid: int; options: 'a dict; topic: Uri.t; args: 'a list; kwArgs: 'a dict }
type ack = { reqid: int; id: int }
type 'a subscribe = { reqid: int; options: 'a dict; topic: Uri.t }
type 'a event = { subid: int; pubid: int; details: 'a dict; args: 'a list; kwArgs: 'a dict }

type 'a msg =
  | Hello of 'a hello
  | Welcome of 'a welcome
  | Abort of 'a details_reason
  | Goodbye of 'a details_reason
  | Error of 'a error
  | Publish of 'a publish
  | Published of ack
  | Subscribe of 'a subscribe
  | Subscribed of ack
  | Unsubscribe of ack
  | Unsubscribed of int
  | Event of 'a event

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

let ok_or_failwith = function
  | Ok v -> v
  | Error msg -> failwith msg

type role =
  | Subscriber
  | Publisher

let string_of_role = function
  | Subscriber -> "subscriber"
  | Publisher -> "publisher"
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
