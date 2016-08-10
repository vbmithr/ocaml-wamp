(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** wamp-proto.org implementation in OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Wamp} *)

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

type 'a dict = (string * 'a) list

type 'a hello = { realm: Uri.t; details: 'a dict } [@@deriving create]
type 'a welcome = { id: int; details: 'a dict } [@@deriving create]
type 'a details_reason = { details: 'a dict; reason: Uri.t } [@@deriving create]
type 'a goodbye = { details: 'a dict; reason: Uri.t } [@@deriving create]
type 'a error = { reqtype: int; reqid: int; details: 'a dict; error: Uri.t; args: 'a list; kwArgs: 'a dict } [@@deriving create]
type 'a publish = { reqid: int; options: 'a dict; topic: Uri.t; args: 'a list; kwArgs: 'a dict } [@@deriving create]
type ack = { reqid: int; id: int } [@@deriving create]
type 'a subscribe = { reqid: int; options: 'a dict; topic: Uri.t } [@@deriving create]
type 'a event = { subid: int; pubid: int; details: 'a dict; args: 'a list; kwArgs: 'a dict } [@@deriving create]

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

type role = Subscriber | Publisher
val show_role : role -> string

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
