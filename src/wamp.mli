(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** wamp-proto.org implementation in OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Wamp} *)

module WList : sig type t = Yojson.Safe.json list end
module Dict : sig type t = (string * Yojson.Safe.json) list end

type hello_t = { realm: Uri.t; details: Dict.t }
type welcome_t = { id: int; details: Dict.t }
type details_reason_t = { details: Dict.t; reason: Uri.t }
type goodbye_t = { details: Dict.t; reason: Uri.t }
type error_t = { reqtype: int; reqid: int; details: Dict.t; error: Uri.t; args: WList.t; kwArgs: Dict.t }
type publish_t = { reqid: int; options: Dict.t; topic: Uri.t; args: WList.t; kwArgs: Dict.t }
type ack_t = { reqid: int; id: int }
type subscribe_t = { reqid: int; options: Dict.t; topic: Uri.t }
type event_t = { subid: int; pubid: int; details: Dict.t; args: WList.t; kwArgs: Dict.t }

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
[@@deriving yojson]

type role = Subscriber | Publisher

val hello: Uri.t -> role list -> msg
val subscribe: ?reqid:int -> ?options:Dict.t -> Uri.t -> int * msg

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
