(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** wamp-proto.org implementation in OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Wamp} *)

module MsgType : sig
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

  val of_enum : int -> t option
  val to_enum : t -> int
end

module Role : sig
  type t =
    | Subscriber
    | Publisher

  val to_string : t -> string
end

module Element : sig
  type arr = t list
  and dict = (string * t) list

  and t =
    | Int of int
    | String of string
    | Bool of bool
    | Dict of dict
    | List of arr

  val pp : Format.formatter -> t -> unit
end

open Element

module type S = sig
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

module type BACKEND = sig
  type repr

  val of_repr : repr -> Element.t
  val to_repr : Element.t -> repr
end

module Make (B: BACKEND) : S with type repr := B.repr

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
