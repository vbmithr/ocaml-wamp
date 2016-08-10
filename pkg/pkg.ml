#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let yojson = Conf.with_pkg "yojson"
let msgpck = Conf.with_pkg "msgpck"

let () =
  Pkg.describe "wamp" @@ fun c ->
  let yojson = Conf.value c yojson in
  let msgpck = Conf.value c msgpck in
  Ok [
    Pkg.mllib "src/wamp.mllib";
    Pkg.mllib ~cond:yojson "src/wamp_yojson.mllib";
    Pkg.mllib ~cond:msgpck "src/wamp_msgpck.mllib"
  ]
