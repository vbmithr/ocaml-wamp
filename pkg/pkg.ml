#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let yojson = Conf.with_pkg "yojson"

let () =
  Pkg.describe "wamp" @@ fun c ->
  let yojson = Conf.value c yojson in
  Ok [
    Pkg.mllib "src/wamp.mllib";
    Pkg.mllib ~cond:yojson "src/wamp_yojson.mllib"
  ]
