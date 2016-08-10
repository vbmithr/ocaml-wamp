#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "wamp" @@ fun c ->
  Ok [ Pkg.mllib "src/wamp.mllib" ]
