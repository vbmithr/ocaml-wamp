#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

(* Sample configuration file *)

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "valet_core";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "Domoco Team <dev@domoco.fr>" ]
end
