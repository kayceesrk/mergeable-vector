#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mergeable-vector" @@ fun c ->
  Ok [ Pkg.mllib "src/mergeable_vector.mllib";
       Pkg.test "test/test"; ]
