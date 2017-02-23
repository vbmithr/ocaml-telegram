#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let async = Conf.with_pkg "async_ssl"
let cohttp = Conf.with_pkg "cohttp"
let () =
  Pkg.describe "telegram" @@ fun c ->
  let async = Conf.value c async in
  let cohttp = Conf.value c cohttp in
  Ok [
    Pkg.mllib "src/telegram.mllib" ;
    Pkg.mllib ~cond:(async && cohttp) "src/telegram_async.mllib" ;
    Pkg.bin ~cond:false ~auto:true "src/test_bot"
  ]
