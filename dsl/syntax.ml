(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type symbol =
  | String of string
  | Nonterminal of string
  | Input of string
  | Tab
  | Newline

type production = {
    prod_name : string;
    symbols : symbol list;
    action : string;
  }

type nonterminal = {
    nt_name : string;
    productions : production list;
  }

type t = {
    prelude : string;
    nonterminals : nonterminal list;
    finale : string;
  }
