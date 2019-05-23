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
