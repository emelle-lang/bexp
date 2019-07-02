(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

{
open Base
open Parser

exception Lex_error of string
}

let whitespace = [' ' '\t']
let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let alphanum = upper | lower | digit
let lident = lower (alphanum | '_' | '\'')*

rule main = parse
  | whitespace { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | '|' { BAR }
  | ':' { COLON }
  | '?' { QUESTION }
  | ';' { SEMICOLON }
  | "{{" {
      let lineno, str =
        ocaml_code lexbuf.Lexing.lex_curr_p.pos_lnum
          (Buffer.create 200) lexbuf
      in
      (* If I call Lexing.new_line directly in ocaml_code, the line number
         tracking doesn't work *)
      lexbuf.Lexing.lex_curr_p <-
        { lexbuf.lex_curr_p with Lexing.pos_lnum = lineno };
      OCAML_CODE str
    }
  | '\"' { STRING_LIT (lex_string (Buffer.create 20) lexbuf) }
  | "newline" { NEWLINE_COMMAND }
  | "tab" { TAB_COMMAND }
  | lident { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }

and ocaml_code lineno buffer = parse
  | "}}" { (lineno, Buffer.contents buffer) }
  | '\n' {
      Buffer.add_string buffer (Lexing.lexeme lexbuf);
      ocaml_code (lineno + 1) buffer lexbuf
    }
  | _ {
      Buffer.add_string buffer (Lexing.lexeme lexbuf);
      ocaml_code lineno buffer lexbuf
    }

and lex_string buffer = parse
  | '\"' { Buffer.contents buffer }
  | "\\\"" { Buffer.add_char buffer '\"'; lex_string buffer lexbuf }
  | "\\\'" { Buffer.add_char buffer '\''; lex_string buffer lexbuf }
  | "\\\\" { Buffer.add_char buffer '\\'; lex_string buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; lex_string buffer lexbuf }
  | "\\t" { Buffer.add_char buffer '\t'; lex_string buffer lexbuf }
  | [^ '\"' '\\' '\n' '\t']+ {
      Buffer.add_string buffer (Lexing.lexeme lexbuf); lex_string buffer lexbuf
    }
  | _ { raise (Lex_error (Lexing.lexeme lexbuf)) }
