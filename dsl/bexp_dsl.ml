(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Stdio

let infile = ref None
let outfile = ref None

let set_infile str =
  match !infile with
  | None -> infile := Some str
  | Some _ -> failwith "Unexpected argument"

let set_outfile str =
  match !outfile with
  | None -> outfile := Some str
  | Some _ -> failwith "Unexpected argument"

let go infile outfile =
  try
    let file =
      In_channel.with_file infile ~f:(fun ifstream ->
          let lexer = Lexing.from_channel ifstream in
          let lexer =
            { lexer with
              Lexing.lex_curr_p =
                { lexer.Lexing.lex_curr_p with
                  Lexing.pos_fname = infile }
            }
          in
          Parser.file Lexer.main lexer
        ) in
    let buf = Generate.emit_toplevel outfile file in
    Out_channel.with_file outfile ~f:(fun ofstream ->
        Out_channel.output_buffer ofstream buf
      )
  with
  | Failure str -> print_endline str

let () =
  Caml.Arg.parse
    ["-o", String set_outfile, "Output file"]
    set_infile
    "Generate OCaml boilerplate code from a specification";
  match !infile, !outfile with
  | Some infile, Some outfile -> go infile outfile
  | None, Some _ -> print_endline "Missing outfile"
  | Some _, None -> print_endline "Missing infile"
  | None, None -> print_endline "Missing infile and outfile"
