(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core
open Stdio

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"..."
    [%map_open
     let infile = anon ("infile" %: string)
     and outfile = flag "o" (required string) ~doc:"Output file"
     in
     fun () ->
     try
       let file =
         In_channel.with_file infile ~f:(fun ifstream ->
             Parser.file Lexer.main (Lexing.from_channel ifstream)
           ) in
       let buf = Generate.emit_toplevel file in
       Out_channel.with_file outfile ~f:(fun ofstream ->
           Out_channel.output_buffer ofstream buf
         )
     with
     | Failure str -> print_endline str
    ]

let () = Command.run command
