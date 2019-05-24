(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type t = {
    buf : Buffer.t;
    indentation : int;
    indent : string;
  }

let indent t f =
  f { t with indentation = t.indentation + 1 }

let tab t =
  for _ = 1 to t.indentation do
    Buffer.add_string t.buf t.indent
  done

let print_line t f =
  tab t;
  let r = f t in
  Buffer.add_char t.buf '\n';
  r

let print_strln t str =
  print_line t (fun t -> Buffer.add_string t.buf str)

let rec print_concat ?(sep="") t = function
  | [] -> ()
  | [str] -> Buffer.add_string t.buf str
  | str :: strs ->
     Buffer.add_string t.buf str;
     print_concat ~sep t strs

let print_concatln ?sep t strs =
  print_line t (fun t -> print_concat ?sep t strs)

let get_arity symbols =
  List.fold symbols ~init:0 ~f:(fun acc next ->
      match next with
      | Syntax.Nonterminal _ | Input _ -> acc + 1
      | _ -> acc
    )

let getter ~arity ~idx =
  let rec gen_pattern str i target_index = function
    | 0 -> str ^ ")"
    | 1 -> str ^ if i = target_index then "x)" else "_)"
    | n ->
       let p = if i = target_index then "x" else "_" in
       gen_pattern (str ^ p ^ ", ") (i + 1) target_index (n - 1)
  in "fun " ^ (gen_pattern "(" 0 idx arity) ^ " -> x"

let arity =
  List.fold ~init:0 ~f:(fun acc next ->
      match next with
      | Syntax.Nonterminal _ | Input _ -> acc + 1
      | _ -> acc
    )

let emit_inputs t =
  let rec f idx = function
    | [] -> ()
    | (Syntax.Input default) :: xs ->
       print_line t (fun t ->
           List.iter ~f:(Buffer.add_string t.buf)
             [ "let input"
             ; Int.to_string idx
             ; " = Bexp.Widget.create_text_input \""
             ; String.escaped default
             ; "\" in" ]
         );
       f (idx + 1) xs
    | _ :: xs -> f idx xs
  in f 0

let emit_symbols t arity =
  let print_symbol idx input_idx = function
    | Syntax.String str ->
       Buffer.add_string t.buf "Bexp.Syntax.text \"";
       Buffer.add_string t.buf (String.escaped str);
       Buffer.add_char t.buf '"';
       idx, input_idx
    | Nonterminal name ->
       List.iter ~f:(Buffer.add_string t.buf)
         [ "Bexp.Syntax.nt "
         ; "(" ^ getter ~arity ~idx ^ ")"
         ; " "; name; "_data" ];
       idx + 1, input_idx
    | Input _ ->
       List.iter ~f:(Buffer.add_string t.buf)
         [ "Bexp.Syntax.widget input"
         ; Int.to_string input_idx
         ; " ("
         ; getter ~arity ~idx
         ; ")" ];
       idx + 1, input_idx + 1
    | Tab ->
       Buffer.add_string t.buf "Bexp.Syntax.tab";
       idx, input_idx
    | Newline ->
       Buffer.add_string t.buf "Bexp.Syntax.newline";
       idx, input_idx
  in
  let rec print_symbols idx input_idx = function
    | [] ->
       Buffer.add_string t.buf "]"
    | [symbol] ->
       let _ = print_symbol idx input_idx symbol in
       Buffer.add_string t.buf " ]"
    | symbol :: next ->
       let idx, input_idx = print_symbol idx input_idx symbol in
       Buffer.add_char t.buf '\n';
       tab t;
       Buffer.add_string t.buf "; ";
       print_symbols idx input_idx next
  in
  Buffer.add_string t.buf "[ ";
  print_symbols 0 0

let filter_args =
  List.filter ~f:(function
      | Syntax.Input _ | Nonterminal _ -> true
      | _ -> false
    )

let emit_create t list =
  let print_elem t idx input_idx symbol f = match symbol with
    | Syntax.Input _ ->
       List.iter ~f:(Buffer.add_string t.buf)
         [ "Bexp.Widget.create_text_input input"
         ; Int.to_string input_idx
         ; "#value" ];
       f t;
       idx + 1, input_idx + 1
    | Nonterminal name ->
       List.iter ~f:(Buffer.add_string t.buf)
         [ "Bexp.Hole.create get_"; name; " "; name; "_data" ];
       f t;
       idx + 1, input_idx
    | _ -> idx, input_idx
  in
  let rec print_create t idx input_idx = function
    | [] -> Buffer.add_char t.buf ')'
    | [symbol] ->
       let _ = print_elem t idx input_idx symbol (fun _t -> ()) in
       Buffer.add_string t.buf " )"
    | symbol :: next ->
       let idx, input_idx =
         print_elem t idx input_idx symbol (fun t ->
             Buffer.add_char t.buf '\n';
             tab t;
             Buffer.add_string t.buf ", "
           ) in
       print_create t idx input_idx next
  in
  Buffer.add_string t.buf "fun () ->\n";
  indent t (fun t ->
      tab t;
      Buffer.add_string t.buf "( ";
      print_create t 0 0 (filter_args list)
    )

let emit_prod t nt_name prod =
  print_line t (fun t ->
      Buffer.add_string t.buf "let ";
      Buffer.add_string t.buf prod.Syntax.prod_name;
      Buffer.add_string t.buf "_def ="
    );
  indent t (fun t ->
      emit_inputs t prod.Syntax.symbols;
      print_strln t "Bexp.Syntax.create";
      indent t (fun t ->
          let arity = arity prod.symbols in
          print_line t (fun t -> emit_symbols t arity prod.symbols);
          print_line t (fun t ->
              Buffer.add_string t.buf "~create:(";
              emit_create t prod.symbols;
              Buffer.add_char t.buf ')'
            );
          print_line t (fun t ->
              Buffer.add_string t.buf "~to_term:(";
              Buffer.add_string t.buf prod.action;
              Buffer.add_char t.buf ')');
          print_line t (fun t ->
              Buffer.add_string t.buf "~symbol_of_term:symbol_of_";
              Buffer.add_string t.buf nt_name)))

let emit_deflist t =
  let rec f = function
    | [] -> Buffer.add_string t.buf " ]"
    | [prod] ->
       Buffer.add_string t.buf
         ("Bexp.Syntax " ^ prod.Syntax.prod_name ^ "_def ]")
    | prod :: next ->
       Buffer.add_string t.buf
         ("Bexp.Syntax " ^ prod.Syntax.prod_name ^ "_def");
       Buffer.add_string t.buf "; ";
       f next
  in
  Buffer.add_string t.buf "[";
  f

let emit_nonterminal t nt =
  List.iter nt.Syntax.productions ~f:(fun prod ->
      print_line t (fun t -> emit_prod t nt.Syntax.nt_name prod);
    )

let emit_palette t prev nt =
  print_strln t ("let " ^ nt.Syntax.nt_name ^ "_palette =");
  indent t (fun t ->
      print_strln t ("Bexp.Palette.create ctx " ^ prev);
      indent t (fun t ->
          print_strln t (nt.nt_name ^ "_data");
          print_line t (fun t ->
              emit_deflist t nt.productions
            )
        )
    )

let emit_toplevel file =
  let t = { buf = Buffer.create 100; indentation = 0; indent = "  " } in
  Buffer.add_string t.buf file.Syntax.prelude;
  let nt's_rev =
    let rec f acc = function
      | [] -> acc
      | nt :: nt's ->
         emit_nonterminal t nt;
         f (nt :: acc) nt's
    in f [] file.Syntax.nonterminals
  in
  let rec f prev = function
    | [] -> ()
    | nt :: nt's ->
       emit_palette t prev nt;
       f ("(Some (Bexp.Palette " ^ nt.nt_name ^ "_palette))") nt's
  in
  f "None" nt's_rev;
  Buffer.add_string t.buf file.finale;
  t.buf
