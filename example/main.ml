(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Js_of_ocaml

module T =
  [%symbol
   type arith =
     | Add of binop
     | If of if_expr
     | Let of
         (Bexp.Widget.text_input * (symbols, arith) Bexp.hole
          * (symbols, arith) Bexp.hole)
     | Num of Bexp.Widget.text_input
     | Var of Bexp.Widget.text_input

   and binop =
     (symbols, arith) Bexp.hole * (symbols, arith) Bexp.hole

   and if_expr =
     (symbols, pred) Bexp.hole
     * (symbols, arith) Bexp.hole
     * (symbols, arith) Bexp.hole

   and pred =
     | Equals of binop
     | Not of (symbols, pred) Bexp.hole
  ]

open T

let doc = Dom_svg.document

let get_arith = function
  | Arith a -> Some a
  | _ -> None

let get_pred = function
  | Pred a -> Some a
  | _ -> None

let symbol_of_arith a = Arith a

let symbol_of_pred p = Pred p

let left (l, _) = l

let right (_, r) = r

let pred (p, _, _) = p

let conseq (_, c, _) = c

let alt (_, _, a) = a

let container = Dom_html.getElementById "workspace-div"

let arith_data =
  { Bexp.palette_name = "arithmetic"
  ; Bexp.palette_color = "red" }

let pred_data =
  { Bexp.palette_name = "pred"
  ; Bexp.palette_color = "blue" }

let main_hole = Bexp.Hole.create get_arith arith_data

let ctx = Bexp.Workspace.create container main_hole

let num_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "120" in
  Bexp.Syntax.create [ widget input (fun x -> x) ]
    ~create:(fun () -> Bexp.Widget.create_text_input input#value)
    ~to_term:(fun args -> Num args)
    ~symbol_of_term:symbol_of_arith

let plus_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [nt left arith_data; text "+"; nt right arith_data]
    ~create:(fun () -> ( Bexp.Hole.create get_arith arith_data
                       , Bexp.Hole.create get_arith arith_data ))
    ~to_term:(fun args -> Add args)
    ~symbol_of_term:symbol_of_arith

let if_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create
    [text "if"; nt pred pred_data; text "then"; newline;
     tab; nt conseq arith_data; newline;
     text "else"; newline;
     tab; nt alt arith_data]
    ~create:(fun () -> ( Bexp.Hole.create get_pred pred_data
                       , Bexp.Hole.create get_arith arith_data
                       , Bexp.Hole.create get_arith arith_data ))
    ~to_term:(fun args -> If args)
    ~symbol_of_term:symbol_of_arith

let let_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "x" in
  Bexp.Syntax.create
    [text "let"; widget input (fun (x, _, _) -> x); text "=";
     nt (fun (_, x, _) -> x) arith_data; text "in"; newline;
     nt (fun (_, _, x) -> x) arith_data]
    ~create:(fun () -> ( Bexp.Widget.create_text_input input#value
                       , Bexp.Hole.create get_arith arith_data
                       , Bexp.Hole.create get_arith arith_data ))
    ~to_term:(fun args -> Let args)
    ~symbol_of_term:symbol_of_arith

let eq_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [nt left arith_data; text " = "; nt right arith_data]
    ~create:(fun () -> ( Bexp.Hole.create get_arith arith_data
                       , Bexp.Hole.create get_arith arith_data ))
    ~to_term:(fun x -> Equals x)
    ~symbol_of_term:symbol_of_pred

let not_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [text "not"; nt (fun x -> x) pred_data]
    ~create:(fun () -> Bexp.Hole.create get_pred pred_data)
    ~to_term:(fun args -> Not args)
    ~symbol_of_term:symbol_of_pred

let pred_palette =
  Bexp.Palette.create ctx None
    pred_data
    [ Bexp.Syntax eq_def
    ; Bexp.Syntax not_def ]

let arith_palette =
  Bexp.Palette.create ctx (Some (Palette pred_palette))
    arith_data
    [ Bexp.Syntax num_def
    ; Bexp.Syntax plus_def
    ; Bexp.Syntax if_def
    ; Bexp.Syntax let_def ]

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.toolbox arith_palette;
  Bexp.Workspace.render ctx
