(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Core_kernel
open Js_of_ocaml

type arith =
  | Add of binop
  | If of if_expr
  | Num of int

and binop =
  (symbols, arith) Bexp.hole' * (symbols, arith) Bexp.hole'

and if_expr =
  (symbols, pred) Bexp.hole'
  * (symbols, arith) Bexp.hole'
  * (symbols, arith) Bexp.hole'

and pred =
  | Equals of binop
  | Not of (symbols, pred) Bexp.hole'

and symbols =
  | Arith of (symbols, arith) Bexp.term'
  | Pred of (symbols, pred) Bexp.term'

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

let svg =
  match
    Dom_svg.getElementById "workspace"
    |> Dom_svg.CoerceTo.svg
    |> Js.Opt.to_option
  with
  | None -> assert false
  | Some svg -> svg

let width = Bexp.Widget.length_of_anim svg##.width

let height = Bexp.Widget.length_of_anim svg##.height

let ctx =
  Bexp.Workspace.create ~x:0.0 ~y:0.0 ~width ~height

let arith_data =
  { Bexp.palette_name = "arithmetic"
  ; Bexp.palette_color = "red" }

let pred_data =
  { Bexp.palette_name = "pred"
  ; Bexp.palette_color = "blue" }

let num_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [ text_input ~str:"120" () ]
    ~create:(fun () -> 120)
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
    ; Bexp.Syntax if_def ]

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.toolbox arith_palette

let () =
  ignore (svg##appendChild (ctx.Bexp.root_layer#element :> Dom.node Js.t));
  Bexp.Workspace.render ctx
