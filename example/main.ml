open Base
open Js_of_ocaml

type expr =
  | Add of (block, expr) Bexp.term option ref
           * (block, expr) Bexp.term option ref
  | If of (block, expr) Bexp.term option ref
          * (block, expr) Bexp.term option ref
          * (block, expr) Bexp.term option ref
  | Num of int

and block = Block of (block, expr) Bexp.term

let doc = Dom_svg.document

let set_block r (Block nb) =
  r := Some nb

let f x = Block x

let make_plus ctx =
  let open Bexp.Builder in
  let left = Bexp.Hole.create set_block ctx doc in
  let right = Bexp.Hole.create set_block ctx doc in
  let items = eval doc [nt left; text "+"; nt right] in
  Bexp.Block.create f doc ctx (Add(left.Bexp.ptr, right.Bexp.ptr)) items

let make_if ctx =
  let open Bexp.Builder in
  let pred = Bexp.Hole.create set_block ctx doc in
  let conseq = Bexp.Hole.create set_block ctx doc in
  let otherwise = Bexp.Hole.create set_block ctx doc in
  let items =
    eval doc
      [text "if"; nt pred; text "then"; newline;
       tab; nt conseq; newline;
       text "else"; newline;
       tab; nt otherwise
      ] in
  Bexp.Block.create f doc ctx
    (If(pred.Bexp.ptr, conseq.Bexp.ptr, otherwise.Bexp.ptr)) items

let ctx =
  match
    Dom_svg.getElementById "workspace"
    |> Dom_svg.CoerceTo.svg
    |> Js.Opt.to_option
  with
  | None -> assert false
  | Some svg -> Bexp.create doc svg

let plus_block = make_plus ctx

let if_block = make_if ctx

let _ = Bexp.add_block ctx plus_block.Bexp.block

let _ = Bexp.add_block ctx if_block.Bexp.block
