open Base
open Js_of_ocaml
open Bexp

type num_expr =
  | Add of (block, num_expr) Block.t option ref
           * (block, num_expr) Block.t option ref
  | Num of int

and block = Block of (block, num_expr) Block.t

let doc = Dom_svg.document

let set_block r (Block nb) =
  r := Some nb

let make_plus () =
  let open Grammar in
  let left = Block.create_hole set_block doc in
  let right = Block.create_hole set_block doc in
  let items =
    eval
      (nt (Block.Hole left) @@ text "+" @@ nt (Block.Hole right) @@ empty)
      doc
  in Block.create doc (Add(left.Block.ptr, right.Block.ptr)) items

let ctx =
  match
    Dom_svg.getElementById "workspace"
    |> Dom_svg.CoerceTo.svg
    |> Js.Opt.to_option
  with
  | None -> assert false
  | Some svg -> Context.create doc svg

let plus_block = make_plus ()

let _ = Context.add_block ctx plus_block.Block.block
