open Base
open Js_of_ocaml
open Bexp

let doc = Dom_svg.document

let concrete_syntax =
  let open Grammar in
  let open Dsl in
  { productions =
      [ { symbols = eval (nt 0 @@ text "+" @@ nt 1 @@ empty) doc
        }
      ; { symbols = eval (nt 0 @@ text "-" @@ nt 1 @@ empty) doc
        }
      ]
  }

let ctx =
  match
    Dom_svg.getElementById "workspace"
    |> Dom_svg.CoerceTo.svg
    |> Js.Opt.to_option
  with
  | None -> assert false
  | Some svg -> Context.create concrete_syntax doc svg

type num_expr =
  | Add of (block, num_expr) Block.t option ref
           * (block, num_expr) Block.t option ref
  | Num of int

and block =
  | Num_block of (block, num_expr) Block.t

let set_block r = function
  | Num_block nb ->
     r := Some nb
