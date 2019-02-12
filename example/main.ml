open Base
open Js_of_ocaml
open Bexp

let doc = Dom_svg.document

let concrete_syntax =
  let open Block.Grammar in
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
  | Add of (block, num_expr) t option ref * (block, num_expr) t option ref
  | Num of int

and block =
  | Num_block of (block, num_expr) t

let set_block r = function
  | Num_block nb ->
     r := Some nb

let root =
  let leaf = ref (Some
    { term = Num 0
    ; items = [] }
  ) in
  let leaf2 = ref (Some
    { term = Num 1
    ; items = [] }
  ) in
  { term = Add(leaf, leaf2)
  ; items =
      [ Child (Hole(leaf, set_block))
      ; Child (Hole(leaf2, set_block)) ] }
