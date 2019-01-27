open Base
open Js_of_ocaml
open Bexp

module Abs_Syn = struct
  type symbol =
    | Add
    | Sub
    | Mul
    | Div
    | Num

  (* Everything is an expression *)
  type sort = Expr

  let is _ = Expr

  let at sym idx =
    match sym, idx with
    | (Add | Sub | Mul | Div), (0 | 1) -> Some (Syntax.Function_symbol Expr)
    | (Add | Sub | Mul | Div), _ -> None
    | Num, 0 -> Some (Syntax.Float 0.0)
    | Num, _ -> None

  let enum_symbols f acc =
    (* Flip arguments for convenience *)
    let f x y = f y x in
    acc
    |> f Add
    |> f Sub
    |> f Mul
    |> f Div
    |> f Num
end

module Ctx = Context.Make(Abs_Syn)

let doc = Dom_svg.document

let concrete_syntax =
  let open Ctx.Block.Concrete_Syntax in
  let open Dsl in
  { productions =
      [ { function_symbol = Abs_Syn.Add
        ; symbols = eval (nt 0 @@ text "+" @@ nt 1 @@ empty) doc
        }
      ; { function_symbol = Abs_Syn.Sub
        ; symbols = eval (nt 0 @@ text "-" @@ nt 1 @@ empty) doc
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
  | Some svg -> Ctx.create concrete_syntax doc svg
