open Base
open Js_of_ocaml

type arith =
  | Add of (sorts, arith) Bexp.term' option ref
           * (sorts, arith) Bexp.term' option ref
  | If of (sorts, pred) Bexp.term' option ref
          * (sorts, arith) Bexp.term' option ref
          * (sorts, arith) Bexp.term' option ref
  | Num of int

and pred =
  | Equals of (sorts, arith) Bexp.term' option ref
              * (sorts, arith) Bexp.term' option ref
  | Not of (sorts, pred) Bexp.term' option ref

and sorts =
  | Arith of (sorts, arith) Bexp.term'
  | Pred of (sorts, pred) Bexp.term'

let doc = Dom_svg.document

let get_arith = function
  | Arith a -> Some a
  | _ -> None

let get_pred = function
  | Pred a -> Some a
  | _ -> None

let sort_of_arith a = Arith a

let sort_of_pred p = Pred p

let make_plus ctx =
  let open Bexp.Builder in
  let left = Bexp.Hole.create get_arith doc in
  let right = Bexp.Hole.create get_arith doc in
  let items = eval doc [nt left; text "+"; nt right] in
  Bexp.Block.create sort_of_arith doc ctx
    (Add(left.Bexp.ptr, right.Bexp.ptr)) items

let make_if ctx =
  let open Bexp.Builder in
  let pred = Bexp.Hole.create get_pred doc in
  let conseq = Bexp.Hole.create get_arith doc in
  let otherwise = Bexp.Hole.create get_arith doc in
  let items =
    eval doc
      [text "if"; nt pred; text "then"; newline;
       tab; nt conseq; newline;
       text "else"; newline;
       tab; nt otherwise
      ] in
  Bexp.Block.create sort_of_arith doc ctx
    (If(pred.Bexp.ptr, conseq.Bexp.ptr, otherwise.Bexp.ptr)) items

let make_eq ctx =
  let open Bexp.Builder in
  let lhs = Bexp.Hole.create get_arith doc in
  let rhs = Bexp.Hole.create get_arith doc in
  let items = eval doc [nt lhs; text " = "; nt rhs] in
  Bexp.Block.create sort_of_pred doc ctx
    (Equals(lhs.Bexp.ptr, rhs.Bexp.ptr)) items

let make_not ctx =
  let open Bexp.Builder in
  let p = Bexp.Hole.create get_pred doc in
  let items = eval doc [text "not"; nt p] in
  Bexp.Block.create sort_of_pred doc ctx (Not p.Bexp.ptr) items

let ctx =
  match
    Dom_svg.getElementById "workspace"
    |> Dom_svg.CoerceTo.svg
    |> Js.Opt.to_option
  with
  | None -> assert false
  | Some svg -> Bexp.create doc svg

let () =
  Bexp.add_block ctx (make_plus ctx);
  Bexp.add_block ctx (make_eq ctx);
  Bexp.add_block ctx (make_not ctx);
  Bexp.add_block ctx (make_if ctx)
