open Base
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

let left (l, _) = Bexp.Hole l

let right (_, r) = Bexp.Hole r

let pred (p, _, _) = Bexp.Hole p

let conseq (_, c, _) = Bexp.Hole c

let alt (_, _, a) = Bexp.Hole a

let plus_def =
  let open Bexp.Builder in
  let items = eval doc [nt left; text "+"; nt right] in
  { Bexp.Syntax.items
  ; create =
      (fun () -> ( Bexp.Hole.create get_arith doc
                 , Bexp.Hole.create get_arith doc))
  ; to_term = fun x -> Add x }

let make_plus ctx =
  Bexp.Builder.run symbol_of_arith doc ctx plus_def

let if_def =
  let open Bexp.Builder in
  let items =
    eval doc
      [text "if"; nt pred; text "then"; newline;
       tab; nt conseq; newline;
       text "else"; newline;
       tab; nt alt
      ] in
  { Bexp.Syntax.items
  ; create =
      (fun () -> ( Bexp.Hole.create get_pred doc
                 , Bexp.Hole.create get_arith doc
                 , Bexp.Hole.create get_arith doc ))
  ; to_term = fun x -> If x }

let make_if ctx =
  Bexp.Builder.run symbol_of_arith doc ctx if_def

let eq_def =
  let open Bexp.Builder in
  let items = eval doc [nt left; text " = "; nt right] in
  { Bexp.Syntax.items
  ; create =
      (fun () -> ( Bexp.Hole.create get_arith doc
                 , Bexp.Hole.create get_arith doc ))
  ; to_term = fun x -> Equals x }

let make_eq ctx =
  Bexp.Builder.run symbol_of_pred doc ctx eq_def

let not_def =
  let open Bexp.Builder in
  let items = eval doc [text "not"; nt (fun x -> Bexp.Hole x)] in
  { Bexp.Syntax.items
  ; create = (fun () -> Bexp.Hole.create get_pred doc)
  ; to_term = fun x -> Not x }

let make_not ctx =
  Bexp.Builder.run symbol_of_pred doc ctx not_def

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
  Bexp.add_block ctx (make_if ctx);
  Bexp.add_block ctx (make_eq ctx);
  Bexp.add_block ctx (make_not ctx)
