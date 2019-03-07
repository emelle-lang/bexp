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

let ctx = Bexp.Workspace.create ~x:0.0 ~y:0.0 ~width ~height ()

let plus_def =
  let open Bexp.Syntax in
  create [nt left; text "+"; nt right]
    ~create:(fun () -> ( Bexp.Hole.create get_arith
                       , Bexp.Hole.create get_arith ))
    ~to_term:(fun args -> Add args)
    ~symbol_of_term:symbol_of_arith
    ctx

let make_plus ctx = Bexp.Syntax.run symbol_of_arith ctx plus_def

let if_def =
  let open Bexp.Syntax in
  create
    [text "if"; nt pred; text "then"; newline;
     tab; nt conseq; newline;
     text "else"; newline;
     tab; nt alt]
    ~create:(fun () -> ( Bexp.Hole.create get_pred
                       , Bexp.Hole.create get_arith
                       , Bexp.Hole.create get_arith ))
    ~to_term:(fun args -> If args)
    ~symbol_of_term:symbol_of_arith
    ctx

let make_if ctx = Bexp.Syntax.run symbol_of_arith ctx if_def

let eq_def =
  let open Bexp.Syntax in
  create [nt left; text " = "; nt right]
    ~create:(fun () -> ( Bexp.Hole.create get_arith
                       , Bexp.Hole.create get_arith ))
    ~to_term:(fun x -> Equals x)
    ~symbol_of_term:symbol_of_pred
    ctx

let make_eq ctx = Bexp.Syntax.run symbol_of_pred ctx eq_def

let not_def =
  let open Bexp.Syntax in
  create [text "not"; nt (fun x -> Bexp.Hole x)]
    ~create:(fun () -> Bexp.Hole.create get_pred)
    ~to_term:(fun args -> Not args)
    ~symbol_of_term:symbol_of_pred
    ctx

let make_not ctx = Bexp.Syntax.run symbol_of_pred ctx not_def

let palette =
  Bexp.Palette.create ~width:150.0 ~height
    [ Bexp.Syntax eq_def
    ; Bexp.Syntax not_def ]

let () =
  Bexp.Workspace.add_block ctx (make_plus ctx);
  Bexp.Workspace.add_block ctx (make_if ctx);
  Bexp.Workspace.add_block ctx (make_eq ctx);
  Bexp.Workspace.add_block ctx (make_not ctx);
  ignore (svg##appendChild (ctx.Bexp.root_layer#element :> Dom.node Js.t));
  Bexp.Workspace.render ctx
