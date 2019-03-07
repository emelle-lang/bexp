open Base
open Js_of_ocaml
open Basic

type ('symbols, 'sort) t = {
    group : Widget.group;
    syntactic_forms : ('symbols, 'sort) syntax list;
  }

let create ?x ?y ~width ~height syntactic_forms =
  let doc = Dom_svg.document in
  let style = "fill:#ababab" in
  let group = new Widget.group ?x ?y ~width ~height ~style doc in
  List.iter syntactic_forms ~f:(fun (Syntax syn) ->
      ignore
        (group#element##appendChild (syn.syn_group#element :> Dom.node Js.t))
    );
  { group
  ; syntactic_forms }

let render t =
  List.fold_left t.syntactic_forms ~init:0 ~f:(fun acc (Syntax syn) ->
      syn.syn_group#set_y (Float.of_int acc *. Block.col_height);
      let (_, cols) = Syntax.render syn in
      acc + cols + 1
    )
