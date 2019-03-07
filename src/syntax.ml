(** This module contains types for defining the syntax of a block and a DSL for
    building a block syntax and converting it into a block. *)

open Base
open Js_of_ocaml
open Basic

module Placeholder = struct
  let create str =
    let doc = Dom_svg.document in
    let text = new Widget.text ~style:"fill:#ffffff" doc str in
    let placeholder_group = new Widget.group doc in
    ignore (placeholder_group#element##appendChild
              (text#element :> Dom.node Js.t));
    { text; placeholder_group }

  let render t =
    t.placeholder_group#set_width (t.text#width);
    t.placeholder_group#set_height 20.0
end

(** "Nonterminal" *)
let nt hole_f = Syn_Child(Placeholder.create "<input>", hole_f)

(** Builds a widget that contains text *)
let text str =
  let doc = Dom_svg.document in
  let text_elem () = new Widget.text ~style:"fill:#ffffff" doc str in
  Syn_Widget ( (text_elem () :> Widget.t)
             , (text_elem :> unit -> Widget.t) )

let newline = Syn_Newline

let tab = Syn_Tab

(** Run the syntax to produce a fresh block. *)
let run symbol_of_term ?x ?y ctx syntax =
  let sym = syntax.syn_create () in
  let term = syntax.term_of_arity sym in
  let items =
    List.map ~f:(function
        | Syn_Child(_, hole_f) -> Basic.Child (hole_f sym)
        | Syn_Newline -> Basic.Newline
        | Syn_Widget(_, f) -> Basic.Widget (f ())
        | Syn_Tab -> Basic.Tab
      ) syntax.syn_items
  in Basic.Block.create ?x ?y symbol_of_term ctx term items

let render syntax =
  let horiz_padding = 4.0 in
  let rec loop max_width x y = function
    | [] -> max_width, y
    | item::items ->
       let x, y = match item with
         | Syn_Tab -> x +. 20.0, y
         | Syn_Newline -> horiz_padding, y + 1
         | Syn_Widget(widget, _) ->
            widget#set_x x;
            widget#set_y (Float.of_int y *. Basic.Block.col_height);
            x +. widget#width +. horiz_padding, y
         | Syn_Child(hole, _) ->
            hole.placeholder_group#set_x x;
            Placeholder.render hole;
            x +. hole.placeholder_group#width, y
       in
       let max_width = Float.max max_width x in
       loop max_width x y items
  in
  let (width, height) as dim =
    loop horiz_padding horiz_padding 0 syntax.syn_items in
  syntax.syn_group#set_width width;
  syntax.syn_group#set_height
    (Float.of_int (height + 1) *. Block.col_height);
  dim

let create ~create ~to_term items ~symbol_of_term workspace =
  let doc = Dom_svg.document in
  let group = new Widget.group ~rx:5.0 ~ry:5.0 ~style:(Block.style) doc in
  List.iter items ~f:(function
      | Syn_Child(hole, _) ->
         ignore
           (group#element##appendChild
              (hole.placeholder_group#element :> Dom.node Js.t))
      | Syn_Widget(w, _) ->
         ignore (group#element##appendChild (w#element :> Dom.node Js.t))
      | _ -> ()
    );
  let syntax =
    { syn_items = items
    ; syn_create = create
    ; term_of_arity = to_term
    ; syn_group = group } in
  group#element##.onmousedown :=
    Dom.handler (fun ev ->
        let x = Float.of_int ev##.clientX in
        let y = Float.of_int ev##.clientY in
        let term = run symbol_of_term ~x ~y workspace syntax in
        Workspace.add_block workspace term;
        ignore (Block.render_block_and_children term.block);
        Js._false
      );
  syntax
