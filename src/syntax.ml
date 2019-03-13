(** This module contains types for defining the syntax of a block and a DSL for
    building a block syntax and converting it into a block. *)

open Base
open Js_of_ocaml
open Types

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

let text_input ?(str="") () =
  let doc = Dom_svg.document in
  let elem () = new Widget.text_input ~str doc in
  let syn_elem = new Widget.text doc str in
  Syn_Widget ( (syn_elem :> Widget.t)
             , (elem :> unit -> Widget.t) )

let newline = Syn_Newline

let tab = Syn_Tab

(** Run the syntax to produce a fresh block. *)
let run symbol_of_term ?x ?y ctx syntax =
  let sym = syntax.syn_create () in
  let term = syntax.term_of_arity sym in
  let items =
    List.map ~f:(function
        | Syn_Child(_, hole_f) -> Child (hole_f sym)
        | Syn_Newline -> Newline
        | Syn_Widget(_, f) -> Widget (f ())
        | Syn_Tab -> Tab
      ) syntax.syn_items
  in
  let block = Block.create ?x ?y symbol_of_term ctx term items in
  List.iter items ~f:(function
      | Widget widget ->
         (* If the widget is ever resized, re-render the block *)
         widget#set_onresize (fun () ->
             ignore (Block.render_block_and_parents block.block)
           )
      | _ -> ()
    );
  block

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
            widget#set_y (Float.of_int y *. Block.col_height);
            x +. widget#width +. horiz_padding, y
         | Syn_Child(hole, _) ->
            hole.placeholder_group#set_x x;
            hole.placeholder_group#set_y (Float.of_int y *. Block.col_height);
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
