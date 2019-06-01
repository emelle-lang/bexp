(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Js_of_ocaml
open Types

module Placeholder = struct
  let create palette_data =
    let doc = Dom_svg.document in
    let text = new Widget.text doc palette_data.palette_name in
    text#element##.style##.fill := Js.string "white";
    let placeholder_group = new Widget.group doc in
    placeholder_group#element##.style##.fill := Js.string "green";
    placeholder_group#add_child (text :> Widget.t);
    set_style placeholder_group palette_data;
    { text; placeholder_group }

  let render t =
    t.placeholder_group#set_width (t.text#width);
    t.placeholder_group#set_height 20.0
end

let accept_border_color = Js.string "#a0ffa0"

let reject_border_color = Js.string "#ff8080"

let create term_of_symbol palette_data =
  let placeholder = Placeholder.create palette_data in
  let group = new Widget.group Dom_svg.document in
  Placeholder.render placeholder;
  group#add_child (placeholder.placeholder_group :> Widget.t);
  let hole =
    { hole_term = None
    ; term_of_symbol
    ; hole_placeholder = placeholder
    ; hole_parent = None
    ; hole_group = group
    ; hole_error = None }
  in
  placeholder.placeholder_group#element##.onclick :=
    Dom.handler (fun _->
        Option.iter ~f:(fun f -> f ()) hole.hole_error;
        Js._false
      );
  hole

let set_border_color hole color =
  hole.hole_placeholder.placeholder_group#rect_style##.stroke := color

let set_block_border_color hole color =
  Option.iter hole.hole_term ~f:(fun term ->
      term.block.group#rect_style##.stroke := color
    )

let highlight_accept hole = set_border_color hole accept_border_color

let highlight_reject hole = set_border_color hole reject_border_color

let unhighlight hole =
  match hole.hole_error with
  | None ->
     set_border_color hole (Js.string "white");
     set_block_border_color hole (Js.string "white")
  | Some _ ->
     set_border_color hole (Js.string "red");
     set_block_border_color hole (Js.string "red")

let append_to_group hole child =
  ignore (hole.hole_group#element##appendChild (child#element :> Dom.node Js.t))

let remove_from_group hole child =
  ignore (hole.hole_group#element##removeChild (child#element :> Dom.node Js.t))

let set_term hole term ~none ~some =
  match hole.term_of_symbol (term.symbol_of_term term) with
  | None ->
     unhighlight hole;
     none ()
  | Some term ->
     hole.hole_term <- Some term;
     term.block.parent <- Hole_parent (Hole hole);
     remove_from_group hole hole.hole_placeholder.placeholder_group;
     append_to_group hole term.block.group;
     term.block.group#element##.onclick :=
       Dom.handler (fun _ ->
           Option.iter ~f:(fun f -> f ()) hole.hole_error;
           Js._false
         );
     some term

let clear hole =
  match hole.hole_term with
  | Some term ->
     remove_from_group hole term.block.group;
     append_to_group hole hole.hole_placeholder.placeholder_group;
     set_block_border_color hole (Js.string "white");
     term.block.group#element##.onclick := Dom.handler (fun _ -> Js._false);
     hole.hole_term <- None;
  | None -> ()

let set_error hole error =
  hole.hole_error <- Some error;
  unhighlight hole

let clear_error hole =
  hole.hole_error <- None;
  unhighlight hole
