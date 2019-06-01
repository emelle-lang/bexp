(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Js_of_ocaml
open Types

let create container hole =
  let painter = Painter.create container in
  let x = 0.0 in
  let y = 0.0 in
  let width = Float.of_int (Painter.width painter) in
  let height = Float.of_int (Painter.height painter) in
  let doc = Dom_svg.document in
  let root_layer = new Widget.group ~x ~y ~width ~height doc in
  let toolbox = Toolbox.create ~x ~y ~width:200.0 ~height painter in
  let width' = width -. (Toolbox.width toolbox) in
  let scrollbox =
    new Widget.scrollbox
      ~x:(Toolbox.width toolbox) ~y ~width:width' ~height painter
  in
  ignore (root_layer#element##appendChild
            (toolbox.toolbox_scrollbox#element :> Dom.node Js.t));
  ignore (root_layer#element##appendChild
            (scrollbox#element :> Dom.node Js.t));
  ignore (scrollbox#group#element##appendChild
            (hole.hole_group#element :> Dom.node Js.t));
  ignore (painter.Painter.svg##appendChild
            (root_layer#element :> Dom.node Js.t));
  { painter
  ; root_layer
  ; picked_up_block = None
  ; scripts = Doubly_linked.create ()
  ; drop_candidate = None
  ; toolbox
  ; script_scrollbox = scrollbox
  ; entry_exists_hole = Hole hole }

let render ctx =
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Toolbox.render ctx.toolbox;
  ctx.script_scrollbox#render;
  let Hole hole = ctx.entry_exists_hole in
  Hole.Placeholder.render hole.hole_placeholder;
  Doubly_linked.iter ctx.scripts ~f:(fun (Term term) ->
      ignore (Block.render term.block)
    )
