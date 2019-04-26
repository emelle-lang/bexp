(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core_kernel
open Js_of_ocaml
open Types

let create container hole =
  let doc = Dom_svg.document in
  let svg_elem = Dom_svg.createSvg doc in
  ignore (container##appendChild (svg_elem :> Dom.node Js.t));
  let width = container##.offsetWidth in
  let height = container##.offsetHeight in
  Widget.set_string_prop svg_elem "width" (Int.to_string width ^ "px");
  Widget.set_string_prop svg_elem "height" (Int.to_string height ^ "px");
  Widget.set_string_prop svg_elem "style" "display: block";
  let x = 0.0 in
  let y = 0.0 in
  let width = Float.of_int width in
  let height = Float.of_int height in
  let root_layer = new Widget.group ~x ~y ~width ~height doc in
  let toolbox = Toolbox.create ~x ~y ~width:200.0 ~height in
  let width' = width -. (Toolbox.width toolbox) in
  let scrollbox =
    new Widget.scrollbox
      ~x:(Toolbox.width toolbox) ~y ~width:width' ~height doc
  in
  ignore (root_layer#element##appendChild
            (toolbox.toolbox_scrollbox#element :> Dom.node Js.t));
  ignore (root_layer#element##appendChild
            (scrollbox#element :> Dom.node Js.t));
  ignore (scrollbox#group#element##appendChild
            (hole.hole_group#element :> Dom.node Js.t));
  ignore (svg_elem##appendChild (root_layer#element :> Dom.node Js.t));
  { container
  ; svg_elem
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
  let Hole hole = ctx.entry_exists_hole in
  Hole.Placeholder.render hole.hole_placeholder;
  Doubly_linked.iter ctx.scripts ~f:(fun (Term term) ->
      ignore (Block.render term.block)
    )
