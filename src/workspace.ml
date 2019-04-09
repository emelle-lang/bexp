(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Core_kernel
open Js_of_ocaml
open Types

let create ?x ?y ~width ~height hole =
  let doc = Dom_svg.document in
  let root_layer = new Widget.group ?x ?y ~width ~height doc in
  let toolbox = Toolbox.create ?x ?y ~width:150.0 ~height in
  root_layer#rect_style##.fill := Js.string "grey";
  ignore (root_layer#element##appendChild
            (toolbox.toolbox_group#element :> Dom.node Js.t));
  ignore (root_layer#element##appendChild
            (hole.hole_group#element :> Dom.node Js.t));
  { root_layer
  ; picked_up_block = None
  ; scripts = Doubly_linked.create ()
  ; drop_candidate = None
  ; toolbox
  ; entry_exists_hole = Hole hole }

let add_block ctx term =
  term.block.parent <-
    Root (Doubly_linked.insert_first ctx.scripts (Term term));
  ignore
    (ctx.root_layer#element##appendChild
       (term.block.group#element :> Dom.node Js.t))

let render ctx =
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Toolbox.render ctx.toolbox;
  let Hole hole = ctx.entry_exists_hole in
  hole.hole_group#set_x (ctx.toolbox.toolbox_group#width);
  Hole.Placeholder.render hole.hole_placeholder;
  Doubly_linked.iter ctx.scripts ~f:(fun (Term term) ->
      ignore (Block.render_block_and_children term.block)
    )
