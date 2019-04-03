(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
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

let create term_of_symbol palette_data =
  let placeholder = Placeholder.create palette_data in
  Placeholder.render placeholder;
  { hole_term = None
  ; term_of_symbol
  ; hole_placeholder = placeholder
  ; hole_parent = None }

let highlight hole =
  hole.hole_placeholder.placeholder_group#rect_style##.stroke :=
    Js.string "green"

let highlight_error hole =
  hole.hole_placeholder.placeholder_group#rect_style##.stroke :=
    Js.string "#ffa0a0"

let unhighlight hole =
  hole.hole_placeholder.placeholder_group#rect_style##.stroke :=
    Js.string "white"
