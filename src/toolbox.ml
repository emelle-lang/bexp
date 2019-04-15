(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Core_kernel
open Js_of_ocaml
open Types

let create ?x ?y ~width ~height  =
  let doc = Dom_svg.document in
  let group = new Widget.group ?x ?y ~width ~height doc in
  { toolbox_group = group
  ; palette = None }

let set_palette toolbox palette =
  let p = Palette palette in
  toolbox.palette <- Some p;
  ignore
    (toolbox.toolbox_group#element##appendChild
       (palette.palette_root#element :> Dom.node Js.t))

let render toolbox =
  Option.iter toolbox.palette ~f:(fun ((Palette t) as palette) ->
      Palette.compute_dims palette;
      Palette.render palette;
      toolbox.toolbox_group#set_width t.palette_group#width
    )
