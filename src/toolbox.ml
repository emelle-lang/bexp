(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core_kernel
open Js_of_ocaml
open Types

let width t = t.toolbox_scrollbox#width

let set_palette toolbox palette =
  let p = Palette palette in
  toolbox.palette <- Some p;
  ignore
    (toolbox.toolbox_scrollbox#group#element##appendChild
       (palette.palette_root#element :> Dom.node Js.t));
  let rec loop (Palette palette) =
    palette.palette_toolbox <- Some toolbox;
    match palette.next_palette with
    | None -> ()
    | Some palette -> loop palette in
  loop p

let render toolbox =
  Option.iter toolbox.palette ~f:(fun ((Palette t) as palette) ->
      Palette.compute_dims palette;
      let height = Palette.render palette in
      toolbox.toolbox_scrollbox#group#set_width t.palette_group#width;
      toolbox.toolbox_scrollbox#group#set_height height;
      toolbox.toolbox_scrollbox#render
    )

let create ?x ?y ~width ~height  =
  let doc = Dom_svg.document in
  let scrollbox =
    new Widget.scrollbox ?x ?y ~width ~height doc in
  let toolbox =
    { toolbox_scrollbox = scrollbox
    ; palette = None } in
  scrollbox#set_on_scroll (fun () -> render toolbox);
  toolbox
