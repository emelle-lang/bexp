(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core_kernel
open Js_of_ocaml
open Svg

type t = {
    svg_doc : Dom_svg.document Js.t;
    container : Dom_html.element Js.t;
    svg : Dom_svg.svgElement Js.t;
    defs : Dom_svg.defsElement Js.t;
    mutable id_gen : int;
  }

let create container =
  let doc = Dom_svg.document in
  let svg = Dom_svg.createSvg doc in
  ignore (container##appendChild (svg :> Dom.node Js.t));
  let width = container##.offsetWidth in
  let height = container##.offsetHeight in
  Svg.set_string_prop svg "width" (Int.to_string width ^ "px");
  Svg.set_string_prop svg "height" (Int.to_string height ^ "px");
  Svg.set_string_prop svg "style" "display: block";
  let defs = Dom_svg.createDefs doc in
  ignore (svg##appendChild (defs :> Dom.node Js.t));
  { svg_doc = doc
  ; container
  ; svg
  ; defs
  ; id_gen = 0 }

let width t = t.container##.clientWidth

let height t = t.container##.clientHeight

(* https://stackoverflow.com/questions/26049488/how-to-get-absolute-coordinates-of-object-inside-a-g-group *)
let local_to_abs t elem x y =
  let offset = t.container##getBoundingClientRect in
  let matrix = elem##getScreenCTM in
  ( matrix##.a *. x +. matrix##.c *. y -. offset##.left
  , matrix##.b *. x +. matrix##.d *. y -. offset##.top )

let create_clip_path t elem =
  let clip_path =
    (* Dom_svg.createClipPath has a bug *)
    (Js.Unsafe.coerce (Dom_svg.createElement t.svg_doc "clipPath")
     :> Dom_svg.clipPathElement Js.t) in
  let id = "path-" ^ Int.to_string t.id_gen in
  set_string_prop clip_path "id" id;
  t.id_gen <- t.id_gen + 1;
  ignore (clip_path##appendChild (elem :> Dom.node Js.t));
  ignore (t.defs##appendChild (clip_path :> Dom.node Js.t));
  "url(#" ^ id ^ ")"

let create_clip_rect t ~x ~y ~width ~height =
  let rect = Dom_svg.createRect t.svg_doc in
  set_x rect x;
  set_y rect y;
  set_width rect width;
  set_height rect height;
  (create_clip_path t rect, rect)

let clip_elem t elem ~width ~height =
  let path, rect =
    create_clip_rect t ~x:0.0 ~y:0.0 ~width ~height in
  set_string_prop elem "clip-path" path;
  rect
