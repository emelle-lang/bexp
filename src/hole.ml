open Js_of_ocaml
open Types

let create term_of_symbol =
  let doc = Dom_svg.document in
  let rect = new Widget.rect ~width:18.0 ~height:18.0 ~rx:5.0 ~ry:5.0 doc in
  rect#element##.style##.fill := Js.string "#a0a0a0";
  { hole_term = None
  ; term_of_symbol
  ; hole_rect = rect
  ; hole_parent = None }

let highlight hole =
  hole.hole_rect#element##.style##.fill := Js.string "#ffffff"

let highlight_error hole =
  hole.hole_rect#element##.style##.fill := Js.string "#ffa0a0"

let unhighlight hole =
  hole.hole_rect#element##.style##.fill := Js.string "#a0a0a0"
