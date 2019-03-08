open Js_of_ocaml
open Types

let create term_of_symbol =
  let doc = Dom_svg.document in
  { hole_term = None
  ; term_of_symbol
  ; hole_rect =
      new Widget.rect
        ~width:18.0 ~height:18.0 ~rx:5.0 ~ry:5.0 ~style:"fill:#a0a0a0" doc
  ; hole_parent = None }

let highlight hole =
  hole.hole_rect#set_style "fill:#ffffff"

let highlight_error hole =
  hole.hole_rect#set_style "fill:#ffa0a0"

let unhighlight hole =
  hole.hole_rect#set_style "fill:#a0a0a0"
