open Base
open Js_of_ocaml

type 'sort t
  = 'sort Block.item list
  -> Dom_svg.document Js.t
  -> 'sort Block.item list

let empty list _doc = list

let nt hole next list doc =
  next ((Block.Child hole)::list) doc

let text str next list doc =
  let text_elem = new Svg.text ~style:"fill:#ffffff" doc str in
  next ((Block.Svg (text_elem :> Svg.t))::list) doc

let eval t = t []
