open Js_of_ocaml

type t = {
    palette_rect : Dom_svg.rectElement Js.t;
    svg : Dom_svg.svgElement Js.t;
    grammar : Grammar.t;
  }

let create grammar doc svg =
  let palette_rect = Dom_svg.createRect doc in
  let height : float = Svg.length_of_anim (svg##.height) in
  Svg.set_width palette_rect 50.0;
  Svg.set_height palette_rect height;
  let _ = svg##appendChild (palette_rect :> Dom.node Js.t) in
  { palette_rect; grammar; svg }
