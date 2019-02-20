open Js_of_ocaml

type t = {
    palette_rect : Dom_svg.rectElement Js.t;
    svg : Dom_svg.svgElement Js.t;
  }

let create doc svg =
  let palette_rect = Dom_svg.createRect doc in
  let height : float = Svg.length_of_anim (svg##.height) in
  Svg.set_width palette_rect 50.0;
  Svg.set_height palette_rect height;
  let _ = svg##appendChild (palette_rect :> Dom.node Js.t) in
  { palette_rect; svg }

let add_block ctx block =
  ignore (ctx.svg##appendChild (block.Block.group#element :> Dom.node Js.t));
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Block.render block
