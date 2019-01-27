open Js_of_ocaml

module type S = sig
  module Block : Block.S

  type t = {
      palette_rect : Dom_svg.rectElement Js.t;
      svg : Dom_svg.svgElement Js.t;
      syntax : Block.Concrete_Syntax.t;
    }

  val create :
    Block.Concrete_Syntax.t
    -> Dom_svg.document Js.t
    -> Dom_svg.svgElement Js.t
    -> t
end

module Make (Abs_Syn : Syntax.Abstract)
       : S with module Block = Block.Make(Abs_Syn) = struct
  module Block = Block.Make(Abs_Syn)

  type t = {
      palette_rect : Dom_svg.rectElement Js.t;
      svg : Dom_svg.svgElement Js.t;
      syntax : Block.Concrete_Syntax.t;
    }

  let create syntax doc svg =
    let palette_rect = Dom_svg.createRect doc in
    let height : float = Svg.length_of_anim (svg##.height) in
    Svg.set_width palette_rect 50.0;
    Svg.set_height palette_rect height;
    let _ = svg##appendChild (palette_rect :> Dom.node Js.t) in
    { palette_rect; syntax; svg }
end
