open Base
open Js_of_ocaml

type symbol =
  | Nonterminal of int
  | Terminal of Dom_svg.element Svg.t

type production = {
    symbols : symbol list;
  }

type t = {
    productions : production list;
  }

module Dsl = struct
  type t = symbol list -> Dom_svg.document Js.t -> symbol list

  let empty list _ = list

  let nt id next list doc =
    next ((Nonterminal id)::list) doc

  let text str next list doc =
    next ((Terminal (Svg.text doc str :> Dom_svg.element Svg.t))::list) doc

  let eval t = t []
end
