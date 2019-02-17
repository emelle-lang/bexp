open Base
open Js_of_ocaml

type symbol =
  | Nonterminal of int
  | Terminal of Svg.t

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
    let text_elem = new Svg.text doc str in
    next ((Terminal (text_elem :> Svg.t))::list) doc

  let eval t = t []
end
