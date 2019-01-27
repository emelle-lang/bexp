open Base
open Js_of_ocaml

type item =
  | Child of int (** A reference to a child block *)
  | Svg of Dom_svg.useElement Svg.t (** A reference to a "keyword" *)

type items = {
    kind : item;
    next : items option;
  }

module type S = sig
  module Abstract_Syntax : Syntax.Abstract
  module Concrete_Syntax : Syntax.Concrete with module Abs = Abstract_Syntax

  type t = {
      children : t Option_array.t;
      production : Concrete_Syntax.production;
      items : items;
      x : int;
      y : int;
    }
end

module Make (Abs_Syn : Syntax.Abstract)
       : S with module Abstract_Syntax = Abs_Syn = struct
  module Abstract_Syntax = Abs_Syn
  module Concrete_Syntax = Syntax.Concrete(Abstract_Syntax)

  type t = {
      children : t Option_array.t;
      production : Concrete_Syntax.production;
      items : items;
      x : int;
      y : int;
    }
end
