open Base
open Js_of_ocaml

(** The product types of the [Bool], [Float], [Int], and [String] tags are the
    default values. *)
type 'sort hole =
  | Function_symbol of 'sort
    (** See https://en.wikipedia.org/wiki/Functional_predicate,
        https://en.wikipedia.org/wiki/Uninterpreted_function, and
        https://en.wikipedia.org/wiki/Signature_(logic) for the terminology
        of "function symbol." *)
  | Bool of bool
  | Float of float
  | Int of int
  | String of string

(** The abstract syntax of a language *)
module type Abstract = sig
  (** A (function) symbol is an expression in the language. Terms are
      *instances* of symbols; that is, symbols combine to form terms. *)
  type symbol

  (** Each symbol has a "sort." For example, symbol sorts in ML might be expr,
      pat, and type. Think of a "sort" as a hole shape in Scratch / Snap!. See
      https://en.wikipedia.org/wiki/Many-sorted_logic for where I got the
      terminology. *)
  type sort

  val is : symbol -> sort

  val at : symbol -> int -> sort hole option

  val enum_symbols : ('a -> symbol -> 'a) -> 'a -> 'a
end

(** The concrete syntax of a language, AKA its formal grammar. *)
module type Concrete = sig
  module Abs : Abstract

  type symbol =
    | Nonterminal of int (** A reference to a function symbol's hole *)
    | Terminal of Dom_svg.element Svg.t (** A "keyword" in the language *)

  type production = {
      function_symbol : Abs.symbol;
      symbols : symbol list;
    }

  type t = {
      productions : production list;
    }

  module Dsl : sig
    type t

    val empty : t

    val nt : int -> t -> t

    val text : string -> t -> t

    val eval : t -> Dom_svg.document Js.t -> symbol list
  end
end

module Concrete (Abs : Abstract) : Concrete with module Abs = Abs = struct
  module Abs = Abs

  type symbol =
    | Nonterminal of int
    | Terminal of Dom_svg.element Svg.t

  type production = {
      function_symbol : Abs.symbol;
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
end
