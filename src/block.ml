open Base
open Js_of_ocaml

type ('sort, 'term) t = {
    term : 'term;
    items : 'sort item list;
  }

and 'sort hole =
  | Hole
    :  ('sort, 'term) t option ref
    *  (('sort, 'term) t option ref -> 'sort -> unit)
    -> 'sort hole

and 'sort item =
  | Child of 'sort hole
  | Svg of Dom_svg.useElement Svg.t (** A reference to a "keyword" *)
