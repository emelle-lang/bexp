open Base

type ('sort, 'term) t = {
    rect : Svg.rect;
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
  | Newline
  | Svg of Svg.t (** A reference to a "keyword" *)
