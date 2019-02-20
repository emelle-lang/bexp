open Base
open Js_of_ocaml

type ('sort, 'term) t = {
    group : Svg.group;
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

let render block =
  let rec loop x = function
    | [] -> x
    | item::items ->
       let x = match item with
         | Svg svg ->
            svg#set_x x;
            x +. svg#width
         | _ -> x
       in loop x items
  in
  let width = loop 0.0 block.items in
  block.rect#set_width width

let append_to_group block child =
  ignore (block.group#element##appendChild (child#element :> Dom.node Js.t))

let create doc term items =
  let block =
    { group = new Svg.group ~x:5.0 ~y:5.0 doc
    ; rect = new Svg.rect doc
               ~height:50.0 ~rx:5.0 ~ry:5.0 ~style:"fill:rgb(255, 0, 0)"
    ; term
    ; items } in
  append_to_group block block.rect;
  List.iter items ~f:(function
      | Svg svg -> append_to_group block svg
      | _ -> ()
    );
  block
