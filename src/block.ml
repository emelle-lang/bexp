open Base
open Js_of_ocaml

(** The [block] type is factored out of the [t] type so that it doesn't need a
    type parameter for the term, which is irrelevant. *)
type 'sort block = {
    group : Svg.group;
    rect : Svg.rect;
    items : 'sort item list;
    mutable dim : (float * int) option;
      (** The cached width and column count. The column is an integer because it
          is counted in discrete intervals! *)
  }

and ('sort, 'term) t = {
    block : 'sort block;
    term : 'term;
  }

and ('sort, 'term) hole' = {
    ptr : ('sort, 'term) t option ref;
    setter : ('sort, 'term) t option ref -> 'sort -> unit;
    hole_svg : Svg.rect;
  }

(** A hole is a GADT with the term type as an existential. Therefore, holes of
    differently sorted terms can be used together while retaining type safety.
 *)
and 'sort hole = Hole : ('sort, 'term) hole' -> 'sort hole

and 'sort item =
  | Child of 'sort hole
  | Newline
  | Svg of Svg.t (** A reference to a "keyword" *)

let col_height = 25.0

let create_hole setter doc =
  { ptr = ref None
  ; setter
  ; hole_svg =
      new Svg.rect
        ~width:20.0 ~height:20.0 ~rx:5.0 ~ry:5.0 ~style:"fill:#a0a0a0" doc }

let rec render block : float * int =
  let rec loop x y = function
    | [] -> x, y
    | item::items ->
       let x, y = match item with
         | Newline ->
            0.0, y + 1
         | Svg svg ->
            svg#set_x x;
            svg#set_y (Float.of_int y *. col_height);
            x +. svg#width, y
         | Child (Hole { ptr; hole_svg;_ }) ->
            match !ptr with
            | None ->
               hole_svg#set_x x;
               hole_svg#set_y (Float.of_int y *. col_height);
               x +. hole_svg#width, y
            | Some block ->
               let (dx, dy) = match block.block.dim with
                 | Some dim -> dim
                 | None -> render block.block
               in (x +. dx, y + dy)
       in loop x y items
  in
  let (width, height) as dim = loop 0.0 0 block.items in
  block.rect#set_width width;
  block.rect#set_height (Float.of_int (height + 1) *. col_height);
  block.dim <- Some dim;
  dim

let append_to_group block child =
  ignore (block.group#element##appendChild (child#element :> Dom.node Js.t))

let drag block ev x_offset y_offset =
  block.group#set_x (Float.of_int ev##.clientX -. x_offset);
  block.group#set_y (Float.of_int ev##.clientY -. y_offset)

let pick_up block ev =
  let x_offset = Float.of_int ev##.clientX -. block.group#x in
  let y_offset = Float.of_int ev##.clientY -. block.group#y in
  let doc = Dom_html.document in
  doc##.onmousemove :=
    Dom.handler (fun ev ->
        drag block ev x_offset y_offset;
        Js._true
      );
  doc##.onmouseup :=
    Dom.handler (fun _ ->
        let pure_handler = Dom.handler (fun _ -> Js._true) in
        doc##.onmousemove := pure_handler;
        doc##.onmouseup := pure_handler;
        Js._true
      )

let create ?(x=0.0) ?(y=0.0) doc term items =
  let block =
    { group = new Svg.group ~x ~y doc
    ; rect = new Svg.rect doc ~height:50.0 ~rx:5.0 ~ry:5.0 ~style:"fill:#ff0000"
    ; items
    ; dim = None } in
  append_to_group block block.rect;
  List.iter items ~f:(function
      | Svg svg -> append_to_group block svg
      | Child (Hole hole) ->
         begin match !(hole.ptr) with
         | None ->
            append_to_group block hole.hole_svg
         | _ -> ()
         end
      | _ -> ()
    );
  block.group#element##.onmousedown :=
    Dom.handler (fun ev ->
        pick_up block ev;
        Js._true
      );
  { term; block }
