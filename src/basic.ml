open Core_kernel
open Js_of_ocaml

type 'sort ctx = {
    palette_rect : Dom_svg.rectElement Js.t;
    svg : Dom_svg.svgElement Js.t;
    mutable picked_up_block : 'sort block option;
    scripts : 'sort block Doubly_linked.t;
    mutable drop_candidate : 'sort hole option;
  }

and 'sort block = {
    group : Svg.group;
    rect : Svg.rect;
    items : 'sort item list;
    mutable dim : (float * int) option;
      (** The cached width and column count. The column is an integer because it
          is counted in discrete intervals! *)
    mutable parent : 'sort parent;
    ctx : 'sort ctx;
    mutable iterator : 'sort block Doubly_linked.Elt.t option
  }

and 'sort parent =
  | Hole_parent of 'sort hole
  | Root
  | Unattached

and ('sort, 'term) term = {
    block : 'sort block;
    term : 'term;
  }

and ('sort, 'term) hole' = {
    ptr : ('sort, 'term) term option ref;
    setter : ('sort, 'term) term option ref -> 'sort -> unit;
    hole_svg : Svg.rect;
    mutable hole_parent : 'sort block option;
  }

(** A hole is a GADT with the term type as an existential. Therefore, holes of
    differently sorted terms can be used together while retaining type safety.
 *)
and 'sort hole = Hole : ('sort, 'term) hole' -> 'sort hole

and 'sort item =
  | Child of 'sort hole
  | Newline
  | Svg of Svg.t (** A reference to a "keyword" *)
  | Tab

module Hole = struct
  let create setter _ctx doc =
    { ptr = ref None
    ; setter
    ; hole_svg =
        new Svg.rect
          ~width:20.0 ~height:20.0 ~rx:5.0 ~ry:5.0 ~style:"fill:#a0a0a0" doc
    ; hole_parent = None }

  let highlight hole =
    hole.hole_svg#set_style "fill:#ffffff"

  let unhighlight hole =
    hole.hole_svg#set_style "fill:#a0a0a0"
end

module Block = struct
  let col_height = 25.0

  let rec render_helper block : float * int =
    let rec loop max_width x y = function
      | [] -> max_width, y
      | item::items ->
         let x, y = match item with
           | Tab -> x +. 20.0, y
           | Newline -> 0.0, y + 1
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
                   | None -> render_helper block.block
                 in (x +. dx, y + dy)
         in
         let max_width = Float.max max_width x in
         loop max_width x y items
    in
    let (width, height) as dim = loop 0.0 0.0 0 block.items in
    block.rect#set_width width;
    block.rect#set_height (Float.of_int (height + 1) *. col_height);
    block.dim <- Some dim;
    dim

  let rec render block =
    ignore (render_helper block);
    match block.parent with
    | Hole_parent (Hole hole) ->
       Option.iter hole.hole_parent ~f:(fun block ->
           render block
         )
    | _ -> ()

  let append_to_group block child =
    ignore
      (block.group#element##appendChild (child#element :> Dom.node Js.t))

  let move_to_front block =
    let elem = (block.group#element :> Dom.node Js.t) in
    Js.Opt.iter elem##.parentNode (fun parent ->
        ignore (parent##removeChild elem);
        ignore (parent##appendChild elem)
      )

  let in_box x y box_x box_y box_w box_h =
    let open Float.O in
    x >= box_x && x < box_x + box_w && y >= box_y && y < box_y + box_h

  let rec get_hovered_hole block x y =
    Option.bind block.dim ~f:(fun (width, block_cols) ->
        if in_box x y block.group#x block.group#y
             width (Float.of_int (block_cols + 1) *. col_height) then
          (* Relativize coordinates *)
          let x = (x -. block.group#x) in
          let y = (y -. block.group#y) in
          List.fold block.items ~init:None ~f:(fun acc next ->
              match acc with
              | None ->
                 begin match next with
                 | Child ((Hole hole) as h) ->
                    begin match !(hole.ptr) with
                    | Some term -> get_hovered_hole term.block x y
                    | None ->
                       if in_box x y
                            hole.hole_svg#x hole.hole_svg#y
                            hole.hole_svg#width hole.hole_svg#height
                       then Some h
                       else None
                    end
                 | _ -> None
                 end
              | some -> some
            )
        else
          None
      )

  let drag block ev x_offset y_offset =
    let x = (Float.of_int ev##.clientX -. x_offset) in
    let y = (Float.of_int ev##.clientY -. y_offset) in
    block.group#set_x x;
    block.group#set_y y;
    Option.iter block.ctx.drop_candidate ~f:(fun (Hole hole) ->
        Hole.unhighlight hole
      );
    let hole =
      Doubly_linked.fold block.ctx.scripts ~init:None ~f:(fun acc next_block ->
          match acc with
          | None -> get_hovered_hole next_block x y
          | some -> some
        ) in
    match hole with
    | None ->
       block.ctx.drop_candidate <- None
    | Some ((Hole hole) as h) ->
       block.ctx.drop_candidate <- Some h;
       Hole.highlight hole

  let drop block =
    block.iterator <- Some (Doubly_linked.insert_first block.ctx.scripts block)

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
          drop block;
          let pure_handler = Dom.handler (fun _ -> Js._true) in
          doc##.onmousemove := pure_handler;
          doc##.onmouseup := pure_handler;
          Js._true
        );
    begin match block.parent with
    | Hole_parent (Hole hole) ->
       hole.ptr := None;
       Option.iter hole.hole_parent ~f:(fun parent ->
           ignore (render parent)
         )
    | _ -> ()
    end;
    block.parent <- Root;
    block.ctx.picked_up_block <- Some block;
    Option.iter block.iterator ~f:(fun elt ->
        Doubly_linked.remove block.ctx.scripts elt
      );
    block.iterator <- None;
    move_to_front block

  let create ?(x=0.0) ?(y=0.0) _sort_of_term doc ctx term items =
    let block =
      { group = new Svg.group ~x ~y doc
      ; rect = new Svg.rect doc ~rx:5.0 ~ry:5.0 ~style:"fill:#ff0000"
      ; items
      ; dim = None
      ; parent = Unattached
      ; ctx
      ; iterator = None } in
    append_to_group block block.rect;
    List.iter items ~f:(function
        | Svg svg -> append_to_group block svg
        | Child (Hole hole) ->
           hole.hole_parent <- Some block;
           begin match !(hole.ptr) with
           | None -> append_to_group block hole.hole_svg
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
end

module Builder = struct
  type 'sort t = Dom_svg.document Js.t -> 'sort item

  let nt hole _doc = Child(Hole hole)

  let text str doc =
    let text_elem = new Svg.text ~style:"fill:#ffffff" doc str in
    Svg (text_elem :> Svg.t)

  let newline _doc = Newline

  let tab _doc = Tab

  let eval doc ts = List.map ~f:(fun f -> f doc) ts
end

let create doc svg =
  let palette_rect = Dom_svg.createRect doc in
  let height : float = Svg.length_of_anim (svg##.height) in
  Svg.set_width palette_rect 50.0;
  Svg.set_height palette_rect height;
  let _ = svg##appendChild (palette_rect :> Dom.node Js.t) in
  { palette_rect
  ; svg
  ; picked_up_block = None
  ; scripts = Doubly_linked.create ()
  ; drop_candidate = None }

let add_block ctx block =
  block.parent <- Root;
  block.iterator <- Some (Doubly_linked.insert_first ctx.scripts block);
  ignore (ctx.svg##appendChild (block.group#element :> Dom.node Js.t));
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Block.render block
