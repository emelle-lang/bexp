open Core_kernel
open Js_of_ocaml

type 'sort ctx = {
    palette_rect : Dom_svg.rectElement Js.t;
    svg : Dom_svg.svgElement Js.t;
    mutable picked_up_block : 'sort term option;
    scripts : 'sort term Doubly_linked.t;
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
    mutable iterator : 'sort term Doubly_linked.Elt.t option
  }

and 'sort parent =
  | Hole_parent of 'sort hole
  | Picked_up
  | Root of 'sort term Doubly_linked.Elt.t
  | Unattached

and ('sort, 'term) term' = {
    block : 'sort block;
    term : 'term;
    sort_of_term : ('sort, 'term) term' -> 'sort;
  }

and 'sort term = Term : ('sort, 'term) term' -> 'sort term

and ('sort, 'term) hole' = {
    ptr : ('sort, 'term) term' option ref;
    term_of_sort : 'sort -> ('sort, 'term) term' option;
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
  let create term_of_sort doc =
    { ptr = ref None
    ; term_of_sort
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
              | Some term ->
                 term.block.group#set_x x;
                 term.block.group#set_y (Float.of_int y *. col_height);
                 let (dx, dy) = match term.block.dim with
                   | Some dim -> dim
                   | None -> render_helper term.block
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

  let render block =
    let rec render x y block =
      ignore (render_helper block);
      let x = x +. block.group#x in
      let y = y +. block.group#y in
      match block.parent with
      | Hole_parent (Hole hole) ->
         begin match hole.hole_parent with
         | Some parent -> render x y parent
         | None -> (x, y)
         end
      | _ -> (x, y)
    in render 0.0 0.0 block

  let clear hole =
    Option.iter !(hole.ptr) ~f:(fun term ->
        Option.iter hole.hole_parent ~f:(fun parent ->
            ignore (
                parent.group#element##removeChild
                  (term.block.group#element :> Dom.node Js.t));
            ignore (
                parent.group#element##appendChild
                  (hole.hole_svg#element :> Dom.node Js.t)
              )
          )
      );
    hole.ptr := None;
    match hole.hole_parent with
    | Some parent -> render parent
    | None -> (0.0, 0.0)

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
        else None
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
      Doubly_linked.fold block.ctx.scripts ~init:None
        ~f:(fun acc (Term t) ->
          match acc with
          | None -> get_hovered_hole t.block x y
          | some -> some
        ) in
    match hole with
    | None ->
       block.ctx.drop_candidate <- None
    | Some ((Hole hole) as h) ->
       block.ctx.drop_candidate <- Some h;
       Hole.highlight hole

  let drop ((Term term) as t) =
    let f () =
      term.block.parent <-
        Root (Doubly_linked.insert_first term.block.ctx.scripts t);
    in
    match term.block.ctx.drop_candidate with
    | None -> f ()
    | Some (Hole hole) ->
       match hole.term_of_sort (term.sort_of_term term) with
       | None -> f ()
       | Some term ->
          hole.ptr := Some term;
          term.block.parent <- Hole_parent (Hole hole);
          Option.iter hole.hole_parent ~f:(fun parent ->
              append_to_group parent term.block.group;
              ignore (render parent)
            )

  let pick_up ((Term term) as t) ev =
    let block = term.block in
    begin match block.parent with
    | Hole_parent (Hole hole) ->
       let (x, y) = clear hole in
       block.group#set_x (block.group#x +. x);
       block.group#set_y (block.group#y +. y);
       Option.iter hole.hole_parent ~f:(fun parent ->
           ignore (render parent)
         )
    | Root iterator ->
       Doubly_linked.remove block.ctx.scripts iterator
    | _ -> ()
    end;
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
          drop t;
          let pure_handler = Dom.handler (fun _ -> Js._true) in
          doc##.onmousemove := pure_handler;
          doc##.onmouseup := pure_handler;
          Js._true
        );
    block.parent <- Picked_up;
    ignore (block.ctx.svg##appendChild (block.group#element :> Dom.node Js.t));
    block.ctx.picked_up_block <- Some t;
    move_to_front block

  let create ?(x=0.0) ?(y=0.0) sort_of_term doc ctx term items =
    let block =
      { group = new Svg.group ~x ~y doc
      ; rect = new Svg.rect doc ~rx:5.0 ~ry:5.0 ~style:"fill:#ff0000"
      ; items
      ; dim = None
      ; parent = Unattached
      ; ctx
      ; iterator = None } in
    let term = { term; block; sort_of_term } in
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
          pick_up (Term term) ev;
          Dom_html.stopPropagation ev;
          Js._true
        );
    term
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

let add_block ctx term =
  term.block.parent <-
    Root (Doubly_linked.insert_first ctx.scripts (Term term));
  ignore (ctx.svg##appendChild (term.block.group#element :> Dom.node Js.t));
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Block.render term.block
