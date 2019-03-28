open Core_kernel
open Js_of_ocaml
open Types

let col_height = 25.0

(* Returns the dimensions of the rendered block, which are used in its
   recursive calls when handling child blocks that have not been rendered yet
 *)
let rec render_block_and_children block : float * int =
  let horiz_padding = 4.0 in
  let rec loop max_width x y = function
    | [] -> max_width, y
    | item::items ->
       let x, y = match item with
         | Tab -> x +. 20.0, y
         | Newline -> horiz_padding, y + 1
         | Widget widget ->
            widget#set_x x;
            widget#set_y (Float.of_int y *. col_height);
            x +. widget#width +. horiz_padding, y
         | Child (Hole { hole_term; hole_placeholder; _ }) ->
            hole_placeholder.placeholder_group#set_x x;
            hole_placeholder.placeholder_group#set_y
              (Float.of_int y *. col_height);
            Hole.Placeholder.render hole_placeholder;
            match hole_term with
            | None ->
               x +. hole_placeholder.placeholder_group#width +. horiz_padding, y
            | Some term ->
               term.block.group#set_x x;
               term.block.group#set_y (Float.of_int y *. col_height);
               let (dx, dy) = match term.block.dim with
                 | Some dim -> dim
                 | None -> render_block_and_children term.block
               in (x +. dx +. horiz_padding, y + dy)
       in
       let max_width = Float.max max_width x in
       loop max_width x y items
  in
  let (width, newline_count) as dim =
    loop horiz_padding horiz_padding 0 block.items
  in
  block.group#set_width width;
  block.group#set_height (Float.of_int (newline_count + 1) *. col_height);
  block.dim <- Some dim;
  dim

(* Renders a block, then recursively renders its parents until reaching the
   root block. Returns the (x, y) offset from the original block, for use in
   converting from local to global coordinates.

   This function is used when picking up a block, where its ancestor blocks
   need to be re-rendered. The picked-up block needs to have its coordinates
   converted from relative to the parent block to relative to the editor, so
   this function does the work of accumulating the offset during the process
   of walking down the ancestors. *)
let render_block_and_parents block =
  let rec go x y block =
    ignore (render_block_and_children block);
    let x = x +. block.group#x in
    let y = y +. block.group#y in
    match block.parent with
    | Hole_parent (Hole hole) ->
       begin match hole.hole_parent with
       | Some parent -> go x y parent
       | None -> (x, y)
       end
    | _ -> (x, y)
  in go 0.0 0.0 block

let append_to_group block child =
  ignore (block.group#element##appendChild (child#element :> Dom.node Js.t))

let remove_from_group block child =
  ignore (block.group#element##removeChild (child#element :> Dom.node Js.t))

let clear hole =
  Option.iter hole.hole_term ~f:(fun term ->
      Option.iter hole.hole_parent ~f:(fun parent ->
          remove_from_group parent term.block.group
        )
    );
  hole.hole_term <- None;
  match hole.hole_parent with
  | Some parent -> render_block_and_parents parent
  | None -> (0.0, 0.0)

let move_to_front block =
  let elem = (block.group#element :> Dom.node Js.t) in
  Js.Opt.iter elem##.parentNode (fun parent ->
      ignore (parent##removeChild elem);
      ignore (parent##appendChild elem)
    )

(* Checks if coordinates are inside the box *)
let in_box x y box_x box_y box_w box_h =
  let open Float.O in
  x >= box_x && x < box_x + box_w && y >= box_y && y < box_y + box_h

(* Searches for a block hole that is under the given coordinates *)
let rec find_hovered_hole block x y =
  Option.bind block.dim ~f:(fun (width, block_cols) ->
      (* Are coordinates in block? *)
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
                  begin match hole.hole_term with
                  | Some term -> find_hovered_hole term.block x y
                  | None ->
                     if in_box x y
                          hole.hole_placeholder.placeholder_group#x
                          hole.hole_placeholder.placeholder_group#y
                          hole.hole_placeholder.placeholder_group#width
                          hole.hole_placeholder.placeholder_group#height
                     then Some h
                     else None
                  end
               | _ -> None
               end
            | some -> some
          )
      else None
    )

let check_bounds block =
  let open Float.O in
  let svg_width = block.ctx.root_layer#width in
  if block.group#x < 0.0 then
    block.group#set_x 0.0
  else if block.group#x +. block.group#width > svg_width then
    block.group#set_x (svg_width -. block.group#width)
  ;
  let svg_height = block.ctx.root_layer#height in
  if block.group#y < 0.0 then
    block.group#set_y 0.0
  else if block.group#y +. block.group#height > svg_height then
    block.group#set_y (svg_height -. block.group#height)

let drag term ev x_offset y_offset =
  let block = term.block in
  let x = (Float.of_int ev##.clientX -. x_offset) in
  let y = (Float.of_int ev##.clientY -. y_offset) in
  block.group#set_x x;
  block.group#set_y y;
  check_bounds block;
  Option.iter block.ctx.drop_candidate ~f:(fun (Hole hole) ->
      Hole.unhighlight hole
    );
  let hole =
    Doubly_linked.fold block.ctx.scripts ~init:None
      ~f:(fun acc (Term t) ->
        match acc with
        | None -> find_hovered_hole t.block x y
        | some -> some
      ) in
  match hole with
  | None ->
     block.ctx.drop_candidate <- None
  | Some ((Hole hole) as h) ->
     match hole.term_of_symbol (term.symbol_of_term term) with
     | None ->
        block.ctx.drop_candidate <- Some h;
        Hole.highlight_error hole
     | Some _ ->
        block.ctx.drop_candidate <- Some h;
        Hole.highlight hole

let drop ((Term term) as t) =
  let block = term.block in
  if block.group#x < block.ctx.toolbox.toolbox_group#width then (
    (* Block is hovering above toolbox, discard *)
    ignore (block.ctx.root_layer#element##removeChild
              (block.group#element :> Dom.node Js.t))
  ) else (
    let f () =
      block.parent <- Root (Doubly_linked.insert_first block.ctx.scripts t)
    in
    match block.ctx.drop_candidate with
    | None -> f ()
    | Some (Hole hole) ->
       match hole.term_of_symbol (term.symbol_of_term term) with
       | None ->
          f ();
          Hole.unhighlight hole
       | Some term ->
          hole.hole_term <- Some term;
          term.block.parent <- Hole_parent (Hole hole);
          Option.iter hole.hole_parent ~f:(fun parent ->
              append_to_group parent term.block.group;
              ignore (render_block_and_parents parent)
            )
  )

let begin_drag ((Term term) as t) ev =
  let block = term.block in
  let x_offset = Float.of_int ev##.clientX -. block.group#x in
  let y_offset = Float.of_int ev##.clientY -. block.group#y in
  let doc = Dom_html.document in
  doc##.onmousemove :=
    Dom.handler (fun ev ->
        drag term ev x_offset y_offset;
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
  ignore
    (block.ctx.root_layer#element##appendChild
       (block.group#element :> Dom.node Js.t));
  block.ctx.picked_up_block <- Some t;
  move_to_front block

let pick_up ((Term term) as t) ev =
  let block = term.block in
  begin match block.parent with
  | Hole_parent (Hole hole) ->
     let (x, y) = clear hole in
     block.group#set_x (block.group#x +. x);
     block.group#set_y (block.group#y +. y);
     Option.iter hole.hole_parent ~f:(fun parent ->
         ignore (render_block_and_parents parent)
       )
  | Root iterator ->
     Doubly_linked.remove block.ctx.scripts iterator
  | _ -> ()
  end;
  begin_drag t ev

let create ?(x=0.0) ?(y=0.0) symbol_of_term ctx term items =
  let doc = Dom_svg.document in
  let block =
    { group = new Widget.group ~x ~y ~rx:5.0 ~ry:5.0 doc
    ; items
    ; dim = None
    ; parent = Unattached
    ; ctx
    ; iterator = None } in
  let term = { term; block; symbol_of_term } in
  List.iter items ~f:(function
      | Widget widget -> append_to_group block widget
      | Child (Hole hole) ->
         hole.hole_parent <- Some block;
         begin match hole.hole_term with
         | None -> append_to_group block hole.hole_placeholder.placeholder_group
         | _ -> ()
         end
      | _ -> ()
    );
  ignore (
      Dom.addEventListener
        block.group#element
        (Dom_html.Event.mousedown)
        (Dom.handler (fun ev ->
             pick_up (Term term) ev;
             Dom_html.stopPropagation ev;
             Js._false
           )
        ) Js._false); (* If this is true, then parent gets picked up first *)
  term
