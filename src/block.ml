(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Js_of_ocaml
open Types

let col_height = 25.0

let rec render block : float * int =
  let rec loop max_width x y = function
    | [] -> max_width, y
    | item::items ->
       let x, y = match item with
         | Tab -> x +. 20.0, y
         | Newline -> 0.0, y + 1
         | Widget widget ->
            widget#set_x x;
            widget#set_y (Float.of_int y *. col_height);
            widget#render;
            x +. widget#width, y
         | Child (Hole { hole_term; hole_group; hole_placeholder; _ }) ->
            hole_group#set_x x;
            hole_group#set_y (Float.of_int y *. col_height);
            match hole_term with
            | None ->
               Hole.Placeholder.render hole_placeholder;
               x +. hole_placeholder.placeholder_group#width, y
            | Some term ->
               (* Check if dimensions have been cached *)
               let dx, dy = match term.block.dim with
                 | Some dim -> dim
                 | None -> render term.block
               in (x +. dx, y + dy)
       in
       let max_width = Float.max max_width x in
       loop max_width x y items
  in
  let (width, newline_count) as dim =
    loop 0.0 0.0 0 block.items
  in
  block.group#set_width width;
  block.group#set_height (Float.of_int (newline_count + 1) *. col_height);
  (* Cache dimensions *)
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
let rerender_root block =
  let rec go x y block =
    (* Invalidate cached dimensions *)
    block.dim <- None;
    match block.parent with
    | Hole_parent (Hole hole) ->
       let x = x +. hole.hole_group#x in
       let y = y +. hole.hole_group#y in
       begin match hole.hole_parent with
       | Some parent -> go x y parent
       | None -> (block, x, y)
       end
    | Root _ -> (block, block.group#x +. x, block.group#y +. y)
    | Picked_up | Unattached -> (block, x, y)
  in
  let root, x, y = go 0.0 0.0 block in
  ignore (render root);
  x, y

let append_to_group block child =
  ignore (block.group#element##appendChild (child#element :> Dom.node Js.t))

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
let rec find_hovered_hole block block_x block_y x y =
  Option.bind block.dim ~f:(fun (width, block_cols) ->
      (* Are coordinates in block? *)
      if in_box x y block_x block_y
           width (Float.of_int (block_cols + 1) *. col_height) then
        List.fold block.items ~init:None ~f:(fun acc next ->
            match acc with
            | None ->
               begin match next with
               | Child ((Hole hole) as h) ->
                  begin match hole.hole_term with
                  | Some term ->
                     find_hovered_hole term.block
                       (block_x +. hole.hole_group#x)
                       (block_y +. hole.hole_group#y) x y
                  | None ->
                     if in_box x y
                          (block_x +. hole.hole_group#x)
                          (block_y +. hole.hole_group#y)
                          hole.hole_placeholder.placeholder_group#width
                          hole.hole_placeholder.placeholder_group#height then
                       Some h
                     else None
                  end
               | _ -> None
               end
            | some -> some
          )
      else None
    )

let to_local_coords block =
  let ctx = block.ctx in
  let group = ctx.script_scrollbox#group in
  ( block.group#x -. ctx.script_scrollbox#x -. group#x
  , block.group#y -. ctx.script_scrollbox#y -. group#y )

let to_global_coords block =
  let ctx = block.ctx in
  let group = ctx.script_scrollbox#group in
  ( block.group#x +. ctx.script_scrollbox#x +. group#x
  , block.group#y +. ctx.script_scrollbox#y +. group#y )

let check_bounds block =
  let open Float.O in
  let script_group = block.ctx.script_scrollbox#group in
  let group_width = script_group#width in
  if block.group#x < 0.0 then
    block.group#set_x 0.0
  else
    let sum = block.group#x +. block.group#width +. 20.0 in
    if sum > group_width then (
      script_group#set_width sum;
      block.ctx.script_scrollbox#render
    )
  ;
  let group_height = script_group#height in
  if block.group#y < 0.0 then
    block.group#set_y 0.0
  else
    let sum = block.group#y +. block.group#height +. 20.0 in
    if sum > group_height then (
      script_group#set_height sum;
      block.ctx.script_scrollbox#render
    )

let drag term ev x_offset y_offset =
  let block = term.block in
  let x = (Float.of_int ev##.clientX -. x_offset) in
  let y = (Float.of_int ev##.clientY -. y_offset) in
  block.group#set_x x;
  block.group#set_y y;
  check_bounds block;
  let x, y = to_local_coords block in
  Option.iter block.ctx.drop_candidate ~f:(fun (Hole hole) ->
      Hole.unhighlight hole
    );
  let hole =
    let (Hole hole) as h = block.ctx.entry_exists_hole in
    let opt = match hole.hole_term with
      | None ->
         if in_box x y
              hole.hole_group#x
              hole.hole_group#y
              hole.hole_placeholder.placeholder_group#width
              hole.hole_placeholder.placeholder_group#height then
           Some h
         else
           None
      | Some term ->
         find_hovered_hole term.block hole.hole_group#x hole.hole_group#y x y
    in
    Doubly_linked.fold block.ctx.scripts ~init:opt
      ~f:(fun acc (Term t) ->
        match acc with
        | None ->
           find_hovered_hole t.block t.block.group#x t.block.group#y x y
        | some -> some
      ) in
  match hole with
  | None ->
     block.ctx.drop_candidate <- None
  | Some (Hole hole) as candidate ->
     match hole.term_of_symbol (term.symbol_of_term term) with
     | None ->
        block.ctx.drop_candidate <- candidate;
        Hole.highlight_reject hole
     | Some _ ->
        block.ctx.drop_candidate <- candidate;
        Hole.highlight_accept hole

let rec listen_to_drags (Term term) =
  ignore (
      Dom.addEventListener
        term.block.group#element
        Dom_html.Event.mousedown
        (Dom.handler (fun ev ->
             term.block.group#element##.onmousemove :=
               Dom.handler (fun ev ->
                   term.block.group#element##.onmousemove :=
                     (Dom.handler (fun _ -> Js._false));
                   pick_up (Term term) ev;
                   Js._false
                 );
             (* Once the mouse is picked up, stop listening to drags *)
             term.block.group#element##.onmouseup :=
               Dom.handler (fun _ev ->
                   term.block.group#element##.onmousemove :=
                     (Dom.handler (fun _ -> Js._false));
                   Js._false
                 );
             Dom_html.stopPropagation ev; (* Important! *)
             Js._false)
        ) Js._false (* If this is true, then parent gets picked up first *)
    );

and drop ((Term term) as t) =
  let open Float.O in
  listen_to_drags t;
  let block = term.block in
  let x, y = to_local_coords block in
  if x < 0.0 then
    (* Block is hovering above toolbox, discard *)
    ignore (block.ctx.root_layer#element##removeChild
              (block.group#element :> Dom.node Js.t))
  else
    let f () =
      block.group#set_x x;
      block.group#set_y y;
      ignore (block.ctx.script_scrollbox#group#element##appendChild
        (block.group#element :> Dom.node Js.t));
      block.parent <- Root (Doubly_linked.insert_first block.ctx.scripts t)
    in
    match block.ctx.drop_candidate with
    | None -> f ()
    | Some (Hole hole) ->
       Hole.set_term hole term ~none:f ~some:(fun term ->
           term.block.group#set_x 0.0;
           term.block.group#set_y 0.0;
           ignore (rerender_root term.block)
         )

and begin_drag ((Term term) as t) ev =
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

and pick_up ((Term term) as t) ev =
  let block = term.block in
  begin match block.parent with
  | Hole_parent (Hole hole) ->
     Hole.clear hole;
     let x, y = match hole.hole_parent with
       | Some parent -> rerender_root parent
       | None -> 0.0, 0.0
     in
     block.group#set_x (hole.hole_group#x +. x);
     block.group#set_y (hole.hole_group#y +.y);
     let x, y =
       to_global_coords block
     in
     block.group#set_x x;
     block.group#set_y y
  | Root iterator ->
     ignore (block.ctx.script_scrollbox#group#element##removeChild
               (block.group#element :> Dom.node Js.t));
     let x, y = to_global_coords block in
     block.group#set_x x;
     block.group#set_y y;
     ignore (block.ctx.root_layer#element##appendChild
               (block.group#element :> Dom.node Js.t));
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
         | None -> append_to_group block hole.hole_group
         | _ -> ()
         end
      | _ -> ()
    );
  listen_to_drags (Term term);
  term
