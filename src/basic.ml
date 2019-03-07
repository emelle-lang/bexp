open Core_kernel
open Js_of_ocaml

(** Throughout the code, the data types are parameterized by the ['symbols] type
    variable. This variable represents the set of {i symbols} in the language.
    The terminology of "symbol" is taken from the idea of a
    {{: https://en.wikipedia.org/wiki/Signature_(logic)} signature} from formal
    logic. A signature describing a logical system defines the set of (constant,
    function, and predicate) symbols used in it.

    Note that this library does not make a distinction between constant,
    function, and predicate symbols. A constant symbol is simply a function
    symbol with an arity of the unit type, and whether something is a predicate
    or function symbol is not something that the library is concerned with.
    (This library is for creating block interfaces, i.e. syntaxes, not
    semantics!) *)

type 'symbols ctx = {
    root_layer : Widget.group;
    script_container : Widget.group;
    mutable picked_up_block : 'symbols term option;
    scripts : 'symbols term Doubly_linked.t;
    mutable drop_candidate : 'symbols hole option;
  }

and 'symbols block = {
    group : Widget.group;
    items : 'symbols item list;
    mutable dim : (float * int) option;
      (** The cached width and column count. The column is an integer because it
          is counted in discrete intervals! *)
    mutable parent : 'symbols parent;
    ctx : 'symbols ctx;
    mutable iterator : 'symbols term Doubly_linked.Elt.t option
  }

and 'symbols parent =
  | Hole_parent of 'symbols hole
  | Picked_up
  | Root of 'symbols term Doubly_linked.Elt.t
  | Unattached

(** [term'] is parameterized by both the set of ['symbols] in the language and
    the ['sort] of the term that it wraps. The ['sort] type parameter enables
    the library to work with
    {{: https://en.wikipedia.org/wiki/Many-sorted_logic} many-sorted logics} in
    a type-safe manner. *)

and ('symbols, 'sort) term' = {
    block : 'symbols block;
    term : 'sort;
    symbol_of_term : ('symbols, 'sort) term' -> 'symbols;
      (** A conversion function that "packs" the wrapped term into the symbol
          type *)
  }

(** In order for terms of different sorts to be used together, the sort needs to
    abstracted away in an existential type variable. *)
and 'symbols term = Term : ('symbols, 'sort) term' -> 'symbols term

and ('symbols, 'sort) hole' = {
    mutable hole_term : ('symbols, 'sort) term' option;
    term_of_symbol : 'symbols -> ('symbols, 'sort) term' option;
      (** A conversion function that "unpacks" the symbol into a term belongin
          to a specific sort. *)
    hole_rect : Widget.rect;
    mutable hole_parent : 'symbols block option;
  }

and 'symbols hole = Hole : ('symbols, 'sort) hole' -> 'symbols hole

and 'symbols item =
  | Child of 'symbols hole
  | Newline
  | Widget of Widget.t (** A reference to a "keyword" *)
  | Tab

and ('symbols, 'sort) palette = {
    palette_group : Widget.group;
    syntactic_forms : ('symbols, 'sort) syntax list;
  }

and placeholder = {
    placeholder_group : Widget.group;
    text : Widget.text;
  }

and ('symbols, 'arity) syn_item =
  | Syn_Child of placeholder * ('arity -> 'symbols hole)
  | Syn_Newline
  | Syn_Widget of Widget.t * (unit -> Widget.t)
  | Syn_Tab

(** [('symbols, 'sort, 'arity) t]

    ['symbols] - The set of symbols in the language
    ['sort] - The set of terms (e.g. expr, type, kind)
    ['arity] - The type of the arity (e.g. type * type, term * term)
 *)
and ('symbols, 'sort, 'arity) syntax' = {
    syn_items : ('symbols, 'arity) syn_item list;
    syn_create : unit -> 'arity;
    term_of_arity : 'arity -> 'sort;
    syn_group : Widget.group
  }

and ('symbols, 'sort) syntax =
  Syntax : ('symbols, 'sort, _) syntax' -> ('symbols, 'sort) syntax

module Hole = struct
  let create term_of_symbol =
    let doc = Dom_svg.document in
    { hole_term = None
    ; term_of_symbol
    ; hole_rect =
        new Widget.rect
          ~width:18.0 ~height:18.0 ~rx:5.0 ~ry:5.0 ~style:"fill:#a0a0a0" doc
    ; hole_parent = None }

  let highlight hole =
    hole.hole_rect#set_style "fill:#ffffff"

  let highlight_error hole =
    hole.hole_rect#set_style "fill:#ffa0a0"

  let unhighlight hole =
    hole.hole_rect#set_style "fill:#a0a0a0"
end

module Block = struct
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
           | Child (Hole { hole_term; hole_rect; _ }) ->
              match hole_term with
              | None ->
                 hole_rect#set_x x;
                 hole_rect#set_y (Float.of_int y *. col_height);
                 x +. hole_rect#width +. horiz_padding, y
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
            remove_from_group parent term.block.group;
            append_to_group parent hole.hole_rect
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
                            hole.hole_rect#x hole.hole_rect#y
                            hole.hole_rect#width hole.hole_rect#height
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
    let svg_width = block.ctx.script_container#width in
    if block.group#x < 0.0 then
      block.group#set_x 0.0
    else if block.group#x +. block.group#width > svg_width then
      block.group#set_x (svg_width -. block.group#width)
    ;
    let svg_height = block.ctx.script_container#height in
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
    let f () =
      block.parent <- Root (Doubly_linked.insert_first block.ctx.scripts t);
      ignore
        (block.ctx.script_container#element##appendChild
           (block.group#element :> Dom.node Js.t))
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
       ignore
         (block.ctx.script_container#element##removeChild
            (block.group#element :> Dom.node Js.t));
       Doubly_linked.remove block.ctx.scripts iterator
    | _ -> ()
    end;
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

  let style = "fill:#ff0000; stroke-width:3; stroke:#ffffff"

  let create ?(x=0.0) ?(y=0.0) symbol_of_term ctx term items =
    let doc = Dom_svg.document in
    let block =
      { group = new Widget.group ~x ~y ~rx:5.0 ~ry:5.0 ~style doc
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
           | None -> append_to_group block hole.hole_rect
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
