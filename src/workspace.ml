open Core_kernel
open Js_of_ocaml
open Types

let create ?(x=0.0) ?y ~width ~height =
  let doc = Dom_svg.document in
  let style = "fill:grey" in
  let root_layer = new Widget.group ~x ?y ~width ~height ~style doc in
  let toolbox = Toolbox.create ~x ?y ~width:150.0 ~height in
  ignore (root_layer#element##appendChild
            (toolbox.toolbox_group#element :> Dom.node Js.t));
  { root_layer
  ; picked_up_block = None
  ; scripts = Doubly_linked.create ()
  ; drop_candidate = None
  ; toolbox }

let add_block ctx term =
  term.block.parent <-
    Root (Doubly_linked.insert_first ctx.scripts (Term term));
  ignore
    (ctx.root_layer#element##appendChild
       (term.block.group#element :> Dom.node Js.t))

let render ctx =
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Toolbox.render ctx.toolbox;
  Doubly_linked.iter ctx.scripts ~f:(fun (Term term) ->
      ignore (Block.render_block_and_children term.block)
    )

let create_syntax ~create ~to_term items ~symbol_of_term workspace =
  let doc = Dom_svg.document in
  let group = new Widget.group ~rx:5.0 ~ry:5.0 ~style:(Block.style) doc in
  List.iter items ~f:(function
      | Syn_Child(hole, _) ->
         ignore
           (group#element##appendChild
              (hole.placeholder_group#element :> Dom.node Js.t))
      | Syn_Widget(w, _) ->
         ignore (group#element##appendChild (w#element :> Dom.node Js.t))
      | _ -> ()
    );
  let syntax =
    { syn_items = items
    ; syn_create = create
    ; term_of_arity = to_term
    ; syn_group = group } in
  group#element##.onmousedown :=
    Dom.handler (fun ev ->
        let x = Float.of_int ev##.clientX in
        let y = Float.of_int ev##.clientY in
        let term = Syntax.run symbol_of_term ~x ~y workspace syntax in
        ignore
          (workspace.root_layer#element##appendChild
             (term.block.group#element :> Dom.node Js.t));
        Block.begin_drag (Term term) ev;
        ignore (Block.render_block_and_children term.block);
        Js._false
      );
  syntax
