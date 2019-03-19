open Core_kernel
open Js_of_ocaml
open Types

let create ?(x=0.0) ?y ~width ~height =
  let doc = Dom_svg.document in
  let root_layer = new Widget.group ~x ?y ~width ~height doc in
  let toolbox = Toolbox.create ~x ?y ~width:150.0 ~height in
  root_layer#rect_style##.fill := Js.string "grey";
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
