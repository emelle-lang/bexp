open Core_kernel
open Js_of_ocaml
open Basic

let create ?x ?y ~width ~height () =
  let doc = Dom_svg.document in
  let style = "fill:grey" in
  let root_layer = new Widget.group ?x ?y ~width ~height ~style doc in
  let script_container = new Widget.group ?x ?y ~width ~height ~style doc in
  ignore
    (root_layer#element##appendChild
       (script_container#element :> Dom.node Js.t));
  { root_layer
  ; script_container
  ; picked_up_block = None
  ; scripts = Doubly_linked.create ()
  ; drop_candidate = None }

let add_block ctx term =
  term.block.parent <-
    Root (Doubly_linked.insert_first ctx.scripts (Term term));
  ignore
    (ctx.script_container#element##appendChild
       (term.block.group#element :> Dom.node Js.t))

let render ctx =
  (* Render the block upon entry into DOM rather than construction so that
     text.getComputedTextLength() works correctly *)
  Doubly_linked.iter ctx.scripts ~f:(fun (Term term) ->
      ignore (Block.render_block_and_children term.block)
    )
