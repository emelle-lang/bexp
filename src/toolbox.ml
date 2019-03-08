open Core_kernel
open Js_of_ocaml
open Types

let create ?x ?y ~width ~height  =
  let doc = Dom_svg.document in
  let group = new Widget.group ?x ?y ~width ~height doc in
  { toolbox_group = group
  ; palettes = Queue.create ()
  ; palette = None }

let create_palette toolbox syntactic_forms =
  let doc = Dom_svg.document in
  let width = toolbox.toolbox_group#width in
  let height = toolbox.toolbox_group#height in
  let style = "fill:#ababab" in
  let palette_group = new Widget.group ~width ~height ~style doc in
  List.iter syntactic_forms ~f:(fun (Syntax syn) ->
      ignore
        (palette_group#element##appendChild
           (syn.syn_group#element :> Dom.node Js.t))
    );
  let palette =
    { palette_group
    ; syntactic_forms }
  in
  palette

let set_palette toolbox palette =
  toolbox.palette <- Some (Palette palette);
  ignore
    (toolbox.toolbox_group#element##appendChild
       (palette.palette_group#element :> Dom.node Js.t))

let render toolbox =
  Option.iter toolbox.palette ~f:(fun (Palette palette) ->
      Palette.render palette;
    )
