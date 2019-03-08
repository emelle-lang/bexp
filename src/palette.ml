open Base
open Js_of_ocaml
open Types

let max l r = if Float.compare l r = 1 then l else r

let rec render (Palette t) =
  let width, cols =
    List.fold t.syntactic_forms ~init:(t.palette_text#width, 1)
      ~f:(fun (width, cols) (Syntax syn) ->
        syn.syn_group#set_y (Float.of_int cols *. Block.col_height);
        let (width', cols') = Syntax.render syn in
        (max width width', cols + cols' + 1)
      ) in
  let t's_height = Float.of_int cols *. Block.col_height in
  let next's_width, next's_height =
    match t.next_palette with
    | None -> (0.0, 0.0)
    | Some next ->
       let Palette next' = next in
       let next's_width, next's_height = render next in
       next'.palette_group#set_y t's_height;
       next's_width, next's_height
  in
  let width = max next's_width width in
  let total_height = next's_height +. t's_height in
  t.palette_group#set_width width;
  t.palette_group#set_height total_height;
  width, total_height

let create toolbox next_palette palette_name syntactic_forms =
  let doc = Dom_svg.document in
  let width = toolbox.toolbox_group#width in
  let height = toolbox.toolbox_group#height in
  let style = "fill:#ababab" in
  let palette_text = new Widget.text doc palette_name in
  let palette_group = new Widget.group ~width ~height ~style doc in
  ignore
    (palette_group#element##appendChild
       (palette_text#element :> Dom.node Js.t));
  List.iter syntactic_forms ~f:(fun (Syntax syn) ->
      ignore
        (palette_group#element##appendChild
           (syn.syn_group#element :> Dom.node Js.t))
    );
  Option.iter next_palette ~f:(fun (Palette palette) ->
      ignore
        (palette_group#element##appendChild
           (palette.palette_group#element :> Dom.node Js.t))
    );
  { palette_text
  ; palette_group
  ; syntactic_forms
  ; next_palette }
