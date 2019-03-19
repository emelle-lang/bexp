open Base
open Js_of_ocaml
open Types

let rec render (Palette t) =
  let width, cols =
    List.fold t.syntactic_forms ~init:(t.palette_text#width, 1)
      ~f:(fun (width, cols) (Syntax syn) ->
        syn.syn_group#set_y (Float.of_int cols *. Block.col_height);
        let (width', cols') = Syntax.render syn in
        (Float.max width width', cols + cols' + 1)
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
  let width = Float.max next's_width width in
  let total_height = next's_height +. t's_height in
  t.palette_group#set_width width;
  t.palette_group#set_height total_height;
  width, total_height

let create
      workspace next_palette palette_name
      ?(palette_style = fun ~g's_style:_ ~rect's_style:_ -> ())
      syntactic_forms =
  let set_style ~g's_style ~rect's_style =
    (* Set defaults *)
    rect's_style##.fill := Js.string "red";
    rect's_style##.strokeWidth := Js.string "3";
    rect's_style##.stroke := Js.string "white";
    g's_style##.fill := Js.string "white";
    g's_style##.fontFamily := Js.string "sans-serif";
    (* Call user-supplied function *)
    palette_style ~g's_style ~rect's_style
  in
  let toolbox = workspace.toolbox in
  let doc = Dom_svg.document in
  let width = toolbox.toolbox_group#width in
  let height = toolbox.toolbox_group#height in
  let palette_text = new Widget.text doc palette_name in
  let palette_group = new Widget.group ~width ~height doc in
  palette_group#rect_style##.fill := Js.string "#ababab";
  palette_group#add_child (palette_text :> Widget.t);
  List.iter syntactic_forms ~f:(fun (Syntax syn) ->
      set_style
        ~g's_style:syn.syn_group#element##.style
                     ~rect's_style:syn.syn_group#rect_style;
      syn.syn_group#element##.onmousedown :=
        Dom.handler (fun ev ->
            begin match
              Js.Optdef.to_option ev##.pageX, Js.Optdef.to_option ev##.pageY
            with
            | Some x, Some y ->
               let x = Float.of_int x in
               let y = Float.of_int y in
               let term =
                 Syntax.run syn.symbol_of_term_template ~x ~y workspace syn in
               set_style
                 ~g's_style:term.block.group#element##.style
                              ~rect's_style:term.block.group#rect_style;
               ignore
                 (workspace.root_layer#element##appendChild
                    (term.block.group#element :> Dom.node Js.t));
               Block.begin_drag (Term term) ev;
               ignore (Block.render_block_and_children term.block)
            | _ -> failwith "Unreachable"
            end;
            Js._false
          );
      palette_group#add_child (syn.syn_group :> Widget.t)
    );
  Option.iter next_palette ~f:(fun (Palette palette) ->
      palette_group#add_child (palette.palette_group :> Widget.t)
    );
  { palette_text
  ; palette_group
  ; palette_style
  ; syntactic_forms
  ; next_palette }
