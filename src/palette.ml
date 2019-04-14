(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Js_of_ocaml
open Types

let bar_height = 20.0

let render ((Palette t) as palette) =
  let rec f acc (Palette t) =
    t.palette_y_offset <- acc;
    let width, cols =
      List.fold t.syntactic_forms ~init:(t.palette_text#width, 1)
        ~f:(fun (width, cols) (Syntax syn) ->
          syn.syn_group#set_y (Float.of_int cols *. Block.col_height);
          let (width', cols') = Syntax.render syn in
          (Float.max width width', cols + cols' + 1)
        ) in
    let t's_height =
      if t.palette_collapsed then (
        t.palette_group#hide;
        bar_height
      ) else (
        t.palette_group#show;
        bar_height +. Float.of_int cols *. Block.col_height
      ) in
    let next's_width, next's_height =
      match t.next_palette with
      | None -> 0.0, 0.0
      | Some next ->
         let Palette next' = next in
         let next's_width, next's_height = f (acc +. t's_height) next in
         next'.palette_root#set_y t's_height;
         next's_width, next's_height
    in
    let width = Float.max next's_width width in
    t.palette_group#set_height t's_height;
    t.palette_root#set_height t's_height;
    width, t's_height +. next's_height
  in
  let width, _ = f t.palette_y_offset palette in
  let rec f (Palette t) =
    t.palette_root#set_width width;
    t.palette_bar#set_width width;
    t.palette_group#set_width width;
    match t.next_palette with
    | None -> ()
    | Some p -> f p
  in f palette

let create workspace next_palette palette_data syntactic_forms =
  let toolbox = workspace.toolbox in
  let doc = Dom_svg.document in
  let width = toolbox.toolbox_group#width in
  let height = toolbox.toolbox_group#height in

  let palette_text = new Widget.text doc palette_data.palette_name in
  palette_text#element##.style##.fill := Js.string "white";
  palette_text#element##.style##.fontFamily := Js.string "sans-serif";

  let palette_bar = new Widget.group ~width ~height:bar_height doc in
  palette_bar#add_child (palette_text :> Widget.t);
  palette_bar#rect_style##.fill := Js.string palette_data.palette_color;

  let palette_group = new Widget.group ~y:bar_height ~width ~height doc in
  palette_group#rect_style##.fill := Js.string "#ababab";

  let palette_root = new Widget.group ~width ~height doc in
  palette_root#rect_style##.fill := Js.string "#ababab";
  palette_root#add_child (palette_bar :> Widget.t);
  palette_root#add_child (palette_group :> Widget.t);

  let palette =
    { palette_data
    ; palette_root
    ; palette_text
    ; palette_bar
    ; palette_group
    ; palette_y_offset = bar_height
    ; palette_collapsed = false
    ; syntactic_forms
    ; next_palette } in

  palette_bar#element##.onclick :=
    Dom.handler (fun _ev ->
        palette.palette_collapsed <- not palette.palette_collapsed;
        ignore (render (Palette palette));
        Js._false
      );

  List.iter syntactic_forms ~f:(fun (Syntax syn) ->
      set_style syn.syn_group palette_data;
      syn.syn_group#element##.onmousedown :=
        Dom.handler (fun ev ->
            let x = syn.syn_group#x in
            let y = syn.syn_group#y +. palette.palette_y_offset in
            let term =
              Syntax.run syn.symbol_of_term_template ~x ~y workspace syn
            in
            set_style term.block.group palette_data;
            ignore
              (workspace.root_layer#element##appendChild
                 (term.block.group#element :> Dom.node Js.t));
            Block.begin_drag (Term term) ev;
            ignore (Block.render_block_and_children term.block);
            Js._false
          );
      palette_group#add_child (syn.syn_group :> Widget.t)
    );
  Option.iter next_palette ~f:(fun (Palette next_palette) ->
      palette_root#add_child (next_palette.palette_root :> Widget.t)
    );
  palette
