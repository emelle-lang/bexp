(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base
open Js_of_ocaml

class type t = object
  method element : Dom_svg.element Js.t
  method x : float
  method y : float
  method set_x : float -> unit
  method set_y : float -> unit
  method width : float
  method height : float
  method set_onresize : (unit -> unit) -> unit
end

let set_string_prop elem prop str =
  elem##setAttribute (Js.string prop) (Js.string str)

let string_of_float float =
  let str = Float.to_string float in
  (* If the stringified float ends in a decimal, append a 0 *)
  if Char.equal (String.get str (String.length str - 1)) '.' then
    str ^ "0"
  else
    str

let set_float_prop elem prop float =
  set_string_prop elem prop (string_of_float float)

let length_of_anim js_t =
  js_t##.baseVal##.value

let set_x elem = set_float_prop elem "x"

let set_y elem = set_float_prop elem "y"

let set_width elem = set_float_prop elem "width"

let set_height elem = set_float_prop elem "height"

let render_transform x y =
  let x = string_of_float x in
  let y = string_of_float y in
  "translate(" ^ x ^ " " ^ y ^ ")"

class rect
        ?(x=0.0) ?(y=0.0)
        ?(width=0.0) ?(height=0.0)
        ?(rx=0.0) ?(ry=0.0)
        doc = object
  val elem =
    let rect_elem = Dom_svg.createRect doc in
    set_x rect_elem x;
    set_y rect_elem y;
    set_width rect_elem width;
    set_height rect_elem height;
    set_float_prop rect_elem "rx" rx;
    set_float_prop rect_elem "ry" ry;
    rect_elem

  method element = elem

  method x = length_of_anim elem##.x

  method set_x = set_x elem

  method y = length_of_anim elem##.y

  method set_y = set_y elem

  method width = length_of_anim elem##.width

  method set_width = set_width elem

  method height = length_of_anim elem##.height

  method set_height = set_height elem

  method set_onresize (_ : unit -> unit) = ()
end

class group
        ?(x=0.0) ?(y=0.0)
        ?(width=0.0) ?(height=0.0)
        ?(rx=0.0) ?(ry=0.0)
        doc = object
  val mutable x = x
  val mutable y = y
  val elem = Dom_svg.createG doc
  val rect = new rect ~x:0.0 ~y:0.0 ~width ~height ~rx ~ry doc

  initializer
    set_string_prop elem "transform" (render_transform x y);
    ignore (elem##appendChild (rect#element :> Dom.node Js.t))

  method element = elem

  method x = x

  method set_x x' =
    x <- x';
    set_string_prop elem "transform" (render_transform x y)

  method y = y

  method set_y y' =
    y <- y';
    set_string_prop elem "transform" (render_transform x y)

  method width = rect#width

  method set_width = rect#set_width

  method height = rect#height

  method set_height = rect#set_height

  method add_child (child : t) =
    ignore (elem##appendChild (child#element :> Dom.node Js.t))

  method set_onresize (_ : unit -> unit) = ()

  method rect_style = rect#element##.style

  method show = elem##.style##.visibility := Js.string "visible"

  method hide = elem##.style##.visibility := Js.string "collapse"
end

class text ?(x=0.0) ?(y=0.0) doc text = object
  val mutable x = x
  val mutable y = y
  val padding = 4.0
  val elem =
    let text_elem = Dom_svg.createTextElement doc in
    ((Js.Unsafe.coerce text_elem)
     : <textContent : Js.js_string Js.t Js.prop> Js.t)##.textContent :=
      Js.string text;
    text_elem

  initializer
    set_x elem (x +. padding);
    set_y elem (y +. 15.0)

  method element = elem

  method x = x

  method set_x x' =
    x <- x';
    set_x elem (x +. padding)

  method y = y

  method set_y y' =
    y <- y';
    set_y elem (y +. 15.0)

  method width = elem##getComputedTextLength +. (padding *. 2.0)

  method height = 20.0

  method set_onresize (_ : unit -> unit) = ()
end

class text_input ?(x=0.0) ?(y=0.0) ?(str="") set_str doc = object
  val foreign_obj =
    (* Dom_svg.createForeignObject is implemented incorrectly -_- *)
    (Js.Unsafe.coerce (Dom_svg.createElement doc "foreignObject")
     : Dom_svg.foreignObjectElement Js.t)
  val input = Dom_html.createInput Dom_html.document

  initializer
    input##.value := Js.string str;
    ignore (foreign_obj##appendChild (input :> Dom.node Js.t));
    set_x foreign_obj x;
    set_y foreign_obj y;
    set_width foreign_obj 30.0;
    set_height foreign_obj 20.0;
    input##.style##.lineHeight := Js.string "10px";
    input##.style##.fontSize := Js.string "10px";
    ignore (
        Dom.addEventListener foreign_obj
          (Dom_html.Event.mousedown)
          (Dom.handler (fun ev ->
               Dom_html.stopPropagation ev;
               Js._true
          ))
          Js._false
      )

  method element = foreign_obj

  method input = input

  method x = length_of_anim foreign_obj##.x

  method set_x = set_x foreign_obj

  method y = length_of_anim foreign_obj##.y

  method set_y = set_y foreign_obj

  method width = length_of_anim foreign_obj##.width

  method height = length_of_anim foreign_obj##.height

  method set_onresize (onresize : unit -> unit) =
    input##.oninput :=
      Dom.handler (fun _ ->
          let str = set_str (Js.to_string input##.value) in
          let char_c = String.length str in
          let sz = if char_c > 0 then char_c else 1 in
          set_float_prop input "size" (Float.of_int sz);
          let rect = input##getBoundingClientRect in
          (* Why would the width be undefined...? MDN doesn't note anything
             like that... *)
          set_width foreign_obj (Js.Optdef.get rect##.width (fun () -> 0.0));
          onresize ();
          Js._false
        )

  method set_enabled b =
    input##.disabled := Js.bool (not b)
end

class ['a] wrapper ?x ?y (ch : (#t as 'a)) doc = object
  val group = new group ?x ?y doc
  val child = ch

  initializer
    group#add_child child;
    group#set_height child#height

  method element = group#element

  method x = group#x

  method set_x = group#set_x

  method y = group#y

  method set_y = group#set_y

  method width =
    group#set_width child#width;
    child#width

  method height =
    group#set_height child#height;
    child#height

  method set_onresize = child#set_onresize

  method style = group#rect_style

  method wrapped = child
end

module type ScrollAxis = sig
  val length : t -> float
  val set_length : rect -> float -> unit
  val pos : t -> float
  val set_pos : t -> float -> unit
  val event_pos : Dom_html.mouseEvent Js.t -> int
end

module Horiz : ScrollAxis = struct
  let length t = t#width
  let set_length t = t#set_width
  let pos t = t#x
  let set_pos t = t#set_x
  let event_pos ev = ev##.clientX
end

module Vert : ScrollAxis = struct
  let length t = t#height
  let set_length t = t#set_height
  let pos t = t#y
  let set_pos t = t#set_y
  let event_pos ev = ev##.clientY
end

(* Generic scrollbar to be instantiated either by Horiz or Vert to be a
   horizontal or vertical scrollbar, respectively *)
module Scrollbar (Axis : ScrollAxis) = struct
  type widget = t

  let color = Js.string "#c0c0c0"

  let on_scroll_color = Js.string "#a0a0a0"

  class t ~rect ~box_length widget =
  object(self)
    val rect = rect
    (* The length of the viewport *)
    val box_length = box_length
    (* The widget being scrolled *)
    val wrapped = widget
    (* Callback called when scrolled *)
    val mutable on_scroll = fun () -> ()

    initializer
      rect#element##.style##.fill := color;
      ignore (
          Dom.addEventListener rect#element Dom_html.Event.mousedown
            (Dom.handler (fun ev ->
                 self#begin_scroll ev;
                 Js._false
            )) Js._false
        )

    method set_on_scroll f =
      on_scroll <- f

    method begin_scroll ev =
      let init_pos = Axis.pos (rect :> widget) in
      let init_client_pos = Axis.event_pos ev in
      rect#element##.style##.fill := on_scroll_color;
      let doc = Dom_html.document in
      doc##.onmousemove :=
        Dom.handler (fun ev ->
            Axis.set_pos (rect :> widget)
              (init_pos +.
                 (Float.of_int (Axis.event_pos ev - init_client_pos)));
            let bounds = box_length -. self#length in
            if Float.compare (Axis.pos (rect :> widget)) bounds = 1 then
              Axis.set_pos (rect :> widget) bounds;
            (* The above check can make the position negative, so do the 0
             check after, not before *)
            if Float.compare (Axis.pos (rect :> widget)) 0.0 = -1 then
              Axis.set_pos (rect :> widget) 0.0;
            let progress = Axis.pos (rect :> widget) /. bounds in
            Axis.set_pos widget (0.0 -. progress *. bounds);
            on_scroll ();
            Js._true
          );
      doc##.onmouseup :=
        Dom.handler (fun _ev ->
            let pure_handler = Dom.handler (fun _ -> Js._true) in
            doc##.onmousemove := pure_handler;
            doc##.onmouseup := pure_handler;
            rect#element##.style##.fill := color;
            Js._true
          )

    method render =
      Axis.set_length rect self#length

    method element = rect#element

    method x = rect#x

    method set_x = rect#set_x

    method y = rect#y

    method set_y = rect#set_y

    (* Length of the scrollbar on the axis being scrolled *)
    method length = box_length /. (Axis.length widget) *. box_length

    method width = rect#width

    method height = rect#height

    method set_onresize (_ : unit -> unit) = ()
  end
end

module HorizScrollbar = Scrollbar(Horiz)
module VertScrollbar = Scrollbar(Vert)

let create_horiz_scrollbar ~x ~y ?(r=5.0) ~width ?(height=10.0) widget doc =
  let rect = new rect ~x ~y ~rx:r ~ry:r ~width ~height doc in
  new HorizScrollbar.t ~rect ~box_length:width widget

let create_vert_scrollbar ~x ~y ?(r=5.0) ?(width=10.0) ~height widget doc =
  let rect = new rect ~x ~y ~rx:r ~ry:r ~width ~height doc in
  new VertScrollbar.t ~rect ~box_length:height widget

class scrollbox ?x ?y ~width ~height (a : t) doc =
  object(self)
    val root = new group ?x ?y ~width ~height doc
    val wrapped = a
    val horiz_scrollbar =
      create_horiz_scrollbar
        ~y:(height -.10.0) ~x:0.0 ~width:(width -. 10.0) a doc
    val vert_scrollbar =
      create_vert_scrollbar
        ~x:(width -. 10.0) ~y:0.0 ~height:(height -. 10.0) a doc
    val clip_group = Dom_svg.createG doc

    initializer
      let on_scroll () = self#render in
      horiz_scrollbar#set_on_scroll on_scroll;
      vert_scrollbar#set_on_scroll on_scroll;
      (* Hide the parts of the wrapped widget that go out of the box *)
      set_string_prop clip_group "clip-path" self#clip_path;
      ignore (clip_group##appendChild (wrapped#element :> Dom.node Js.t));
      ignore (root#element##appendChild (clip_group :> Dom.node Js.t));
      root#add_child (horiz_scrollbar :> t);
      root#add_child (vert_scrollbar :> t)

    method clip_path =
      (* I think I only need to do this if the SVG was positioned using
         transforms, and if it's something like a rect, it won't work *)
      let x = 0.0 -. wrapped#x in
      let y = 0.0 -. wrapped#y in
      let width = string_of_float (x +. width -. 10.0) in
      let height = string_of_float (y +. height -. 10.0) in
      let x = string_of_float x in
      let y = string_of_float y in
      "polygon(" ^ x ^ " " ^ y ^ ", "
      ^ width ^ " " ^ y ^ ", "
      ^ width ^ " " ^ height ^ ", "
      ^ x ^ " " ^ height ^ ")"

    method wrapped = wrapped

    method element = root#element

    method x = root#x

    method set_x = root#set_x

    method y = root#y

    method set_y = root#set_y

    method width = root#width

    method render =
      set_string_prop clip_group "clip-path" self#clip_path;
      horiz_scrollbar#render;
      vert_scrollbar#render

    method height = height

    method set_on_scroll f =
      horiz_scrollbar#set_on_scroll f;
      vert_scrollbar#set_on_scroll f
end
