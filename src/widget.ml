open Base
open Js_of_ocaml

class type t = object
  method element : Dom_svg.element Js.t
  method set_x : float -> unit
  method set_y : float -> unit
  method width : float
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
  val elem =
    let text_elem = Dom_svg.createTextElement doc in
    ((Js.Unsafe.coerce text_elem)
     : <textContent : Js.js_string Js.t Js.prop> Js.t)##.textContent :=
      Js.string text;
    text_elem

  initializer
    set_x elem x;
    set_y elem (y +. 15.0)

  method element = elem

  method x = x

  method set_x x' =
    x <- x';
    set_x elem x

  method y = y

  method set_y y' =
    y <- y';
    set_y elem (y +. 15.0)

  method width = elem##getComputedTextLength

  method set_onresize (_ : unit -> unit) = ()
end

class text_input ?(x=0.0) ?(y=0.0) ?(str="") doc = object
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

  method x = length_of_anim foreign_obj##.x

  method set_x = set_x foreign_obj

  method y = length_of_anim foreign_obj##.y

  method set_y = set_y foreign_obj

  method width = length_of_anim foreign_obj##.width

  method height = length_of_anim foreign_obj##.height

  method set_onresize onresize =
    input##.oninput :=
      Dom.handler (fun _ ->
          let char_c = input##.value##.length in
          set_float_prop input "size" (Float.of_int char_c);
          let rect = input##getBoundingClientRect in
          (* Why would the width be undefined...? MDN doesn't note anything
             like that... *)
          set_width foreign_obj (Js.Optdef.get rect##.width (fun() -> 0.0));
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
    (* Hack for my purposes; how to make this more modular? *)
    group#set_height 20.0

  method element = group#element

  method x = group#x

  method set_x = group#set_x

  method y = group#y

  method set_y = group#set_y

  method width =
    group#set_width child#width;
    child#width

  method set_onresize = child#set_onresize

  method style = group#rect_style

  method wrapped = child
end
