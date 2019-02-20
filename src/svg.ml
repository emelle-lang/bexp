open Base
open Js_of_ocaml

class virtual t = object
  method virtual element : Dom_svg.element Js.t
  method virtual set_x : float -> unit
  method virtual width : float
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

class group ?(x=0.0) ?(y=0.0) doc = object
  val mutable x = x
  val mutable y = y
  val elem =
    let group_elem = Dom_svg.createG doc in
    group_elem

  initializer
    set_string_prop elem "transform" (render_transform x y)

  method element = elem

  method x = x

  method set_x x' =
    x <- x';
    set_string_prop elem "transform" (render_transform x y)

  method y = y
end

class rect
        ?(x=0.0) ?(y=0.0)
        ?(width=0.0) ?(height=0.0)
        ?(rx=0.0) ?(ry=0.0)
        ?style
        doc = object
  val elem =
    let rect_elem = Dom_svg.createRect doc in
    set_x rect_elem x;
    set_y rect_elem y;
    set_width rect_elem width;
    set_height rect_elem height;
    set_float_prop rect_elem "rx" rx;
    set_float_prop rect_elem "ry" ry;
    Option.iter style ~f:(fun style ->
        set_string_prop rect_elem "style" style
      );
    rect_elem

  method element = elem

  method x = elem##.x

  method set_x = set_x elem

  method y = elem##.y

  method set_y = set_y elem

  method width = length_of_anim elem##.width

  method set_width = set_width elem

  method height = length_of_anim elem##.height
end

class text doc text = object
  val elem =
    let text_elem = Dom_svg.createTextElement doc in
    ((Js.Unsafe.coerce text_elem)
     : <textContent : Js.js_string Js.t Js.prop> Js.t)##.textContent :=
      Js.string text;
    text_elem

  method element = elem

  method x = elem##.x

  method set_x = set_x elem

  method y = elem##.y

  method width =
    elem##getComputedTextLength
end
