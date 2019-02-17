open Base
open Js_of_ocaml

class virtual t = object
  method virtual set_x : float -> unit
  method virtual width : float
end

let string_of_float float =
  let str = Float.to_string float in
  (* If the stringified float ends in a decimal, append a 0 *)
  if Char.equal (String.get str (String.length str - 1)) '.' then
    str ^ "0"
  else
    str

let set_float_prop elem prop float =
  elem##setAttribute (Js.string prop) (Js.string @@ string_of_float float)

let set_x elem = set_float_prop elem "x"

let set_y elem = set_float_prop elem "y"

let set_width elem = set_float_prop elem "width"

let set_height elem = set_float_prop elem "height"

let length_of_anim js_t =
  js_t##.baseVal##.value

class rect doc x y width height = object
  inherit t
  val elem =
    let rect_elem = Dom_svg.createRect doc in
    set_x rect_elem x;
    set_y rect_elem y;
    set_width rect_elem width;
    set_height rect_elem height;
    rect_elem

  method set_x = set_x elem

  method width = length_of_anim elem##.width
end

class text doc text = object
  inherit t
  val elem =
    let text_elem = Dom_svg.createTextElement doc in
    ((Js.Unsafe.coerce text_elem)
     : <textContent : Js.js_string Js.t Js.prop> Js.t)##.textContent :=
      Js.string text;
    text_elem

  method set_x = set_x elem

  method width = length_of_anim elem##.textLength
end
