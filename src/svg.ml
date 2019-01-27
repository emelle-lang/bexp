open Base
open Js_of_ocaml

(** A wrapper type around [Js_of_ocaml.Dom_svg.element] *)
type +'a t = {
    elt : 'a Js.t;
    width : int;
  }

let text doc text : Dom_svg.textElement t =
  let text_elem = Dom_svg.createTextElement doc in
  ((Js.Unsafe.coerce text_elem)
   : <textContent : Js.js_string Js.t Js.prop> Js.t)##.textContent :=
    Js.string text;
  { elt = text_elem
  ; width = 0 }

let string_of_float float =
  let str = Float.to_string float in
  (* If the stringified float ends in a decimal, append a 0 *)
  if Char.equal (String.get str (String.length str - 1)) '.' then
    str ^ "0"
  else
    str

let set_float_prop elem prop float =
  elem##setAttribute (Js.string prop) (Js.string @@ string_of_float float)

let set_width elem = set_float_prop elem "width"

let set_height elem = set_float_prop elem "height"

let length_of_anim js_t =
  js_t##.baseVal##.value
