(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core_kernel
open Js_of_ocaml

(** Throughout the code, the data types are parameterized by the ['symbols] type
    variable. This variable represents the set of {i symbols} in the language.
    The terminology of "symbol" is taken from the idea of a
    {{: https://en.wikipedia.org/wiki/Signature_(logic)} signature} from formal
    logic. A signature describing a logical system defines the set of (constant,
    function, and predicate) symbols used in it.

    Note that this library does not make a distinction between constant,
    function, and predicate symbols. A constant symbol is simply a function
    symbol with an arity of the unit type, and whether something is a predicate
    or function symbol is not something that the library is concerned with.
    (This library is for creating block interfaces, i.e. syntaxes, not
    semantics!) *)

(* Because these types mutually depend on each other, they must be defined in a
   single file. *)

type 'symbols ctx = {
    root_layer : Widget.group;
    script_scrollbox : Widget.scrollbox;
    mutable picked_up_block : 'symbols exists_term option;
    scripts : 'symbols exists_term Doubly_linked.t;
    mutable drop_candidate : 'symbols exists_hole option;
    toolbox : 'symbols toolbox;
    entry_exists_hole : 'symbols exists_hole;
  }

and 'symbols block = {
    group : Widget.group;
    items : 'symbols item list;
    mutable dim : (float * int) option;
      (** The cached width and column count. The column is an integer because it
          is counted in discrete intervals! *)
    mutable parent : 'symbols parent;
    ctx : 'symbols ctx;
    mutable iterator : 'symbols exists_term Doubly_linked.Elt.t option
  }

and 'symbols parent =
  | Hole_parent of 'symbols exists_hole
  | Picked_up
  | Root of 'symbols exists_term Doubly_linked.Elt.t
  | Unattached

(** [term'] is parameterized by both the set of ['symbols] in the language and
    the ['sort] of the term that it wraps. The ['sort] type parameter enables
    the library to work with
    {{: https://en.wikipedia.org/wiki/Many-sorted_logic} many-sorted logics} in
    a type-safe manner. *)
and ('symbols, 'sort) term = {
    block : 'symbols block;
    term : 'sort;
    symbol_of_term : ('symbols, 'sort) term -> 'symbols;
      (** A conversion function that "packs" the wrapped term into the symbol
          type *)
  }

(** In order for terms of different sorts to be used together, the sort needs to
    abstracted away in an existential type variable. *)
and 'symbols exists_term = Term : ('symbols, 'sort) term -> 'symbols exists_term

and ('symbols, 'sort) hole = {
    mutable hole_term : ('symbols, 'sort) term option;
    term_of_symbol : 'symbols -> ('symbols, 'sort) term option;
      (** A conversion function that "unpacks" the symbol into a term belonging
          to a specific sort. *)
    hole_placeholder : placeholder;
    hole_group : Widget.group;
    mutable hole_parent : 'symbols block option;
  }

and 'symbols exists_hole =
  Hole : ('symbols, _) hole -> 'symbols exists_hole

and 'symbols item =
  | Child of 'symbols exists_hole
  | Newline
  | Widget of Widget.t (** A reference to a "keyword" *)
  | Tab

and 'symbols toolbox = {
    toolbox_scrollbox : Widget.scrollbox;
    mutable palette : 'symbols exists_palette option;
  }

and palette_data = {
    palette_name : string;
    palette_color : string;
  }

and ('symbols, 'sort) palette = {
    palette_data : palette_data;
    palette_root : Widget.group;
    palette_text : Widget.text;
    palette_bar : Widget.group;
    palette_group : Widget.group;
    mutable palette_collapsed : bool;
    syntactic_forms : ('symbols, 'sort) exists_syntax list;
    mutable palette_y_offset : float;
    mutable palette_expanded_height : float;
    mutable palette_dims_computed : bool;
    mutable palette_toolbox : 'symbols toolbox option;
    next_palette : 'symbols exists_palette option;
      (** A linked-list style "pointer" to the next palette *)
  }

and 'symbols exists_palette =
  Palette : ('symbols, _) palette -> 'symbols exists_palette

and placeholder = {
    placeholder_group : Widget.group;
    text : Widget.text;
  }

and ('symbols, 'arity) syn_item =
  | Syn_Child :
      placeholder * ('arity -> ('symbols, _) hole)
      -> ('symbols, 'arity) syn_item
  | Syn_Newline
  | Syn_Widget :
      Widget.t * ('arity -> Widget.t) -> ('symbols, 'arity) syn_item
  | Syn_Tab

(** [('symbols, 'sort, 'arity) t]

    ['symbols] - The set of symbols in the language
    ['sort] - The set of terms (e.g. expr, type, kind)
    ['arity] - The type of the arity (e.g. type * type, term * term) *)
and ('symbols, 'sort, 'arity, 'syn_arity) syntax = {
    syn_items : ('symbols, 'arity) syn_item list;
    syn_create : unit -> 'arity;
    term_of_arity : 'arity -> 'sort;
    symbol_of_term_template : ('symbols, 'sort) term -> 'symbols;
    syn_group : Widget.group
  }

and ('symbols, 'sort) exists_syntax =
  Syntax : ('symbols, 'sort, _, _) syntax -> ('symbols, 'sort) exists_syntax

type ('symbols, 'sort) t = {
    workspace : 'symbols ctx;
    hole : ('symbols, 'sort) hole;
  }

let set_style widget palette_data =
  let g's_style = widget#element##.style in
  let rect's_style = widget#rect_style in
  rect's_style##.fill := Js.string palette_data.palette_color;
  rect's_style##.strokeWidth := Js.string "3";
  rect's_style##.stroke := Js.string "white";
  g's_style##.fill := Js.string "white";
  g's_style##.fontFamily := Js.string "sans-serif";
