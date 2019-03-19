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

(** Because these types mutually depend on each other, they must be defined in a
    single file. *)

type 'symbols ctx = {
    root_layer : Widget.group;
    mutable picked_up_block : 'symbols term option;
    scripts : 'symbols term Doubly_linked.t;
    mutable drop_candidate : 'symbols hole option;
    toolbox : 'symbols toolbox;
  }

and 'symbols block = {
    group : Widget.group;
    items : 'symbols item list;
    mutable dim : (float * int) option;
      (** The cached width and column count. The column is an integer because it
          is counted in discrete intervals! *)
    mutable parent : 'symbols parent;
    ctx : 'symbols ctx;
    mutable iterator : 'symbols term Doubly_linked.Elt.t option
  }

and 'symbols parent =
  | Hole_parent of 'symbols hole
  | Picked_up
  | Root of 'symbols term Doubly_linked.Elt.t
  | Unattached

(** [term'] is parameterized by both the set of ['symbols] in the language and
    the ['sort] of the term that it wraps. The ['sort] type parameter enables
    the library to work with
    {{: https://en.wikipedia.org/wiki/Many-sorted_logic} many-sorted logics} in
    a type-safe manner. *)

and ('symbols, 'sort) term' = {
    block : 'symbols block;
    term : 'sort;
    symbol_of_term : ('symbols, 'sort) term' -> 'symbols;
      (** A conversion function that "packs" the wrapped term into the symbol
          type *)
  }

(** In order for terms of different sorts to be used together, the sort needs to
    abstracted away in an existential type variable. *)
and 'symbols term = Term : ('symbols, 'sort) term' -> 'symbols term

and ('symbols, 'sort) hole' = {
    mutable hole_term : ('symbols, 'sort) term' option;
    term_of_symbol : 'symbols -> ('symbols, 'sort) term' option;
      (** A conversion function that "unpacks" the symbol into a term belongin
          to a specific sort. *)
    hole_rect : Widget.rect;
    mutable hole_parent : 'symbols block option;
  }

and 'symbols hole = Hole : ('symbols, 'sort) hole' -> 'symbols hole

and 'symbols item =
  | Child of 'symbols hole
  | Newline
  | Widget of Widget.t (** A reference to a "keyword" *)
  | Tab

and 'symbols toolbox = {
    toolbox_group : Widget.group;
    mutable palette : 'symbols palette option;
  }

and ('symbols, 'sort) palette' = {
    palette_text : Widget.text;
    palette_group : Widget.group;
    palette_style :
      (  g's_style : Dom_html.cssStyleDeclaration Js.t
      -> rect's_style : Dom_html.cssStyleDeclaration Js.t
      -> unit );
    syntactic_forms : ('symbols, 'sort) syntax list;
    next_palette : 'symbols palette option;
      (** A linked-list style "pointer" to the next palette *)
  }

and 'symbols palette =
  Palette : ('symbols, 'sort) palette' -> 'symbols palette

and placeholder = {
    placeholder_group : Widget.group;
    text : Widget.text;
  }

and ('symbols, 'arity) syn_item =
  | Syn_Child of placeholder * ('arity -> 'symbols hole)
  | Syn_Newline
  | Syn_Widget of Widget.t * (unit -> Widget.t)
  | Syn_Tab

(** [('symbols, 'sort, 'arity) t]

    ['symbols] - The set of symbols in the language
    ['sort] - The set of terms (e.g. expr, type, kind)
    ['arity] - The type of the arity (e.g. type * type, term * term) *)
and ('symbols, 'sort, 'arity) syntax' = {
    syn_items : ('symbols, 'arity) syn_item list;
    syn_create : unit -> 'arity;
    term_of_arity : 'arity -> 'sort;
    symbol_of_term_template : ('symbols, 'sort) term' -> 'symbols;
    syn_group : Widget.group
  }

and ('symbols, 'sort) syntax =
  Syntax : ('symbols, 'sort, _) syntax' -> ('symbols, 'sort) syntax
