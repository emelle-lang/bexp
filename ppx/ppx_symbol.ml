open Base
open Ppxlib

let make_varpat ~loc name =
  { ppat_loc = loc
  ; ppat_desc = Ppat_var { txt = name; loc }
  ; ppat_attributes = [] }

let get_types list =
  let f (tys, nontys) item = match item.pstr_desc with
    | Pstr_type (_rec_flag, typedecls) -> typedecls :: tys, nontys
    | _ -> tys, item :: nontys
  in
  let tys, nontys = List.fold_left ~f ~init:([], []) list in
  List.concat tys, List.rev nontys

let get_typenames =
  List.map ~f:(fun typedecl -> typedecl.ptype_name.txt)

let core_type ~loc desc =
  { ptyp_desc = desc
  ; ptyp_loc = loc
  ; ptyp_attributes = [] }

let ty_constr ~loc ident args =
  core_type ~loc (Ptyp_constr ({txt = ident; loc}, args))

let make_constr ~loc name =
  let nt_ty = ty_constr ~loc (Lident name) [] in
  let symbol_ty = ty_constr ~loc (Lident "symbols") [] in
  let ty = ty_constr ~loc (Ldot (Lident "Bexp", "term")) [symbol_ty; nt_ty] in
  { pcd_name = { txt = String.capitalize name; loc }
  ; pcd_args = Pcstr_tuple [ty]
  ; pcd_res = None
  ; pcd_loc = loc
  ; pcd_attributes = [] }

let gen_symbol_ty ~loc types =
  let names = get_typenames types in
  let symbol_ty =
    { ptype_name = { txt = "symbols"; loc }
    ; ptype_params = []
    ; ptype_cstrs = []
    ; ptype_kind = Ptype_variant (List.map ~f:(make_constr ~loc) names)
    ; ptype_private = Public
    ; ptype_manifest = None
    ; ptype_attributes = []
    ; ptype_loc = loc }
  in symbol_ty, names

let gen_wrapper ~loc name =
  let fun_name = make_varpat ~loc ("symbol_of_" ^ name) in
  let constr_expr =
    { pexp_desc =
        (let lident_loc = { txt = Lident (String.capitalize name); loc } in
         Pexp_construct(lident_loc, Some [%expr data]))
    ; pexp_loc = loc
    ; pexp_attributes = [] } in
  [%stri let [%p fun_name] = fun data -> [%e constr_expr]]

let gen_getter ~loc name =
  let fun_name = make_varpat ~loc ("get_" ^ name) in
  let constr_pat =
    { ppat_desc =
        (let lident_loc = { txt = Lident (String.capitalize name); loc } in
         Ppat_construct(lident_loc, Some [%pat? data]))
    ; ppat_loc = loc
    ; ppat_attributes = [] } in
  [%stri let [%p fun_name] = function
     | [%p constr_pat] -> Some data
     | _ -> None
  ]

let gen_symbol_ty ~loc ~path:_ = function
  | PStr items ->
     let types, nontypes = get_types items in
     let symbol_ty, names = gen_symbol_ty ~loc types in
     { pmod_desc =
         Pmod_structure
           ({ pstr_desc = Pstr_type (Recursive, symbol_ty :: types)
            ; pstr_loc = loc }
            :: List.fold names ~init:nontypes
                 ~f:(fun acc next ->
                   (gen_wrapper ~loc next) :: (gen_getter ~loc next) :: acc)
           )
     ; pmod_loc = loc
     ; pmod_attributes = [] }
  | _ -> Location.raise_errorf ~loc "Error, not a structure"

let ext =
  (* Payload: structure
     Result (Context): module expr *)
  Extension.declare
    "symbol" Extension.Context.Module_expr Ast_pattern.__ gen_symbol_ty

let () =
  Driver.register_transformation "symbol"
    ~rules:[Ppxlib.Context_free.Rule.extension ext]
