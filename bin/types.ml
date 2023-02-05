open PPrint

exception Not_Polymorphic

type tyvar = Atom.t (* type variable *) [@@deriving show, eq]

type ty =
  (* boolean *)
  | TyBool
  (* type variable *)
  | TyFreeVar of tyvar
  | TyBoundVar of int
  (* function type: TyFun(S, T) is S -> T *)
  | TyFun of ty * ty
  (* polymorphic type: PolymorphicType(X, T) is forAll X. T *)
  | PolymorphicType of (string[@equal fun _ _ -> true]) * ty
  (* tuple: T_0 * ... * T_k *)
  | TyTuple of ty list
[@@deriving show, eq]

(********** Type manipulation **********)

let rec abstract_gen x ty c =
  match ty with
  | PolymorphicType (y, t) -> PolymorphicType (y, abstract_gen x t (c + 1))
  | TyFreeVar v when v = x -> TyBoundVar c
  | TyFun (t1, t2) ->
      let t1 = abstract_gen x t1 c in
      let t2 = abstract_gen x t2 c in
      TyFun (t1, t2)
  | TyTuple l ->
      let l' = List.map (fun t -> abstract_gen x t c) l in
      TyTuple l'
  | _ as t -> t

let abstract x ty =
  let ty = abstract_gen x ty 0 in
  PolymorphicType (Atom.pretty_print_atom x, ty)

let rec fill_gen c s = function
  | PolymorphicType (x, t) -> PolymorphicType (x, fill_gen (c + 1) s t)
  | TyBoundVar x when x = c -> s
  | TyFun (t1, t2) ->
      let t1 = fill_gen c s t1 in
      let t2 = fill_gen c s t2 in
      TyFun (t1, t2)
  | TyTuple l ->
      let l = List.map (fill_gen c s) l in
      TyTuple l
  | t -> t

let fill t s =
  match t with
  | PolymorphicType (_, t) -> fill_gen 0 s t
  | _ -> raise Not_Polymorphic

(********** Pretty printing **********)
let rec pretty_print_type_paren paren t =
  match t with
  | TyBool -> string "bool"
  | TyFreeVar x -> !^(Atom.pretty_print_atom x)
  | TyBoundVar x -> !^(Int.to_string x)
  | _ ->
      let ty =
        match t with
        | TyFun (s, t) -> print_ty_fun s t
        | PolymorphicType (x, t) -> print_poly_type x t
        | TyTuple ts -> print_ty_tuple ts
        | _ -> assert false
      in
      group @@ if paren then parens ty else ty

and print_ty_fun s t =
  (pretty_print_type_paren true s ^^ blank 1 ^^ !^"->")
  ^//^ pretty_print_type_paren true t

and print_poly_type x t =
  (!^"forall" ^^ space ^^ !^x ^^ char '.') ^//^ pretty_print_type_paren true t

and print_ty_tuple ts =
  separate_map (space ^^ star ^^ space) (pretty_print_type_paren true) ts

let pretty_print_type = pretty_print_type_paren false

let to_string t =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print_type t);
  Buffer.contents b

module VarMap = Map.Make (Atom)

let rec sub_ty ty map =
  match ty with
  | TyBool -> TyBool
  | TyFreeVar x -> ( try VarMap.find x map with Not_found -> TyFreeVar x)
  | TyBoundVar _ -> ty
  | TyFun (ty1, ty2) -> TyFun (sub_ty ty1 map, sub_ty ty2 map)
  | PolymorphicType (s, ty) -> PolymorphicType (s, sub_ty ty map)
  | TyTuple l -> TyTuple (List.map (fun x -> sub_ty x map) l)

let sub_tyvar x map = try VarMap.find x map with Not_found -> TyFreeVar x

module VarSet = Set.Make (Atom)

let rec free_ty_vars t =
  let open VarSet in
  match t with
  | TyBool | TyBoundVar _ -> empty
  | TyFreeVar x -> singleton x
  | TyFun (s, t) -> union (free_ty_vars t) (free_ty_vars s)
  | PolymorphicType (_, body) -> free_ty_vars body
  | TyTuple l ->
      List.fold_left (fun acc x -> union (free_ty_vars x) acc) empty l
