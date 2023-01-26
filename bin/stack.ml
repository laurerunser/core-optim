open Terms
open Types
open PPrint

type frame =
  | HoleFun of base (* applied function: f(term) -> where f is not given *)
  | HoleType of
      ty (* instantiated polymorphism: T[ty] -> where T is not given *)
  | HoleIf of term * term (* for conditions: _ then e1 else e2 *)
[@@deriving show]

and stack = frame list [@@deriving show]

let rec plug (s : stack) (t : term) =
  match s with
  | [] -> t
  | f :: s ->
      let filled_term =
        match f with
        | HoleFun arg -> FunApply (t, arg)
        | HoleType arg -> TypeApply (t, arg)
        | HoleIf (e1, e2) -> IfThenElse (t, e1, e2)
      in
      plug s filled_term

let pretty_print_frame f =
  match f with
  | HoleFun arg ->
      let f = string "_" in
      let x = print_base arg in
      group @@ prefix 2 1 f x
  | HoleType arg ->
      let t = string "_" in
      group @@ t ^^ lbracket ^^ pretty_print_type arg ^^ rbracket
  | HoleIf (e1, e2) ->
      let t = string " _ " in
      group @@ string "if" ^^ t ^^ string "then"
      ^^ surround 2 1 empty (pretty_print e1) empty
      ^^ string "else"
      ^^ surround 0 1 empty (pretty_print e2) empty

let rec pretty_print s =
  match s with
  | [] -> empty
  | f :: tail -> pretty_print_frame f ^^ hardline ^^ nest 1 (pretty_print tail)

let to_string s =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print s);
  Buffer.contents b

module VarMap = Map.Make (Atom)

let sub_var x map =
  let x' = try VarMap.find x map with Not_found -> Var x in
  Base x'

let rec sub_ty ty map =
  match ty with
  | TyBool -> TyBool
  | TyFreeVar x -> ( try VarMap.find x map with Not_found -> TyFreeVar x)
  | TyBoundVar _ as ty -> ty
  | TyFun (ty1, ty2) -> TyFun (sub_ty ty1 map, sub_ty ty2 map)
  | PolymorphicType (s, ty) -> PolymorphicType (s, sub_ty ty map)
  | TyTuple l -> TyTuple (List.map (fun x -> sub_ty x map) l)

let rec simplify (t : term) (acc : stack) (p_var : base VarMap.t)
    (p_ty : ty VarMap.t) =
  match (t, acc) with
  (* simplify if branches with booleans *)
  | Base (Bool b), HoleIf (e1, e2) :: acc' ->
      if b then plug acc' e1 else plug acc' e2
  (* replace base values, renaming variables if they appear in the substitutions *)
  | Base (Bool _), acc -> plug acc t
  | Base (Var x), acc ->
      let x' = sub_var x p_var in
      plug acc x'
  (* plug acc (sub_var x p_var) *)
  (* abstractions with the right context to simplify *)
  | Fun (x, _, body), HoleFun b :: acc' ->
      simplify body acc' (VarMap.add x b p_var) p_ty
  | TypeAbstraction (ty, t), HoleType ty2 :: acc' ->
      simplify t acc' p_var (VarMap.add ty ty2 p_ty)
  (* abstractions but can't simplify in the context *)
  | Fun (x, ty, body), acc ->
      let x' = Atom.fresh x.identifier in
      plug acc
        (Fun
           ( x',
             sub_ty ty p_ty,
             simplify body [] (VarMap.add x (Var x') p_var) p_ty ))
  | TypeAbstraction (ty, t), acc ->
      let ty' = Atom.fresh ty.identifier in
      plug acc
        (TypeAbstraction
           (ty', simplify t [] p_var (VarMap.add ty (TyFreeVar ty') p_ty)))
  (* cases that create a context that we can simplify in *)
  | FunApply (f, arg), acc -> simplify f (HoleFun arg :: acc) p_var p_ty
  | TypeApply (t, ty), acc ->
      let ty' = sub_ty ty p_ty in
      simplify t (HoleType ty' :: acc) p_var p_ty
  (* other cases *)
  | IfThenElse (t1, t2, t3), acc ->
      let t2' = simplify t2 [] p_var p_ty in
      let t3' = simplify t3 [] p_var p_ty in
      simplify t1 (HoleIf (t2', t3') :: acc) p_var p_ty
  | Let (x, t1, t_body), acc ->
      let x' = Atom.fresh x.identifier in
      let t1' = simplify t1 [] p_var p_ty in
      let p_var = VarMap.add x (Var x') p_var in
      let t_body' = simplify t_body [] p_var p_ty in
      plug acc (Let (x', t1', t_body'))
  (* throw away the type annotation *)
  | TypeAnnotation (t, _), acc -> simplify t acc p_var p_ty
