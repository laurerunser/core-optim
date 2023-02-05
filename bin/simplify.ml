open Terms
open Types
open Stack
open Typechecker

let rec simplify (t : term) =
  let ty = synth VarMap.empty t in
  let t = simplify_aux (empty_scope t) [] in
  assert (check VarMap.empty ty t = ty);
  t

and simplify_aux (t : term scoped) (acc : stack) =
  let rec go t acc =
    match (t.scope, acc) with
    (* simplify if branches with booleans *)
    | Base (Bool b), HoleIf (e1, e2) :: acc ->
        if b then
          let e1' = simplify_aux e1 [] in
          plug acc (inherit_scope e1' e1)
        else
          let e2' = simplify_aux e2 [] in
          plug acc (inherit_scope e2' e2)
    (* abstractions with the right context to simplify *)
    | Fun (x, _, body), HoleFun b :: acc ->
        go (scope_with_new_var body t x b.scope) acc
    | TypeAbstraction (alpha, body), HoleType ty2 :: acc ->
        go (scope_with_new_ty body t alpha ty2.scope) acc
    (* replace base values, renaming variables if they appear in the substitutions *)
    | Base _, acc -> plug acc t
    (* abstractions but can't simplify in the context *)
    | Fun (x, ty, body), acc ->
        let x' = Atom.fresh x.identifier in
        let body_scoped = scope_with_new_var body t x (Var x') in
        let new_body = go body_scoped [] in
        plug acc (inherit_scope (Fun (x', ty, new_body)) t)
    | TypeAbstraction (alpha, body), acc ->
        let alpha' = Atom.fresh alpha.identifier in
        let body_scoped = scope_with_new_ty body t alpha (TyFreeVar alpha') in
        let new_body = go body_scoped [] in
        plug acc (inherit_scope (TypeAbstraction (alpha', new_body)) t)
    (* cases that create a context that we can simplify in *)
    | FunApply (f, arg), acc ->
        let f = inherit_scope f t in
        let arg = inherit_scope arg t in
        go f (HoleFun arg :: acc)
    | TypeApply (f, ty), acc ->
        let f = inherit_scope f t in
        let ty = inherit_scope ty t in
        go f (HoleType ty :: acc)
    | IfThenElse (t1, t2, t3), acc ->
        let t1 = inherit_scope t1 t in
        let t2 = inherit_scope t2 t in
        let t3 = inherit_scope t3 t in
        go t1 (HoleIf (t2, t3) :: acc)
    (* other cases *)
    | Let (x, t1, body), acc ->
        let x' = Atom.fresh x.identifier in
        let t1 = go (inherit_scope t1 t) [] in
        let body = go (scope_with_new_var body t x (Var x')) [] in
        plug acc (inherit_scope (Let (x', t1, body)) t)
    (* throw away the type annotation *)
    | TypeAnnotation (body, _), acc -> go (inherit_scope body t) acc
  in
  assert (well_scoped t free_vars free_ty_vars_of_term);
  let t' = go t acc in
  (* check that the term is closed *)
  let free_variables = Terms.free_vars t' in
  let free_ty_variables = Terms.free_ty_vars_of_term t' in
  assert (VarSet.subset free_variables t.vars_term);
  assert (VarSet.subset free_ty_variables t.vars_ty);
  t'
