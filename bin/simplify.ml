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
  (* =go= *)
  let rec go t acc =
    assert (well_scoped_term t);
    assert (well_scoped_stack acc);
    match (t.scope, acc) with
    (* replace base values, renaming variables if they appear in the substitutions *)
    | Base _, acc -> (
        let t = discharge_term t in
        match (t, acc) with
        (* simplify if branches with booleans *)
        | Base (Bool b), HoleIf (e1, e2) :: acc ->
            if b then go e1 acc else go e2 acc
        | _ -> plug acc t)
    (* abstractions with the right context to simplify *)
    | Fun (x, _, body), HoleFun b :: acc -> (*@ \label{go:fun-holefun} *)
        go (scope_with_new_var body t x (discharge_base b)) acc
    | TypeAbstraction (alpha, body), HoleType ty2 :: acc ->
        go (scope_with_new_ty body t alpha (discharge_ty ty2)) acc
    (* abstractions but can't simplify in the context *)
    | Fun (x, ty, body), acc ->
        let x' = Atom.fresh x.identifier in
        let ty = discharge_ty (inherit_scope ty t) in
        let body_scoped = scope_with_new_var body t x (Var x') in
        let new_body = go body_scoped [] in
        let t = Fun (x', ty, new_body) in
        plug acc t
    | TypeAbstraction (alpha, body), acc ->
        let alpha' = Atom.fresh alpha.identifier in
        let body_scoped = scope_with_new_ty body t alpha (TyFreeVar alpha') in
        let new_body = go body_scoped [] in
        let t = TypeAbstraction (alpha', new_body) in
        plug acc t
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
        let t = Let (x', t1, body) in
        plug acc t
    (* throw away the type annotation *)
    | TypeAnnotation (body, _), acc -> go (inherit_scope body t) acc
  (* =end= *)
  in
  let t' = go t acc in
  (* check that the term is closed *)
  assert (
    let free_variables = Terms.free_vars t' in
    VarSet.subset free_variables t.vars_term);
  assert (
    let free_ty_variables = Terms.free_ty_vars_of_term t' in
    VarSet.subset free_ty_variables t.vars_ty);
  t'
