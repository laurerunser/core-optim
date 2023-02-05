open Terms
open Types
open PPrint
module VarSet = Terms.VarSet
module VarMap = Terms.VarMap

type 'a scoped = {
  (* the term *)
  scope : 'a;
  (* the variables that can appear in the next map
     ie the free variables we saw earlier *)
  vars_term : VarSet.t; [@opaque]
  (* the variables that should be substituted in the term *)
  p_term : base VarMap.t; [@opaque]
  (* same thing but for type variables *)
  vars_ty : VarSet.t; [@opaque]
  p_ty : ty VarMap.t; [@opaque]
}
[@@deriving show]

type frame =
  | HoleFun of
      base scoped (* applied function: f(term) -> where f is not given *)
  | HoleType of
      ty scoped (* instantiated polymorphism: T[ty] -> where T is not given *)
  | HoleIf of term scoped * term scoped (* for conditions: _ then e1 else e2 *)
[@@deriving show]

and stack = frame list [@@deriving show]

let well_scoped (t : 'a scoped) (freevars_term : 'a -> VarSet.t)
    (freevars_ty : 'a -> VarSet.t) =
  (* vars_term and vars_ty must be disjoint *)
  if not (VarSet.disjoint t.vars_term t.vars_ty) then false
  else
    (* p_term and p_ty must be disjoint *)
    let get_keys map =
      VarSet.of_list @@ fst @@ List.split @@ VarMap.bindings map
    in
    if not (VarSet.disjoint (get_keys t.p_term) (get_keys t.p_ty)) then false
    else
      (* the free variables must be included in vars_term
         and the free type variables must be included in vars_ty *)
      let f_term = freevars_term t.scope in
      let f_ty = freevars_ty t.scope in
      VarSet.subset f_term t.vars_term && VarSet.subset f_ty t.vars_ty

(* Returns a scoped term where the scope is [t], and all the sets/maps are empty *)
let empty_scope t =
  {
    scope = t;
    vars_term = VarSet.empty;
    vars_ty = VarSet.empty;
    p_term = VarMap.empty;
    p_ty = VarMap.empty;
  }

let inherit_scope t old =
  {
    scope = t;
    vars_term = old.vars_term;
    vars_ty = old.vars_ty;
    p_term = old.p_term;
    p_ty = old.p_ty;
  }

let scope_with_new_var (t : term) (old : term scoped) (old_var : Atom.t)
    (new_var : base) =
  {
    scope = t;
    vars_term = old.vars_term;
    vars_ty = old.vars_ty;
    p_term = VarMap.add old_var new_var old.p_term;
    p_ty = old.p_ty;
  }

let scope_with_new_ty (t : term) (old : term scoped) (old_ty : Atom.t)
    (new_ty : ty) =
  {
    scope = t;
    vars_term = old.vars_term;
    vars_ty = old.vars_ty;
    p_term = old.p_term;
    p_ty = VarMap.add old_ty new_ty old.p_ty;
  }

let discharge_term (t : term scoped) =
  let rec sub_terms term =
    match term with
    | Base (Bool _) as b -> b
    | Base (Var x) -> Base (sub_var x t.p_term)
    | Fun (x, ty, body) -> (
        match sub_var x t.p_term with
        | Var x -> Fun (x, sub_ty ty t.p_ty, sub_terms body)
        | _ -> assert false)
    | FunApply (f, arg) ->
        let arg' =
          match arg with Bool _ as b -> b | Var x -> sub_var x t.p_term
        in
        FunApply (sub_terms f, arg')
    | Let (x, e, body) -> (
        match sub_var x t.p_term with
        | Var x -> Let (x, sub_terms e, sub_terms body)
        | _ -> assert false)
    | IfThenElse (c, e1, e2) ->
        IfThenElse (sub_terms c, sub_terms e1, sub_terms e2)
    | TypeAbstraction (alpha, body) -> (
        match sub_tyvar alpha t.p_ty with
        | TyFreeVar x -> TypeAbstraction (x, sub_terms body)
        | _ -> assert false)
    | TypeApply (f, arg) -> TypeApply (sub_terms f, sub_ty arg t.p_ty)
    | TypeAnnotation (x, ty) -> TypeAnnotation (sub_terms x, sub_ty ty t.p_ty)
  in
  sub_terms t.scope

let discharge_base (t : base scoped) =
  match t.scope with Bool _ as b -> b | Var x -> sub_var x t.p_term

let discharge_ty (t : ty scoped) = sub_ty t.scope t.p_ty

let plug (s : stack) (t : term scoped) =
  let rec plug_term s t =
    match s with
    | [] -> t
    | f :: s ->
        let filled_term =
          match f with
          | HoleFun arg -> FunApply (t, discharge_base arg)
          | HoleType arg -> TypeApply (t, discharge_ty arg)
          | HoleIf (e1, e2) ->
              IfThenElse (t, discharge_term e1, discharge_term e2)
        in
        plug_term s filled_term
  in
  plug_term s (discharge_term t)

let pretty_print_frame f =
  match f with
  | HoleFun arg ->
      let f = string "_" in
      let x = print_base arg.scope in
      group @@ prefix 2 1 f x
  | HoleType arg ->
      let t = string "_" in
      group @@ t ^^ lbracket ^^ pretty_print_type arg.scope ^^ rbracket
  | HoleIf (e1, e2) ->
      let t = string " _ " in
      group @@ string "if" ^^ t ^^ string "then"
      ^^ surround 2 1 empty (pretty_print e1.scope) empty
      ^^ string "else"
      ^^ surround 0 1 empty (pretty_print e2.scope) empty

let rec pretty_print s =
  match s with
  | [] -> empty
  | f :: tail -> pretty_print_frame f ^^ hardline ^^ nest 1 (pretty_print tail)

let to_string s =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print s);
  Buffer.contents b
