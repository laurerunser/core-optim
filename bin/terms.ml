open Types
open PPrint

(* a basic value: either a variable name or a boolean *)
type base = Var of Atom.t | Bool of bool [@@deriving show, eq]

type term =
  (* base value *)
  | Base of base
  (* abstraction: Fun(x,T,t) is fun (x:T) = t*)
  | Fun of Atom.t * ty * term
  (* function application: FunApply(t,u) is t u *)
  | FunApply of term * base
  (* let binding: Let(x,t,u) is let x = t in u *)
  | Let of Atom.t * term * term
  (* condition: IfThenElse(e1, e2, e3) is If e1 Then e2 Else e3 *)
  | IfThenElse of term * term * term
  (* type abstraction: TypeAbstraction(X,t) is fun[X]=t *)
  | TypeAbstraction of Atom.t * term
  (* type application: TypeApply(t,T) is t[T] *)
  | TypeApply of term * ty
  (* type annotation: TypeAnnotation(t,T) is (t:T) *)
  | TypeAnnotation of term * ty
[@@deriving show, eq]

let print_base x =
  match x with
  | Bool b -> if b then string "true" else string "false"
  | Var n -> string (Atom.pretty_print_atom n)

let rec pretty_print t =
  match t with
  | Base x -> print_base x
  | Fun (x, ty, body) -> print_abstraction x ty body
  | FunApply (f, x) -> print_fun_apply f x
  | Let (lhs, rhs, body) -> print_let_in lhs rhs body
  | IfThenElse (e1, e2, e3) -> print_if e1 e2 e3
  | TypeAbstraction (tyvar, t) -> print_type_abstraction tyvar t
  | TypeApply (t, ty) -> print_type_apply t ty
  | TypeAnnotation (x, t) -> print_type_annotation x t

and get_term_with_parens t =
  match t with
  | Fun _ | FunApply _ -> parens (pretty_print t)
  | _ -> pretty_print t

and print_abstraction x ty body =
  let body_parens = get_term_with_parens body in
  group
  @@ prefix 2 1
       (string "fun" ^^ blank 1
       ^^ pretty_print (TypeAnnotation (Base (Var x), ty))
       ^^ blank 1 ^^ equals)
       body_parens

and print_fun_apply f x =
  let f_parens = get_term_with_parens f in
  let x_parens = print_base x in
  group @@ prefix 2 1 f_parens x_parens

and print_let_in lhs rhs body =
  let rhs_parens = get_term_with_parens rhs in
  group @@ string "let"
  ^^ surround 2 1 empty (string (Atom.pretty_print_atom lhs)) empty
  ^^ string "="
  ^^ surround 2 1 empty rhs_parens empty
  ^^ string "in"
  ^^ prefix 0 1 empty (pretty_print body)

and print_if e1 e2 e3 =
  group @@ string "if"
  ^^ surround 2 1 empty (pretty_print e1) empty
  ^^ string "then"
  ^^ surround 2 1 empty (pretty_print e2) empty
  ^^ string "else"
  ^^ surround 0 1 empty (pretty_print e3) empty

and print_type_abstraction tyvar t =
  let t_parens = get_term_with_parens t in
  group
  @@ prefix 2 1
       (string "fun" ^^ blank 1
       ^^ brackets (string (Atom.pretty_print_atom tyvar))
       ^^ blank 1 ^^ equals)
       t_parens

and print_type_apply t ty =
  let t_parens = get_term_with_parens t in
  group @@ t_parens ^^ lbracket ^^ pretty_print_type ty ^^ rbracket

and print_type_annotation x t =
  let x_parens = get_term_with_parens x in
  group @@ parens (x_parens ^^ string ":" ^^ break 1 ^^ pretty_print_type t)

let to_string t =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print t);
  Buffer.contents b

module VarSet = Types.VarSet

let free_vars_base b =
  let open VarSet in
  match b with
  | Var x -> singleton x
  | _ -> empty (* boolean value, not a free var *)

let rec free_vars t =
  let open VarSet in
  match t with
  | Base x -> free_vars_base x
  | Fun (x, _, t) -> diff (free_vars t) (singleton x)
  | FunApply (t, u) ->
      union (free_vars t) (free_vars (Base u))
      (* u is either a boolean value or a variable defined outside of the function *)
  | Let (x, t, u) -> union (free_vars t) (diff (free_vars u) (singleton x))
  | IfThenElse (e1, e2, e3) ->
      union (free_vars e1) (union (free_vars e2) (free_vars e3))
  | TypeAbstraction (_, t) | TypeApply (t, _) | TypeAnnotation (t, _) ->
      free_vars t

let rec free_ty_vars_of_term t =
  let open VarSet in
  match t with
  | Base _ -> empty
  | FunApply (a, _) -> free_ty_vars_of_term a
  | Let (_, a, b) -> union (free_ty_vars_of_term a) (free_ty_vars_of_term b)
  | IfThenElse (c, e1, e2) ->
      union
        (union (free_ty_vars_of_term c) (free_ty_vars_of_term e1))
        (free_ty_vars_of_term e2)
  | TypeAbstraction (tyvar, t) ->
      diff (free_ty_vars_of_term t) (singleton tyvar)
  | Fun (_, ty, t) | TypeApply (t, ty) | TypeAnnotation (t, ty) ->
      union (Types.free_ty_vars ty) (free_ty_vars_of_term t)

module VarMap = Types.VarMap

let rec sub_var x map =
  let sub x map = try VarMap.find x map with Not_found -> Var x in
  match sub x map with
  (* keep trying to substitute until you reach a fix point *)
  | Var y -> if y = x then Var y else sub_var y map
  | _ as y -> y

let alpha_eq t1 t2 =
  let rec alpha_eq_aux p_var p_ty t1 t2 =
    let alpha_eq_same = alpha_eq_aux p_var p_ty in
    match (t1, t2) with
    | Base (Bool x), Base (Bool y) -> x = y
    | Base (Var x), Base (Var y) -> sub_var x p_var = Var y
    | Fun (x1, ty1, t1), Fun (x2, ty2, t2) when sub_ty ty1 p_ty = ty2 ->
        let p_var = VarMap.add x1 (Var x2) p_var in
        alpha_eq_aux p_var p_ty t1 t2
    | FunApply (t1, b1), FunApply (t2, b2) ->
        alpha_eq_same t1 t2 && alpha_eq_same (Base b1) (Base b2)
    | Let (x1, t1, u1), Let (x2, t2, u2) when alpha_eq_same t1 t2 ->
        let p_var = VarMap.add x1 (Var x2) p_var in
        alpha_eq_aux p_var p_ty u1 u2
    | IfThenElse (i1, t1, e1), IfThenElse (i2, t2, e2) ->
        alpha_eq_same i1 i2 && alpha_eq_same t1 t2 && alpha_eq_same e1 e2
    | TypeAbstraction (ty1, t1), TypeAbstraction (ty2, t2) ->
        let p_ty = VarMap.add ty1 (TyFreeVar ty2) p_ty in
        alpha_eq_aux p_var p_ty t1 t2
    | TypeApply (t1, ty1), TypeApply (t2, ty2) ->
        sub_ty ty1 p_ty = ty2 && alpha_eq_same t1 t2
    | TypeAnnotation (t1, ty1), TypeAnnotation (t2, ty2) ->
        sub_ty ty1 p_ty = ty2 && alpha_eq_same t1 t2
    | _ -> false
  in
  alpha_eq_aux VarMap.empty VarMap.empty t1 t2

exception Type_Error of ty

let type_error msg = failwith (Printf.sprintf "Type error!\n%s" msg)

let type_check_error term expected actual =
  type_error
    (Printf.sprintf "Term : %s\nExpected type: %s\nReceived type: %s\n"
       (to_string term) (Types.to_string expected) (Types.to_string actual))

let type_synth_error term actual expected =
  type_error
    (Printf.sprintf "Term : %s\nExpected a %s type\nReceived type: %s\n"
       (to_string term) expected (Types.to_string actual))

let type_if_branches_error branch expected actual =
  type_error
    (Printf.sprintf
       "Branches do not have the same type\n\
        Branch: %s\n\
        Received type: %s\n\
        Expected %s" (to_string branch) (Types.to_string actual)
       (Types.to_string expected))

let rec synth (ctxt : ty VarMap.t) (t : term) =
  match t with
  | Base (Bool _) -> TyBool
  | Base (Var v) -> (
      try VarMap.find v ctxt
      with _ ->
        failwith
          (Printf.sprintf "The variable %s was not in the type map\n"
             (Atom.pretty_print_atom v)))
  | Fun (v, ty, t) ->
      let ctxt = VarMap.add v ty ctxt in
      let ty2 = synth ctxt t in
      TyFun (ty, ty2)
  | FunApply (t1, t2) -> (
      (* find the type of the function *)
      let ty = synth ctxt t1 in
      (* verify that the argument is the right type *)
      match ty with
      | TyFun (ty1, ty2) -> (
          try
            let _ = check ctxt ty1 (Base t2) in
            ty2
          with Type_Error ty -> type_check_error (Base t2) ty ty1)
      | _ -> type_synth_error t ty "function")
  | Let (v, t, body) ->
      (* find the type of the new binding *)
      let ty = synth ctxt t in
      (* add the binding into the map *)
      let ctxt = VarMap.add v ty ctxt in
      (* find and return the type of the body *)
      synth ctxt body
  | IfThenElse (e1, e2, e3) -> (
      let _ =
        (* check that the condition is a boolean (either true or false) *)
        try check ctxt TyBool e1
        with Type_Error actual_ty -> type_check_error e1 TyBool actual_ty
      in
      (* check that both branches have the same type *)
      let ty = synth ctxt e2 in
      try check ctxt ty e3
      with Type_Error actual_ty -> type_if_branches_error e3 ty actual_ty)
  | TypeAbstraction (ty_var, t) ->
      (* ignore the X not in freevars *)
      (* also leave the context as it was -> no need to add the X *)
      let ty = synth ctxt t in
      (* abstract the type we got for `t` with `ty_var` as the bound variabl *)
      abstract ty_var ty
  | TypeApply (t, ty) -> (
      (* get the general type of `t` *)
      let for_all_ty = synth ctxt t in
      try fill for_all_ty ty (* replace the bound variable by `ty` *)
      with Not_Polymorphic -> type_synth_error t ty "polymorphic")
  | TypeAnnotation (t, ty) -> (
      try check ctxt ty t with Type_Error ty' -> type_check_error t ty' ty)

and check (ctxt : ty VarMap.t) (ty : ty) (t : term) =
  let ty1 = synth ctxt t in
  if Types.equal_ty ty1 ty then ty else raise (Type_Error ty1)
