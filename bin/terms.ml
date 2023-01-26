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

module VarSet = Set.Make (Atom)

let rec free_vars t =
  let open VarSet in
  match t with
  | Base x -> (
      match x with
      | Var x -> singleton x
      | _ -> empty (* boolean value, not a free var *))
  | Fun (x, _, t) -> diff (free_vars t) (singleton x)
  | FunApply (t, u) ->
      union (free_vars t) (free_vars (Base u))
      (* u is either a boolean value or a variable defined outside of the function *)
  | Let (x, t, u) -> union (free_vars t) (diff (free_vars u) (singleton x))
  | IfThenElse (e1, e2, e3) ->
      union (free_vars e1) (union (free_vars e2) (free_vars e3))
  | TypeAbstraction (_, t) | TypeApply (t, _) | TypeAnnotation (t, _) ->
      free_vars t

let rec substitution v s = function
  | Base (Var x) when x = v -> Base s
  | Fun (x, ty, t) when x <> v -> Fun (x, ty, substitution v s t)
  | FunApply (t, Var x) when x = v -> FunApply (substitution v s t, s)
  | FunApply (t, a) -> FunApply (substitution v s t, a)
  | Let (x, t, u) ->
      Let (x, substitution v s t, if x <> v then substitution v s u else u)
  | IfThenElse (e1, e2, e3) ->
      IfThenElse (substitution v s e1, substitution v s e2, substitution v s e3)
  | TypeAbstraction (ty, t) -> TypeAbstraction (ty, substitution v s t)
  | TypeApply (t, ty) -> TypeApply (substitution v s t, ty)
  | TypeAnnotation (t, ty) -> TypeAnnotation (substitution v s t, ty)
  | t -> t

let rec beta_reduce t =
  match t with
  | Base _ -> t
  | Fun (x, ty, t) -> Fun (x, ty, beta_reduce t)
  | FunApply (Fun (x, _, t), a) -> substitution x a (beta_reduce t)
  | FunApply (t, a) -> FunApply (beta_reduce t, a)
  | Let (x, t, u) -> Let (x, beta_reduce t, beta_reduce u)
  | IfThenElse (e1, e2, e3) ->
      IfThenElse (beta_reduce e1, beta_reduce e2, beta_reduce e3)
  | TypeAbstraction (ty, t) -> TypeAbstraction (ty, beta_reduce t)
  | TypeApply (t, ty) -> TypeApply (beta_reduce t, ty)
  | TypeAnnotation (t, ty) -> TypeAnnotation (beta_reduce t, ty)
