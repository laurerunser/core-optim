open Types
open PPrint

type term =
  (* variable *)
  | Var of variable

  (* abstraction: Fun(x,T,t) is fun (x:T) = t*)
  | Fun of variable * ty * term

  (* function application: FunApply(t,u) is t u *)
  | FunApply of term * term

  (* let binding: Let(x,t,u) is let x = t in u *)
  | Let of variable * term * term

  (* type abstraction: TypeAbstraction(X,t) is fun[X]=t *)
  | TypeAbstraction of tyvar * term

  (* type application: TypeApply(t,T) is t[T] *)
  | TypeApply of term * ty

  (* type annotation: TypeAnnotation(t,T) is (t:T) *)
  | TypeAnnotation of term * ty
[@@deriving show]

and variable = string (* variable *)
[@@deriving show]


let rec pretty_print t = 
  match t with
  | Var x -> string x
  | Fun(x, ty, body) -> print_abstraction x ty body
  | FunApply(f, x) -> print_fun_apply f x
  | Let(lhs, rhs, body) -> print_let_in lhs rhs body
  | TypeAbstraction(tyvar, t) -> print_type_abstraction tyvar t
  | TypeApply(t, ty) -> print_type_apply t ty
  | TypeAnnotation(x, t) -> print_type_annotation x t

and print_abstraction x ty body =
  group @@
  prefix 2 1 
  (string "fun" ^^ parens (pretty_print (TypeAnnotation(Var(x), ty))) ^^ equals)
  (pretty_print body)
and print_fun_apply f x =
  group @@ 
  prefix 2 1
  (pretty_print f)
  (pretty_print x)
and print_let_in lhs rhs body = 
  group @@
  string "let"
  ^^ surround 2 1 empty (string lhs) empty
  ^^ string "="
  ^^ surround 2 1 empty (pretty_print rhs) empty
  ^^ string "in"
  ^^ prefix 0 1 empty (pretty_print body)

and print_type_abstraction tyvar t = 
  group @@
  prefix 2 1 
    (string "fun" ^^ brackets (string tyvar) ^^ equals)
    (pretty_print t)

and print_type_apply t ty = 
  group @@
  (pretty_print t)
  ^^ lbracket ^^ break 1
  ^^ (pretty_print_type ty)
  ^^ break 1
  ^^ rbracket

and print_type_annotation x t = 
  group @@
  parens
    ((pretty_print x) 
      ^^ string ":"
      ^^ (pretty_print_type t)) 


and pretty_print_type ty =
  match ty with 
  | TyVar x -> string x
  | TyFun(f, x) -> print_ty_fun f x
  | PolymorphicType(tyvar, ty) -> print_poly_type tyvar ty
  | TyTuple(ty_list) -> failwith "todo"

and print_ty_fun f x = 
  group @@
  prefix 2 1 
  ((pretty_print_type f) ^^ string "->")
  (pretty_print_type x)

and print_poly_type tyvar ty =
  group @@
  string "for all"
  ^^ string tyvar
  ^^ string "."
  ^^ (pretty_print_type ty)
