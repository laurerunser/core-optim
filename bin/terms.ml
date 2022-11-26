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

and get_term_with_parens t = 
  match t with
  | Fun(_) | FunApply(_) -> parens (pretty_print t)   
  | _ -> pretty_print t

and print_abstraction x ty body =
  let body_parens = get_term_with_parens body in
  group @@
  prefix 2 1
  (string "fun" ^^ blank 1 ^^ (pretty_print (TypeAnnotation(Var(x), ty))) 
  ^^ blank 1 ^^ equals)
  body_parens

and print_fun_apply f x =
  let f_parens = get_term_with_parens f in 
  let x_parens = get_term_with_parens x in 
  group @@ 
  prefix 2 1
  f_parens
  x_parens

and print_let_in lhs rhs body = 
 let rhs_parens = get_term_with_parens rhs in
  group @@
  string "let"
  ^^ surround 2 1 empty (string lhs) empty
  ^^ string "="
  ^^ surround 2 1 empty rhs_parens empty
  ^^ string "in"
  ^^ prefix 0 1 empty (pretty_print body)

and print_type_abstraction tyvar t = 
  let t_parens = get_term_with_parens t in
  group @@
  prefix 2 1 
    (string "fun" ^^ blank 1 ^^ 
    brackets (string tyvar) ^^ blank 1 ^^ equals)
    t_parens

and print_type_apply t ty = 
  let t_parens = get_term_with_parens t in
  group @@
  t_parens
  ^^ lbracket
  ^^ (pretty_print_type ty)
  ^^ rbracket

and print_type_annotation x t = 
  let x_parens = get_term_with_parens x in
  group @@
  parens
    (x_parens 
      ^^ string ":" ^^ break 1
      ^^ (pretty_print_type t))


let to_string t =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print t);
  Buffer.contents b
      