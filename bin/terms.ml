open Types

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
