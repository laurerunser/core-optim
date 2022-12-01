open Libfun.Terms
open Libfun.Types
(*
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

and variable = string (* variable *)
*)

let fn v t f = Fun(v, t, f(Var (v)))

let ($) f s = FunApply (f, s)

let letin v t f = Let(v, t, f(Var(v)))

let ty_fn v f = TypeAbstraction(v, f(TyFreeVar(v)))

let ($!) t ty = TypeApply(t, ty)

let (^) t ty = TypeAnnotation(t, ty)