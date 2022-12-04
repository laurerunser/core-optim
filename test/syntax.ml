open Libfun.Terms
open Libfun.Types

(* smart constructors for the terms *)
let fn v t f = Fun(v, t, f(Var (v)))
let ($) f s = FunApply (f, s)
let letin v t f = Let(v, t, f(Var(v)))
let ty_fn v f = TypeAbstraction(v, f(TyFreeVar(v)))
let ($!) t ty = TypeApply(t, ty)
let (^) t ty = TypeAnnotation(t, ty)

(* smart constructors for the types *)
let fv s = TyFreeVar s
let bv i = TyBoundVar i 
let (=>) s t = TyFun(s, t)
let tuple l = TyTuple l 
let fn_ty _ _ = failwith "todo"
