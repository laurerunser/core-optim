open Libfun.Terms
open Libfun.Types
open Libfun.Stack

(* smart constructors for the terms *)
let fn v t f = Fun (v, t, f (Var v))
let ( $ ) f s = FunApply (f, s)
let letin v t f = Let (v, t, f (Var v))
let ite t1 t2 t3 = IfThenElse (t1, t2, t3)
let ty_fn v f = TypeAbstraction (v, f (TyFreeVar v))
let ( $! ) t ty = TypeApply (t, ty)
let ( ^ ) t ty = TypeAnnotation (t, ty)

(* smart constructors for the types *)
let fv s = TyFreeVar s
let bv i = TyBoundVar i
let ( => ) s t = TyFun (s, t)
let tuple l = TyTuple l
let poly_ty v f = abstract v (f (TyFreeVar v))

(* smart constructors for the frames *)
let hfun t = HoleFun (empty_scope t)
let htype t = HoleType (empty_scope t)
let hif t1 t2 = HoleIf (empty_scope t1, empty_scope t2)