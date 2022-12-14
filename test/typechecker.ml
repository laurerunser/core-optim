open Libfun
open Terms
open Types
open Atom
open Typechecker
open Syntax

let check_ty = Alcotest.testable pp_ty equal_ty

let test_typecheck expected ty =
  Alcotest.(check check_ty) (Format.asprintf "%a" pp_ty expected) expected ty

let x = fresh "x"
let y = fresh "y"
let z = fresh "z"
let a = fresh "a"
let b = fresh "b"
let tx = fresh "X"
let ts = fresh "S"

(**************************)
(* Var *)
let test_typecheck_var_in_map () =
  let toto = fresh "toto" in
  let ty = TyFreeVar ts in
  let ctxt = VarMap.add toto ty VarMap.empty in
  test_typecheck ty (synth ctxt (Var toto))

(* This test fails (as it should) because the variable `toto` is
   not in the VarMap *)
(* let test_fail_not_in_map () =
   let term = Var(fresh "toto") in
   test_typecheck
   (synth VarMap.empty term)
   (TyFreeVar(tx)) *)

(**************************)
(* Fun *)
let test_typecheck_fun_id () =
  let tv = TyFreeVar ts in
  let f = fn x tv (fun x -> x) in
  let ty = TyFun (tv, tv) in
  test_typecheck ty (synth VarMap.empty f)

(**************************)
(* FunApply *)
let test_typecheck_fun_apply () =
  let tv = TyFreeVar ts in
  let f = fn x tv (fun x -> x) in
  let ctxt = VarMap.add x tv VarMap.empty in
  test_typecheck tv (synth ctxt (FunApply (f, Var x)))

(* This test fails (as it should) because the function is supposed to be
   of type `ts` but we give it `x` of type `tx` *)
(* let test_fun_apply2 () =
   let tv = TyFreeVar ts in
   let tvx = TyFreeVar tx in
   let f = fn x tv (fun x -> x) in
   let ctxt = VarMap.add x tvx VarMap.empty in
   test_typecheck tvx (synth ctxt (FunApply (f, Var x))) *)

(**************************)
(* Let *)

(* let x = a in (fun (b:S) -> x) in which `a` is of type X *)
let test_typecheck_let () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = TyFun (ty2, ty) in
  let l = letin x (Var a) (fun z -> fn b ty2 (fun _ -> z)) in
  let ctxt = VarMap.add a ty VarMap.empty in
  test_typecheck ty_expected (synth ctxt l)

(**************************)
(* TypeAbstraction *)
let test_typecheck_fun_poly () =
  let poly_id = ty_fn x (fun t -> fn tx t (fun x -> x)) in
  test_typecheck (poly_ty x (fun x -> x => x)) (synth VarMap.empty poly_id)

(**************************)
(* TypeApply *)
let test_typecheck_type_apply () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = ty2 in
  let ty_fun = ty_fn tx (fun x -> TypeAnnotation (Var a, x)) in
  let term = TypeApply (ty_fun, ty2) in
  let ctxt = VarMap.add a ty VarMap.empty in
  test_typecheck ty_expected (synth ctxt term)

(**************************)
(* TypeAnnotation *)

(* let x = (a:X) in (fun (b:S) -> x) *)
let test_typecheck_type_annotation () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = TyFun (ty2, ty) in
  let ctxt = VarMap.add a ty VarMap.empty in
  let l =
    letin x (TypeAnnotation (Var a, ty)) (fun z -> fn b ty2 (fun _ -> z))
  in
  test_typecheck ty_expected (synth ctxt l)