open Libfun
open Terms
open Types
open Atom
open Typechecker
open Syntax

let check_ty = Alcotest.testable pp_ty equal_ty

let test_typecheck expected ctxt term =
  Alcotest.(check check_ty)
    (Format.asprintf "%a" pp_ty expected)
    expected (synth ctxt term)

let x = fresh "x"
let y = fresh "y"
let z = fresh "z"
let a = fresh "a"
let b = fresh "b"
let tx = fresh "X"
let ts = fresh "S"
let tt = fresh "T"

(**************************)
(* Var *)
let test_typecheck_var_in_map () =
  let toto = fresh "toto" in
  let ty = TyFreeVar ts in
  let ctxt = VarMap.singleton toto ty in
  test_typecheck ty ctxt (Var toto)

(* This test fails (as it should) because the variable `toto` is
   not in the VarMap *)
let test_typecheck_fail_not_in_map () =
  let term = Var (fresh "toto") in
  test_typecheck (TyFreeVar tt) VarMap.empty term

(**************************)
(* Fun *)

(* fun x -> x where `x` is of type `S` *)
let test_typecheck_fun_id () =
  let tv = TyFreeVar ts in
  let f = fn x tv (fun x -> x) in
  let ty = tv => tv in
  test_typecheck ty VarMap.empty f

let test_typecheck_fun_id2 () =
  let tv =
    poly_ty tx (fun x -> x => tuple [ TyFreeVar ts; x; x; TyFreeVar tt ])
    => TyFreeVar tt
  in
  let f = fn x tv (fun x -> x) in
  let ctxt = VarMap.singleton a tv in
  test_typecheck tv ctxt (f $ Var a)

(**************************)
(* FunApply *)
let test_typecheck_fun_apply () =
  let tv = TyFreeVar ts in
  let f = fn x tv (fun x -> x) in
  let ctxt = VarMap.singleton a tv in
  test_typecheck tv ctxt (f $ Var a)

(* This test fails (as it should) because the function is supposed to be
   of type `ts` but we give it `x` of type `tx` *)
let test_typecheck_fun_apply2 () =
  let tv = TyFreeVar ts in
  let tvx = TyFreeVar tx in
  let f = fn x tv (fun x -> x) in
  let ctxt = VarMap.singleton a tvx in
  test_typecheck tvx ctxt (f $ Var a)

(**************************)
(* Let *)

(* let x = a in (fun (b:S) -> x) in which `a` is of type X *)
let test_typecheck_let () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = ty2 => ty in
  let l = letin x (Var a) (fun z -> fn b ty2 (fun _ -> z)) in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty_expected ctxt l

(**************************)
(* TypeAbstraction *)
(* fun [X] = fun (a:X) -> a *)
let test_typecheck_type_abstract () =
  let poly_id = ty_fn tx (fun t -> fn x t (fun z -> z)) in
  let ty = poly_ty tt (fun x -> x => x) in
  test_typecheck ty VarMap.empty poly_id

(* This test fails because the variable use for type abstraction is a free
   variable *)
let test_typecheck_type_abstract_fv () =
  let ty = TyFreeVar tx in
  let t = ty_fn tx (fun _ -> Var a ^ TyFreeVar tx) in
  let ty_expected = poly_ty tt (fun _ -> TyFreeVar tx) in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty_expected ctxt t

(**************************)
(* TypeApply *)
(* (fun [X] -> (a:X)) S *)
let test_typecheck_type_apply () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_fun = ty_fn tx (fun x -> Var a ^ x) in
  let term = ty_fun $! ty2 in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty2 ctxt term

(**************************)
(* TypeAnnotation *)
(* (x : X) *)
let test_typecheck_type_annotation_simple () =
  let ty = TyFreeVar tx in
  let t = Var x ^ ty in
  let ctxt = VarMap.singleton x ty in
  test_typecheck ty ctxt t

(* let x = (a:X) in (fun (b:S) -> x) *)
let test_typecheck_type_annotation () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = ty2 => ty in
  let ctxt = VarMap.singleton a ty in
  let l = letin x (Var a ^ ty) (fun z -> fn b ty2 (fun _ -> z)) in
  test_typecheck ty_expected ctxt l
