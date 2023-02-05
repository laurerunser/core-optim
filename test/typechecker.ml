open Libfun
open Terms
open Types
open Atom
open Typechecker
open Syntax
open Stack

let check_ty = Alcotest.testable pp_ty equal_ty

let test_typecheck expected ctxt term =
  Alcotest.(check check_ty)
    (Format.asprintf "term: %a\ntype: %a" pp_term term pp_ty expected)
    expected (synth ctxt term)

let test_fail_typecheck msg ctxt term =
  Alcotest.(
    check_raises "failure" (Failure msg) (fun () ->
        let _ = synth ctxt term in
        ()))

let toto = fresh "toto"
let x = fresh "x"
let y = fresh "y"
let z = fresh "z"
let a = fresh "a"
let b = fresh "b"
let tx = fresh "X"
let ts = fresh "S"
let tt = fresh "T"
let pa = pretty_print_atom

(**************************)
(* Var *)
let test_typecheck_var_in_map () =
  let toto = fresh "toto" in
  let ty = TyFreeVar ts in
  let ctxt = VarMap.singleton toto ty in
  test_typecheck ty ctxt (Base (Var toto))

(* This test fails (as it should) because the variable `toto` is
   not in the VarMap *)
let test_typecheck_fail_not_in_map () =
  let toto = fresh "toto" in
  let term = Base (Var toto) in
  test_fail_typecheck
    (Format.sprintf "The variable %s was not in the type map\n"
       (Atom.pretty_print_atom toto))
    VarMap.empty term

(**************************)
(* Fun *)

(* fun x -> x where `x` is of type `S` *)
let test_typecheck_fun_id () =
  let tv = TyFreeVar ts in
  let f = fn x tv (fun x -> Base x) in
  let ty = tv => tv in
  test_typecheck ty VarMap.empty f

let test_typecheck_fun_id2 () =
  let tv =
    poly_ty tx (fun x -> x => tuple [ TyFreeVar ts; x; x; TyFreeVar tt ])
    => TyFreeVar tt
  in
  let f = fn x tv (fun x -> Base x) in
  let ctxt = VarMap.singleton a tv in
  test_typecheck tv ctxt (f $ Var a)

let test_typecheck_fun_simple () =
  let tv = TyFreeVar ts in
  let tu = TyFreeVar tt in
  let f = fn x tv (fun _ -> Base (Var y)) in
  let ty = tv => tu in
  let ctxt = VarMap.singleton y tu in
  test_typecheck ty ctxt f

(**************************)
(* FunApply *)
let test_typecheck_fun_apply () =
  let tv = TyFreeVar ts in
  let f = fn x tv (fun x -> Base x) in
  let ctxt = VarMap.singleton a tv in
  test_typecheck tv ctxt (f $ Var a)

(* This test fails (as it should) because the function is supposed to be
   of type `ts` but we give it `x` of type `tx` *)
let test_typecheck_fun_apply2 () =
  let tv = TyFreeVar ts in
  let tvx = TyFreeVar tx in
  let f = fn x tv (fun x -> Base x) in
  let ctxt = VarMap.singleton a tvx in
  test_fail_typecheck
    (Format.sprintf
       "Type error!\nTerm : %s\nExpected type: %s\nReceived type: %s\n"
       (Terms.to_string (Base (Var a)))
       (Types.to_string tvx) (Types.to_string tv))
    ctxt (f $ Var a)

let test_typecheck_fun_apply_simple () =
  let tv = TyFreeVar ts in
  let tu = TyFreeVar tt in
  let f = fn x tv (fun _ -> Base (Var y)) in
  let ctxt = VarMap.add y tu (VarMap.singleton a tv) in
  test_typecheck tu ctxt (f $ Var a)

(**************************)
(* Let *)

let test_typecheck_let_simple () =
  let tv = TyFreeVar ts in
  let tu = TyFreeVar tt in
  let l = letin x (Base (Var a)) (fun _ -> Base (Var y)) in
  let ctxt = VarMap.add y tv (VarMap.singleton a tu) in
  test_typecheck tv ctxt l

(* let x = a in (fun (b:S) -> x) in which `a` is of type X *)
let test_typecheck_let () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = ty2 => ty in
  let l = letin x (Base (Var a)) (fun z -> fn b ty2 (fun _ -> Base z)) in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty_expected ctxt l

(**************************)
(* IfThenElse *)
let test_typecheck_if1 () =
  let ty2 = TyFreeVar ts in
  let t = IfThenElse (Base (Bool true), Base (Var x), Base (Var y)) in
  let ctxt = VarMap.singleton x ty2 in
  let ctxt = VarMap.add y ty2 ctxt in
  test_typecheck ty2 ctxt t

let test_typecheck_if2 () =
  let ty2 = TyFreeVar ts in
  let t = IfThenElse (Base (Bool false), Base (Var x), Base (Var y)) in
  let ctxt = VarMap.singleton x ty2 in
  let ctxt = VarMap.add y ty2 ctxt in
  test_typecheck ty2 ctxt t

let test_typecheck_if_bad1 () =
  (* fails because the condition in the If is NOT a boolean *)
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let t = IfThenElse (Base (Var a), Base (Var x), Base (Var y)) in
  let ctxt = VarMap.singleton x ty2 in
  let ctxt = VarMap.add y ty2 ctxt in
  let ctxt = VarMap.add a ty ctxt in
  let msg =
    Printf.sprintf
      "Type error!\nTerm : %s\nExpected type: bool\nReceived type: %s\n" (pa a)
      (pa tx)
  in
  test_fail_typecheck msg ctxt t

let test_typecheck_if_bad2 () =
  (* fails because both branches do NOT have the same type *)
  let ty = TyBool in
  let ty2 = TyFreeVar ts in
  let t = IfThenElse (Base (Bool false), Base (Var x), Base (Var y)) in
  let ctxt = VarMap.singleton x ty in
  let ctxt = VarMap.add y ty2 ctxt in
  let msg =
    Printf.sprintf
      "Type error!\n\
       Branches do not have the same type\n\
       Branch: %s\n\
       Received type: %s\n\
       Expected bool" (pa y) (pa ts)
  in
  test_fail_typecheck msg ctxt t

(**************************)
(* TypeAbstraction *)
let test_typecheck_type_abstract_simple () =
  let tv = TyFreeVar tt in
  let t = ty_fn tx (fun _ -> Base (Var a)) in
  let ty = poly_ty tx (fun _ -> tv) in
  let ctxt = VarMap.singleton a tv in
  test_typecheck ty ctxt t

(* fun [X] = fun (a:X) -> a *)
let test_typecheck_type_abstract () =
  let poly_id = ty_fn tx (fun t -> fn x t (fun z -> Base z)) in
  let ty = poly_ty tt (fun x -> x => x) in
  test_typecheck ty VarMap.empty poly_id

(**************************)
(* TypeApply *)
let test_typecheck_type_apply_simple () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_fun = ty_fn tx (fun _ -> Base (Var a)) in
  let term = ty_fun $! ty2 in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty2 ctxt term

(* (fun [X] -> (a:X)) S *)
let test_typecheck_type_apply () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_fun = ty_fn tx (fun x -> Base (Var a) ^ x) in
  let term = ty_fun $! ty2 in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty2 ctxt term

(**************************)
(* TypeAnnotation *)
(* (x : X) *)
let test_typecheck_type_annotation_simple () =
  let ty = TyFreeVar tx in
  let t = Base (Var x) ^ ty in
  let ctxt = VarMap.singleton x ty in
  test_typecheck ty ctxt t

(* let x = (a:X) in (fun (b:S) -> x) *)
let test_typecheck_type_annotation () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_expected = ty2 => ty in
  let ctxt = VarMap.singleton a ty in
  let l = letin x (Base (Var a) ^ ty) (fun z -> fn b ty2 (fun _ -> Base z)) in
  test_typecheck ty_expected ctxt l

(**********************************************************************)
(* stack Typechecking *)

let test_typechecking_stack expected stack ty ctxt =
  Alcotest.(check check_ty)
    (Format.asprintf "type: %a\nin stack: %a\nexpected: %a" pp_ty ty pp_stack
       stack pp_ty expected)
    expected
    (Typechecker.synth_stack stack ty ctxt)

let test_fail_typecheck_stack msg stack t ctxt =
  Alcotest.(
    check_raises "failure" (Failure msg) (fun () ->
        let _ = synth_stack stack t ctxt in
        ()))

let test_stack_fun1_good () =
  let stack = [ hfun (Var x) ] in
  let ty = TyFreeVar ts => TyFreeVar tt in
  let ctxt = VarMap.singleton x (TyFreeVar ts) in
  let expected_ty = TyFreeVar tt in
  test_typechecking_stack expected_ty stack ty ctxt

let test_stack_fun1_bad () =
  (* same test than the previous one, except the type of `x` in the ctxt *)
  let stack = [ hfun (Var x) ] in
  let ty = TyFreeVar ts => TyFreeVar tt in
  let ctxt = VarMap.singleton x (TyFreeVar tx) in
  test_fail_typecheck_stack
    (Printf.sprintf
       "Type error!\n\
        Frame: %s\n\
        Expected a %s->_ type\n\
        Received type: %s -> %s\n"
       (Stack.to_string stack) (pa tx) (pa ts) (pa tt))
    stack ty ctxt

let test_stack_poly1_good () =
  let stack = [ htype (TyFreeVar tx) ] in
  let ty = poly_ty (fresh "X") (fun x -> x => TyFreeVar ts) in
  let expected_ty = TyFreeVar tx => TyFreeVar ts in
  test_typechecking_stack expected_ty stack ty VarMap.empty

let test_stack_poly1_bad () =
  let stack = [ htype (TyFreeVar tx) ] in
  let ty = TyFreeVar ts in
  test_fail_typecheck_stack
    (Printf.sprintf
       "Type error!\nFrame: %s\nExpected a polymorphic type\nReceived type: %s"
       (Stack.to_string stack) (pa ts))
    stack ty VarMap.empty

let test_stack_fun2_good () =
  let stack = [ hfun (Var y); hfun (Var x) ] in
  let ty = TyFreeVar tx => (TyFreeVar ts => TyFreeVar toto) in
  let ctxt = VarMap.singleton y (TyFreeVar tx) in
  let ctxt = VarMap.add x (TyFreeVar ts) ctxt in
  let expected_ty = TyFreeVar toto in
  test_typechecking_stack expected_ty stack ty ctxt

let test_stack_fun2_bad () =
  let stack = [ hfun (Var y); hfun (Var x) ] in
  (* this one will fail because the return type of f is not a function,
     which means it cannot plug the next hole *)
  let ty = TyFreeVar tx => TyFreeVar toto in
  let ctxt = VarMap.singleton y (TyFreeVar tx) in
  let ctxt = VarMap.add x (TyFreeVar ts) ctxt in
  test_fail_typecheck_stack
    (Printf.sprintf
       "Type error!\nFrame: %s\nExpected a %s->_ type\nReceived type: %s\n"
       (Stack.to_string [ List.nth stack 1 ])
       (pa ts) (pa toto))
    stack ty ctxt

let test_stack_poly2_good () =
  let stack = [ htype (TyFreeVar tx); htype (TyFreeVar ts) ] in
  let ty =
    poly_ty (fresh "X") (fun _ -> poly_ty (fresh "Y") (fun _ -> TyFreeVar toto))
  in
  let expected_ty = TyFreeVar toto in
  test_typechecking_stack expected_ty stack ty VarMap.empty

let test_stack_both () =
  let stack = [ hfun (Var x); htype (TyFreeVar tx) ] in
  let ty = TyFreeVar tt => poly_ty (fresh "X") (fun _ -> TyFreeVar toto) in
  let ctxt = VarMap.singleton x (TyFreeVar tt) in
  let expected_ty = TyFreeVar toto in
  test_typechecking_stack expected_ty stack ty ctxt

let test_stack_if_good () =
  let ty = TyFreeVar tx in
  let stack = [ hif (Base (Var x)) (Base (Var y)) ] in
  let ctxt = VarMap.singleton x ty in
  let ctxt = VarMap.add y ty ctxt in
  test_typechecking_stack ty stack TyBool ctxt

let test_stack_if_bad () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let stack = [ hif (Base (Var x)) (Base (Var y)) ] in
  let ctxt = VarMap.singleton x ty in
  let ctxt = VarMap.add y ty2 ctxt in
  test_fail_typecheck_stack
    (Printf.sprintf
       "Type error!\n\
        Branches do not have the same type\n\
        Branch: %s\n\
        Received type: %s\n\
        Expected %s" (pa y) (pa ts) (pa tx))
    stack TyBool ctxt
