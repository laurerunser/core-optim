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
  test_typecheck ty ctxt (Var toto)

(* This test fails (as it should) because the variable `toto` is
   not in the VarMap *)
let test_typecheck_fail_not_in_map () =
  let toto = fresh "toto" in
  let term = Var toto in
  test_fail_typecheck
    (Format.sprintf "The variable %s was not in the type map\n"
       (Atom.pretty_print_atom toto))
    VarMap.empty term

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

let test_typecheck_fun_simple () =
  let tv = TyFreeVar ts in
  let tu = TyFreeVar tt in
  let f = fn x tv (fun _ -> Var y) in
  let ty = tv => tu in
  let ctxt = VarMap.singleton y tu in
  test_typecheck ty ctxt f

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
  test_fail_typecheck
    (Format.sprintf
       "Type error!\nTerm : %s\nExpected type: %s\nReceived type: %s\n"
       (Terms.to_string (Var a)) (Types.to_string tvx) (Types.to_string tv))
    ctxt (f $ Var a)

let test_typecheck_fun_apply_simple () =
  let tv = TyFreeVar ts in
  let tu = TyFreeVar tt in
  let f = fn x tv (fun _ -> Var y) in
  let ctxt = VarMap.add y tu (VarMap.singleton a tv) in
  test_typecheck tu ctxt (f $ Var a)

(**************************)
(* Let *)

let test_typecheck_let_simple () =
  let tv = TyFreeVar ts in
  let tu = TyFreeVar tt in
  let l = letin x (Var a) (fun _ -> Var y) in
  let ctxt = VarMap.add y tv (VarMap.singleton a tu) in
  test_typecheck tv ctxt l

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
let test_typecheck_type_abstract_simple () =
  let tv = TyFreeVar tt in
  let t = ty_fn tx (fun _ -> Var a) in
  let ty = poly_ty tx (fun _ -> tv) in
  let ctxt = VarMap.singleton a tv in
  test_typecheck ty ctxt t

(* fun [X] = fun (a:X) -> a *)
let test_typecheck_type_abstract () =
  let poly_id = ty_fn tx (fun t -> fn x t (fun z -> z)) in
  let ty = poly_ty tt (fun x -> x => x) in
  test_typecheck ty VarMap.empty poly_id

(**************************)
(* TypeApply *)
let test_typecheck_type_apply_simple () =
  let ty = TyFreeVar tx in
  let ty2 = TyFreeVar ts in
  let ty_fun = ty_fn tx (fun _ -> Var a) in
  let term = ty_fun $! ty2 in
  let ctxt = VarMap.singleton a ty in
  test_typecheck ty2 ctxt term

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

(**********************************************************************)
(* stack Typechecking *)

let test_typechecking_stack msg expected stack term ctxt =
  Alcotest.(check check_ty)
    msg expected
    (Typechecker.synth_stack stack term ctxt)

let test_typechecking_frame msg expected frame term ctxt =
  Alcotest.(check check_ty)
    msg expected
    (Typechecker.synth_frame frame term ctxt)

let test_fail_typecheck_frame msg frame t ctxt =
  Alcotest.(
    check_raises "failure" (Failure msg) (fun () ->
        let _ = synth_frame frame t ctxt in
        ()))

let test_fail_typecheck_stack msg stack t ctxt =
  Alcotest.(
    check_raises "failure" (Failure msg) (fun () ->
        let _ = synth_stack stack t ctxt in
        ()))

let test_frame1_good () =
  let frame = HoleFun (Var x) in
  let f = fn x (TyFreeVar ts) (fun _ -> Var toto) in
  let ctxt = VarMap.singleton x (TyFreeVar ts) in
  let ctxt = VarMap.add toto (TyFreeVar tt) ctxt in
  let expected_ty = TyFreeVar tt in
  test_typechecking_frame "HoleFun frame" expected_ty frame f ctxt

let test_frame1_bad () =
  (* same test than the previous one, except the type of `x` in the ctxt *)
  let frame = HoleFun (Var x) in
  let f = fn x (TyFreeVar ts) (fun _ -> Var toto) in
  let ctxt = VarMap.singleton x (TyFreeVar tx) in
  let ctxt = VarMap.add toto (TyFreeVar tt) ctxt in
  test_fail_typecheck_frame
    (Printf.sprintf
       "Type error!\n\
        Frame: %s\n\
        Expected a %s->_ type\n\
        Received type: %s -> %s\n"
       (Stack.to_string [ frame ])
       (pa tx) (pa ts) (pa tt))
    frame f ctxt

let test_frame2_good () =
  let frame = HoleType (TyFreeVar tx) in
  let f = ty_fn ts (fun _ -> Var toto) in
  let ctxt = VarMap.singleton toto (TyFreeVar ts) in
  let expected_ty = TyFreeVar tx in
  test_typechecking_frame "HoleType frame" expected_ty frame f ctxt

let test_frame2_bad () =
  let frame = HoleType (TyFreeVar tx) in
  let f = Var toto in
  let ctxt = VarMap.singleton toto (TyFreeVar ts) in
  test_fail_typecheck_frame
    (Printf.sprintf
       "Type error!\nFrame: %s\nExpected a polymorphic type\nReceived type: %s"
       (Stack.to_string [ frame ])
       (pa ts))
    frame f ctxt

let test_stack1_good () =
  let stack = [ HoleFun (Var x) ] in
  let f = fn x (TyFreeVar ts) (fun _ -> Var toto) in
  let ctxt = VarMap.singleton x (TyFreeVar ts) in
  let ctxt = VarMap.add toto (TyFreeVar tt) ctxt in
  let expected_ty = TyFreeVar tt in
  test_typechecking_stack "stack with 1 HoleFun frame" expected_ty stack f ctxt

let test_stack1_bad () =
  (* same test than the previous one, except the type of `x` in the ctxt *)
  let stack = [ HoleFun (Var x) ] in
  let f = fn x (TyFreeVar ts) (fun _ -> Var toto) in
  let ctxt = VarMap.singleton x (TyFreeVar tx) in
  let ctxt = VarMap.add toto (TyFreeVar tt) ctxt in
  test_fail_typecheck_stack
    (Printf.sprintf
       "Type error!\n\
        Frame: %s\n\
        Expected a %s->_ type\n\
        Received type: %s -> %s\n"
       (Stack.to_string stack) (pa tx) (pa ts) (pa tt))
    stack f ctxt

let test_stack2_good () =
  let stack = [ HoleFun (Var y); HoleFun (Var x) ] in
  let f =
    fn x (TyFreeVar tx) (fun _ -> fn y (TyFreeVar ts) (fun _ -> Var toto))
  in
  let ctxt = VarMap.singleton toto (TyFreeVar toto) in
  let ctxt = VarMap.add y (TyFreeVar ts) ctxt in
  let ctxt = VarMap.add x (TyFreeVar tx) ctxt in
  let expected_ty = TyFreeVar toto in
  test_typechecking_stack "stack with 2 HoleFun frame" expected_ty stack f ctxt

let test_stack2_bad () =
  let stack = [ HoleFun (Var y); HoleFun (Var x) ] in
  (* this one will fail because the return type of f is not a function,
     which means it cannot plug the next hole *)
  let f = fn x (TyFreeVar tx) (fun _ -> Var toto) in
  let ctxt = VarMap.singleton toto (TyFreeVar toto) in
  let ctxt = VarMap.add y (TyFreeVar ts) ctxt in
  let ctxt = VarMap.add x (TyFreeVar tx) ctxt in
  test_fail_typecheck_stack
    (Printf.sprintf
       "Type error!\nFrame: %s\nExpected a %s->_ type\nReceived type: %s\n"
       (Stack.to_string [ List.hd stack ])
       (pa ts) (pa toto))
    stack f ctxt
