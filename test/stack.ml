open Libfun
open Stack
open Atom
open Terms
open Types
open Syntax

(************************************************************)
(* Pretty Print *)
let test_pretty_print expected stack =
  Alcotest.(check string) expected expected (Stack.to_string stack)

let pa = pretty_print_atom
let toto = fresh "toto"
let x = fresh "x"
let y = fresh "y"
let tx = fresh "X"
let ty = fresh "Y"

let test_print_fun_frame () =
  let s = [ HoleFun (Var toto) ] in
  test_pretty_print (Printf.sprintf "_ %s\n" (pa toto)) s

let test_print_ty_frame () =
  let s = [ HoleType (TyFreeVar toto) ] in
  test_pretty_print (Printf.sprintf "_[%s]\n" (pa toto)) s

let test_print_empty_stack () = test_pretty_print "" []
(* should print nothing *)

let test_print_big_stack () =
  let s =
    [
      HoleFun (Var toto);
      HoleType (TyFreeVar x);
      HoleFun (Var y);
      HoleType (TyFreeVar x);
    ]
  in
  test_pretty_print
    (Printf.sprintf "_ %s\n_[%s]\n _ %s\n  _[%s]\n" (pa toto) (pa x) (pa y)
       (pa x))
    s

(*****************************************************************************)
(* plug *)

let test_equal_plug_term expected stack term =
  let check_term = Alcotest.testable pp_term equal_term in
  Alcotest.(check check_term)
    (Format.asprintf "%a" pp_term expected)
    expected (plug stack term)

let test_plug_fun1 () =
  let arg = Var x in
  let stack = [ HoleFun arg ] in
  let f = fn y (TyFreeVar ty) (fun _ -> Atom (Var toto)) in
  test_equal_plug_term (f $ arg) stack f

let test_plug_fun2 () =
  let arg1, arg2 = (Var y, Var x) in
  let stack = [ HoleFun arg1; HoleFun arg2 ] in
  let f =
    fn (fresh "z1") (TyFreeVar tx) (fun _ ->
        fn (fresh "z2") (TyFreeVar tx) (fun _ -> Atom (Var toto)))
  in
  test_equal_plug_term (f $ arg1 $ arg2) stack f

let test_plug_ty1 () =
  let arg = TyFreeVar tx in
  let stack = [ HoleType arg ] in
  let t = ty_fn ty (fun _ -> Atom (Var toto)) in
  test_equal_plug_term (t $! arg) stack t

let test_plug_ty2 () =
  let arg1, arg2 = (TyFreeVar ty, TyFreeVar tx) in
  let stack = [ HoleType arg1; HoleType arg2 ] in
  let t =
    ty_fn (fresh "Z1") (fun _ -> ty_fn (fresh "Z2") (fun _ -> Atom (Var toto)))
  in
  test_equal_plug_term (t $! arg1 $! arg2) stack t

let test_plug_mix () =
  let arg1, arg2 = (Var x, TyFreeVar tx) in
  let stack = [ HoleFun arg1; HoleType arg2 ] in
  let f =
    fn y (TyFreeVar ty) (fun _ -> ty_fn (fresh "Z") (fun _ -> Atom (Var toto)))
  in
  test_equal_plug_term (f $ arg1 $! arg2) stack f
