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
let z = fresh "z"
let tx = fresh "X"
let ty = fresh "Y"
let ts = fresh "S"
let tt = fresh "T"

let test_print_fun_frame () =
  let s = [ hfun (Var toto) ] in
  test_pretty_print (Printf.sprintf "_ %s\n" (pa toto)) s

let test_print_ty_frame () =
  let s = [ htype (TyFreeVar toto) ] in
  test_pretty_print (Printf.sprintf "_[%s]\n" (pa toto)) s

let test_print_empty_stack () = test_pretty_print "" []
(* should print nothing *)

let test_print_big_stack () =
  let s =
    [ hfun (Var toto); htype (TyFreeVar x); hfun (Var y); htype (TyFreeVar x) ]
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
    expected
    (plug stack (empty_scope term))

let test_plug_fun1 () =
  let arg = Var x in
  let stack = [ hfun arg ] in
  let f = fn y (TyFreeVar ty) (fun _ -> Base (Var toto)) in
  test_equal_plug_term (f $ arg) stack f

let test_plug_fun2 () =
  let arg1, arg2 = (Var y, Var x) in
  let stack = [ hfun arg1; hfun arg2 ] in
  let f =
    fn (fresh "z1") (TyFreeVar tx) (fun _ ->
        fn (fresh "z2") (TyFreeVar tx) (fun _ -> Base (Var toto)))
  in
  test_equal_plug_term (f $ arg1 $ arg2) stack f

let test_plug_ty1 () =
  let arg = TyFreeVar tx in
  let stack = [ htype arg ] in
  let t = ty_fn ty (fun _ -> Base (Var toto)) in
  test_equal_plug_term (t $! arg) stack t

let test_plug_ty2 () =
  let arg1, arg2 = (TyFreeVar ty, TyFreeVar tx) in
  let stack = [ htype arg1; htype arg2 ] in
  let t =
    ty_fn (fresh "Z1") (fun _ -> ty_fn (fresh "Z2") (fun _ -> Base (Var toto)))
  in
  test_equal_plug_term (t $! arg1 $! arg2) stack t

let test_plug_mix () =
  let arg1, arg2 = (Var x, TyFreeVar tx) in
  let stack = [ hfun arg1; htype arg2 ] in
  let f =
    fn y (TyFreeVar ty) (fun _ -> ty_fn (fresh "Z") (fun _ -> Base (Var toto)))
  in
  test_equal_plug_term (f $ arg1 $! arg2) stack f
