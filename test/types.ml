open Libfun
open Syntax
open Types
open Atom

let check_ty = Alcotest.testable pp_ty equal_ty

let test_equal_ty expected ty =
  Alcotest.(check check_ty) (Format.asprintf "%a" pp_ty expected) expected ty

let x = fresh "X"
let y = fresh "Y"
let z = fresh "Z"
let s = fresh "S"
let tt = fresh "T"
let u = fresh "U"

(*****************************************************************************)
(* equal_ty *)
let test_pp_equal_ty_poly1 () =
  test_equal_ty
    (PolymorphicType (x, TyBoundVar 0))
    (PolymorphicType (y, TyBoundVar 0))

let test_pp_equal_ty_poly2 () =
  test_equal_ty
    (PolymorphicType (x, PolymorphicType (y, TyFun (TyBoundVar 0, TyBoundVar 1))))
    (PolymorphicType (y, PolymorphicType (x, TyFun (TyBoundVar 0, TyBoundVar 1))))

let test_pp_equal_ty_poly3 () =
  test_equal_ty
    (PolymorphicType (x, PolymorphicType (y, TyFun (TyBoundVar 0, TyFreeVar tt))))
    (PolymorphicType
       ( fresh "X",
         PolymorphicType (fresh "Y", TyFun (TyBoundVar 0, TyFreeVar tt)) ))

(*****************************************************************************)
(* smart constructors *)
let test_poly_ty1 () =
  test_equal_ty
    (PolymorphicType
       ( fresh "X",
         PolymorphicType (fresh "Y", TyFun (TyBoundVar 1, TyBoundVar 0)) ))
    (poly_ty (fresh "X") (fun x -> poly_ty (fresh "Y") (fun y -> x => y)))

let test_poly_ty2 () =
  test_equal_ty
    (PolymorphicType
       ( fresh "X",
         TyFun
           ( TyFreeVar s,
             PolymorphicType
               ( fresh "Y",
                 TyTuple
                   [
                     TyBoundVar 0;
                     PolymorphicType
                       (fresh "Z", TyFun (TyBoundVar 2, TyBoundVar 0));
                   ] ) ) ))
    (poly_ty (fresh "X") (fun x ->
         TyFreeVar s
         => poly_ty (fresh "Y") (fun y ->
                tuple [ y; poly_ty (fresh "Z") (fun z -> x => z) ])))

let test_poly_ty3 () =
  test_equal_ty (* "forall X. (0 -> (forall X. (0 -> 1)))" *)
    (PolymorphicType
       ( fresh "X",
         TyFun
           ( TyBoundVar 0,
             PolymorphicType (fresh "X", TyFun (TyBoundVar 0, TyBoundVar 1)) )
       ))
    (poly_ty (fresh "X") (fun x -> x => poly_ty (fresh "X") (fun y -> y => x)))

let test_poly_ty4 () =
  test_equal_ty (* "forall X. (0 -> (forall X. (0 -> 0)))" *)
    (PolymorphicType
       ( x,
         TyFun
           ( TyBoundVar 0,
             PolymorphicType (x, TyFun (TyBoundVar 0, TyBoundVar 0)) ) ))
    (poly_ty x (fun y -> y => poly_ty x (fun z -> z => y)))

(*****************************************************************************)
(* abstract *)
let test_abstract expected v ty =
  let msg = Format.asprintf "abstract %a with %a" pp_ty ty pp v in
  let ty = abstract v ty in
  Alcotest.(check check_ty) msg expected ty

let test_abstract_fn () =
  test_abstract
    (PolymorphicType (fresh "X", TyBoundVar 0 => TyFreeVar tt))
    s
    (TyFreeVar s => TyFreeVar tt)

let test_abstract_poly1 () =
  test_abstract
    (PolymorphicType
       ( fresh "X",
         TyBoundVar 0
         => PolymorphicType
              (fresh "Y", tuple [ TyFreeVar tt; TyBoundVar 0; TyBoundVar 1 ]) ))
    s
    (TyFreeVar s
    => PolymorphicType
         (fresh "X", tuple [ TyFreeVar tt; TyBoundVar 0; TyFreeVar s ]))

let test_abstract_poly2 () =
  test_abstract
    (PolymorphicType
       ( fresh "X",
         TyBoundVar 0
         => PolymorphicType
              ( fresh "Y",
                tuple
                  [
                    PolymorphicType (fresh "Z", TyBoundVar 0 => TyBoundVar 0);
                    TyBoundVar 0;
                    TyBoundVar 1;
                  ] ) ))
    s
    (TyFreeVar s
    => PolymorphicType
         ( fresh "X",
           tuple
             [
               PolymorphicType (fresh "Y", TyBoundVar 0 => TyBoundVar 0);
               TyBoundVar 0;
               TyFreeVar s;
             ] ))

let test_abstract_poly3 () =
  test_abstract
    (PolymorphicType
       ( fresh "X",
         PolymorphicType
           ( fresh "Y",
             TyBoundVar 0
             => PolymorphicType
                  ( fresh "Z",
                    tuple
                      [
                        PolymorphicType
                          ( fresh "X",
                            TyBoundVar 0 => TyBoundVar 2 => TyBoundVar 1 );
                        TyBoundVar 1;
                        TyBoundVar 0;
                      ] ) ) ))
    s
    (poly_ty (fresh "X") (fun x ->
         x
         => poly_ty (fresh "Y") (fun y ->
                tuple [ poly_ty (fresh "Z") (fun z -> z => x => y); x; y ])))

(*****************************************************************************)
(* fill *)
let test_fill expected ty v =
  let msg = Format.asprintf "fill %a with %a" pp_ty ty pp_ty v in
  let ty = fill ty v in
  Alcotest.(check check_ty) msg expected ty

let test_fill_fn () =
  test_fill
    (TyFreeVar s => TyFreeVar tt)
    (poly_ty (fresh "X") (fun x -> x => TyFreeVar tt))
    (TyFreeVar s)

let test_fill_poly1 () =
  test_fill
    (TyFreeVar s => poly_ty (fresh "Y") (fun y -> tuple [ TyFreeVar tt; y ]))
    (poly_ty (fresh "X") (fun x ->
         TyFreeVar s => poly_ty (fresh "Y") (fun y -> tuple [ x; y ])))
    (TyFreeVar tt)

let test_fill_poly2 () =
  test_fill
    (TyFreeVar tt
    => poly_ty (fresh "X") (fun y ->
           tuple
             [
               poly_ty (fresh "Y") (fun z -> z => TyFreeVar tt); y; TyFreeVar tt;
             ]))
    (poly_ty (fresh "X") (fun x ->
         x
         => poly_ty (fresh "Y") (fun y ->
                tuple [ poly_ty (fresh "X") (fun z -> z => x); y; x ])))
    (TyFreeVar tt)

let test_fill_poly3 () =
  test_fill
    (TyFreeVar tt
    => poly_ty (fresh "X") (fun y ->
           tuple
             [
               poly_ty (fresh "Y") (fun z -> z => TyFreeVar tt => y);
               TyFreeVar tt;
               y;
             ]))
    (poly_ty (fresh "X") (fun x ->
         x
         => poly_ty (fresh "Y") (fun y ->
                tuple [ poly_ty (fresh "Z") (fun z -> z => x => y); x; y ])))
    (TyFreeVar tt)

(*****************************************************************************)
(* Pretty print *)
let test_pretty_print expected ty =
  Alcotest.(check string) expected expected (to_string ty)

let pa = pretty_print_atom

let test_print_ty_fun_simple () =
  let t = fv x => fv y in
  test_pretty_print (Format.sprintf "%s -> %s" (pa x) (pa y)) t

let test_print_ty_fun_double_left () =
  let t = fv x => fv y => fv z in
  test_pretty_print (Format.sprintf "(%s -> %s) -> %s" (pa x) (pa y) (pa z)) t

let test_print_ty_fun_double_right () =
  let t = fv x => (fv y => fv z) in
  test_pretty_print (Format.sprintf "%s -> (%s -> %s)" (pa x) (pa y) (pa z)) t

let test_print_ty_fun_very_long () =
  (* to test line breaks and indents *)
  let v1 = fresh "long_variable_name1" in
  let v2 = fresh "long_variable_name2" in
  let v3 = fresh "long_variable_name3" in
  let s1 = pa v1 in
  let s2 = pa v2 in
  let s3 = pa v3 in
  let t =
    fv v1
    => (fv v2
       => (fv v1 => (fv v2 => (fv v1 => (fv v2 => (fv v1 => (fv v2 => fv v3))))))
       )
  in
  test_pretty_print
    (Format.sprintf
       "%s ->\n\
       \  (%s ->\n\
       \    (%s ->\n\
       \      (%s ->\n\
       \        (%s ->\n\
       \          (%s ->\n\
       \            (%s ->\n\
       \              (%s -> %s)))))))" s1 s2 s1 s2 s1 s2 s1 s2 s3)
    t

let test_print_poly_type_simple () =
  let t = poly_ty x (fun x -> x) in
  test_pretty_print (Format.sprintf "forall %s. 0" (pa x)) t

let test_print_poly_type_double () =
  let t = poly_ty x (fun x -> poly_ty y (fun _y -> x)) in
  test_pretty_print (Format.sprintf "forall %s. (forall %s. 1)" (pa x) (pa y)) t

let test_print_poly_type_complex () =
  let t =
    poly_ty x (fun x ->
        poly_ty y (fun y -> poly_ty z (fun z -> tuple [ z; y; x ])))
  in
  test_pretty_print
    (Format.sprintf "forall %s. (forall %s. (forall %s. (0 * 1 * 2)))" (pa x)
       (pa y) (pa z))
    t

let test_print_ty_tuple_simple () =
  let t = tuple [ fv x; fv y; fv z ] in
  test_pretty_print (Format.sprintf "%s * %s * %s" (pa x) (pa y) (pa z)) t

let test_print_ty_tuple_double () =
  let t = tuple [ tuple [ fv x; fv y; fv z ]; fv s; fv tt ] in
  test_pretty_print
    (Format.sprintf "(%s * %s * %s) * %s * %s" (pa x) (pa y) (pa z) (pa s)
       (pa tt))
    t

let test_print_ty_compose1 () =
  let t = poly_ty x (fun x -> x => fv s) => tuple [ fv tt; fv u ] in
  test_pretty_print
    (Format.sprintf "(forall %s. (0 -> %s)) -> (%s * %s)" (pa x) (pa s) (pa tt)
       (pa u))
    t

let test_print_ty_compose2 () =
  let t = tuple [ fv s; fv tt ] => poly_ty x (fun x -> x => fv u) in
  test_pretty_print
    (Format.sprintf "(%s * %s) -> (forall %s. (0 -> %s))" (pa s) (pa tt) (pa x)
       (pa u))
    t

let test_print_ty_compose3 () =
  let t = poly_ty x (fun x -> tuple [ fv s; fv tt ] => (x => fv u)) in
  test_pretty_print
    (Format.sprintf "forall %s. ((%s * %s) -> (0 -> %s))" (pa x) (pa s) (pa tt)
       (pa u))
    t

let test_print_ty_compose4 () =
  let t = poly_ty x (fun x -> tuple [ fv s; x => fv tt ] => fv u) in
  test_pretty_print
    (Format.sprintf "forall %s. ((%s * (0 -> %s)) -> %s)" (pa x) (pa s) (pa tt)
       (pa u))
    t
