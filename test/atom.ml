open Libfun
open Atom

let test_equal_atom a1 a2 =
  let check_atom = Alcotest.testable pp equal in
  Alcotest.(check check_atom) (Format.asprintf "%a" Atom.pp a1) a1 a2

let test_lt_atom a1 a2 =
  Alcotest.(check bool) "less than" true (compare a1 a2 < 0)

let test_atom_eq1 () =
  test_equal_atom
    { identifier = "a"; number = 1 }
    { identifier = "a"; number = 1 }

let test_atom_eq2 () =
  test_equal_atom
    { identifier = "a"; number = 1 }
    { identifier = "b"; number = 1 }

let test_atom_lt1 () =
  test_lt_atom { identifier = "a"; number = 1 } { identifier = "a"; number = 2 }

let test_atom_lt2 () =
  test_lt_atom { identifier = "a"; number = 1 } { identifier = "b"; number = 2 }
