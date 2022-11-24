(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* The tests *)
let test_ppshow () = 
  Alcotest.(check string) "same string" "(Term.Var \"toto\")" (Libfun.Term.(Format.asprintf "%a" pp_term (Var("toto")))) 

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "test Var", [ test_case "String mashing" `Quick test_ppshow  ];
    ]
    