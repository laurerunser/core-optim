type ty = 
  (* type variable *)
  | TyVar of tyvar  

  (* type application: TyFun(S, T) is S -> T *)
  | TyFun of ty * ty  

  (* polymorphic type: PolymorphicType(X, T) is forAll X. T *)
  | PolymorphicType of tyvar * ty 

  (* tuple: T_0 * ... * T_k *)  
  | TyTuple of ty list            
[@@deriving show]

and tyvar = string  (* type variable *)
[@@deriving show]
