
termes => `terms.ml`  
t,u ::=   
    | x                 (variable)  
    | fun (x:T) = t     (abstraction)  
    | t u               (function application)  
    | let x = t i u     (let binding)  
    | fun[X]=t          (type abstraction)  
    | t[T]              (type application)  
    | (t:T)             (type annotation)  

types => `types.ml`  
S,T ::=  
    | X                 (type variable)  
    | S -> T            (function type)  
    | $\forall$ X.T     (polymorphic type)  
    | $T_0$ * ... * $T_k$   (tuples)  
