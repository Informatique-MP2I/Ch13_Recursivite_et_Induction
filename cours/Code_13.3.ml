let rec evaluate formula a b c d =
  match formula with
  | Var v -> (match v with 
              | A -> a
              | B -> b
              | C -> c
              | D -> d)
  | Const b -> b
  | Not f -> not (evaluate f a b c d)
  | And(f1, f2) -> (evaluate f1 a b c d) && (evaluate f2 a b c d)
  | Or(f1, f2) -> (evaluate f1 a b c d) || (evaluate f2 a b c d)  
