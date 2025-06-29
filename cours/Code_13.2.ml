type variable = A | B | C | D
type formula =
  | Var of variable 
  | Const of bool   
  | Not of formula  
  | And of formula * formula 
  | Or of formula * formula 
