type poly =
  | Cst of int
  | X_Pow of int
  | Add of poly * poly
  | Mul of poly * poly

let rec dx p =
  match p with
  | Cst _ -> Cst 0
  | X_Pow n -> Mul (Cst n, X_Pow (n-1))
  | Add (p1, p2) -> Add (dx p1, dx p2)
  | Mul (p1, p2) -> Add (Mul (dx p1, p2), Mul (p1, dx p2))

let rec poly_to_string p =
  match p with
  | Cst c -> string_of_int c
  | X_Pow n -> "X ^ " ^ string_of_int n
  | Add (p1, p2) -> "(" ^ poly_to_string p1 ^ " + " ^ poly_to_string p2 ^ ")"
  | Mul (p1, p2) -> poly_to_string p1 ^ " * " ^ poly_to_string p2
                      
let rec simplify p =
  let rec aux p = 
    match p with
    | Cst n -> Cst n  (* Les constantes sont déjà simplifiées *)
    | Add (p1, p2) ->
      let sp1 = simplify p1 in
      let sp2 = simplify p2 in
      (match (sp1, sp2) with
      | (Cst 0, _) -> sp2  (* Add(0, p) -> p *)
      | (_, Cst 0) -> sp1  (* Add(p, 0) -> p *)
      | (Cst n1, Cst n2) -> Cst (n1 + n2)  (* Add(Cst n1, Cst n2) -> Cst(n1+n2) *)
      | (Cst n1, Add(Cst n2, p)) -> Add(Cst (n1 + n2), simplify p) 
      | _ -> Add(sp1, sp2))
    | Mul (p1, p2) ->
      let sp1 = simplify p1 in
      let sp2 = simplify p2 in
      (match (sp1, sp2) with
      | (Cst 0, _) | (_, Cst 0) -> Cst 0  (* Mul(0, p) -> 0, Mul(p, 0) -> 0 *)
      | (Cst 1, _) -> sp2  (* Mul(1, p) -> p *)
      | (_, Cst 1) -> sp1  (* Mul(p, 1) -> p *)
      | (Cst n1, Cst n2) -> Cst (n1 * n2)  (* Mul(Cst n1, Cst n2) -> Cst(n1*n2) *)
      | (Cst n1, Mul(Cst n2, p)) -> Mul(Cst (n1 * n2), simplify p) 
      | _ -> Mul(sp1, sp2))
    | X_Pow n ->
      match n with
      | 0 -> Cst 1  (* Pow(X, 0) -> 1 *)
      | _ -> X_Pow n
  in
  let sp = aux p in
    if sp = p then p else simplify sp

let () =
  let p = Add (Add ( Mul (Cst 4,X_Pow 3), Mul (Cst 3,X_Pow 1)), Cst (-7)) in 
  Printf.printf "P(x) = %s\n" (poly_to_string p);
  Printf.printf "P'(x) = %s\n" (poly_to_string (dx p));
  Printf.printf "P'(x) = %s\n" (poly_to_string (simplify (dx p)));
  
