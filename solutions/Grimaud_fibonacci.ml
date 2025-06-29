let rec fib n =
  if n<=1 then
    n
  else
    fib (n-1) + fib (n-2)

let () =
  let n = 15 in
  Printf.printf "fib(%d) = %d\n" n (fib n)
