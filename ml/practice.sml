(* some classics in sml *)

fun fact n =
  if n = 0
  then 1
  else n * fact (n - 1);

fun facti (n, p) =
  if n = 0
  then p
  else facti(n - 1, n * p);

fun gcd (m, n) =
  if m = 0
  then n
  else gcd(n mod m, m);

fun power (x, k) : real =
  if k = 1
    then x
  else if k mod 2 = 0
    then   power (x*x, k div 2)
  else x * power (x*x, k div 2);
