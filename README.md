# hscaml

toy OCaml interpreter in Haskell

## Build & Run
```
$ make
$ ./main
```

## Example
```
# let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2) in fib 10;;
- : int = 89
# let rec fact n = if n = 0 then 1 else n * fact (n - 1);;
val fact : int -> int = <fun>
# fact 10;;
- : int = 3628800
```

let polymorphism
```
# let f x = x;;
val f : t0 -> t0 = <fun>
# (f true, f 0);;
- : (bool * int) = (true, 0)
# (f f) f;;
- : t5 -> t5 = <fun>
# f (f f);;
- : t9 -> t9 = <fun>
```

pattern match
```
# match (1, 2) with
  | (x, y) -> x + y;;
- : int = 3
# let hd l = match l with
  | [] -> 0
  | x :: y -> x;;
val hd : int list -> int = <fun>
# hd [1;2;3];;
- : int = 1
# hd [];;
- : int = 0
```
