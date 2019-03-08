# hscaml

toy OCaml interpreter in Haskell

## Build & Usage
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
