# Creme

Creme is an implementation of a subset of the Kernel programming language in OCaml.

## Requirements

* OCaml 4.00+

## Examples

You can, of course, use Creme in a manner similar to you would with Scheme:

```
($define! map
  ($lambda (f ls)
    ($if (null? ls)
         ls
         (cons (f (car ls))
               (map f (cdr ls))))))
```

Which defines an applicative combiner `map`. An applicative combiner will have its arguments evaluated before being passed to it.

The power of Kernel, however, comes from the `$vau` operative. An operative does not get its arguments evaluated. Instead, we combine `$vau` with the `eval` operative, which allows explicity evaluation of forms. This allows us to define syntax like `$cond` or `$let`:

```
($define! $cond
  ($vau clauses env
    ($define! aux
      ($lambda ((test . body) . clauses)
        ($if (eval test env)
             (apply (wrap $sequence) body env)
             (apply (wrap $cond) clauses env))))
    ($if (null? clauses)
         #inert
         (apply aux clauses))))
```

This means that unlike in Scheme, where macros aren't first-class, we get something similar to macros as first-class citizens. Neat.

## Todo

* Add <del>#o, #b, #x,</del> #e, #i numerical prefixes
* Add rational numbers to numerical tower
* Improve environment support
* Add to test cases
* Speed up non-tail-recursive routines?

```
% time ./creme tests/fib.crm > /dev/null
./creme tests/fib.crm > /dev/null  5.57s user 0.01s system 99% cpu 5.582 total
% time ./creme tests/fib-tail.crm > /dev/null 
./creme tests/fib-tail.crm > /dev/null  0.00s user 0.00s system 86% cpu 0.008 total
```
