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

## Optimizations

While most of Kernel's features can be bootstrapped from a few primitives, it can lead to performance
issues. The biggest performance gain was gotten by rewriting the `$sequence` operative in OCaml. Further
performance gains were gotten by doing the same for `$vau` and `$lambda`.

```
% time ./creme-no-opt tests/tak.crm > /dev/null
./creme-no-opt tests/tak.crm > /dev/null  19.39s user 0.02s system 99% cpu 19.418 total

% time ./creme-opt-seq tests/tak.crm > /dev/null
./creme-opt-seq tests/tak.crm > /dev/null  5.13s user 0.01s system 99% cpu 5.142 total

% time ./creme-opt-seq-vau tests/tak.crm > /dev/null
./creme-opt-seq-vau tests/tak.crm > /dev/null  4.18s user 0.01s system 99% cpu 4.190 total

% time ./creme-opt-seq-vau-lambda tests/tak.crm > /dev/null
./creme-opt-seq-vau-lambda tests/tak.crm > /dev/null  2.39s user 0.01s system 99% cpu 2.394 total
```

## Todo

* Add <del>#o, #b, #x,</del> #e, #i numerical prefixes
* Add rational numbers to numerical tower
* Improve environment support
* Add to test cases
