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

The power of Kernel, however, comes from the `$vau` operative. An operative does not get its arguments evaluated. This allows us to define syntax like `$cond` or `$let`, which be a second-class citizen in Scheme:

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
``
