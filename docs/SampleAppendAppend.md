## Input code

```
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));

appendXYaZ(xs, ys, zs) = append(append(xs, ys), zs);
```
## Start Function

```
appendXYaZ
```

## Supercompiled code

```
append1(Nil(), a, b) = append2(a, b);
append1(Cons(a, b), c, d) = Cons(a, append1(b, c, d));
append2(Cons(a, b), c) = Cons(a, append2(b, c));
append2(Nil(), a) = a;

appendXYaZ(a, b, c) = append1(a, b, c);
```

## Notes

This sample shows transforming a 2-pass algorithm to a 1-pass one for binary
functions.
