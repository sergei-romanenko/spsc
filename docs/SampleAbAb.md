## Input code

```
ab(A(x)) = B(ab(x));
ab(B(x)) = A(ab(x));
abab(x) = ab(ab(x));
```

## Start function
```
abab
```

## Supercompiled code

```
ab1(A(a)) = A(ab1(a));
ab1(B(a)) = B(ab1(a));
abab(a) = ab1(a);
```

## Notes

This sample shows that SPSC is able to transform a 2-pass algorithm to a 1-pass 
one. SPSC notes that
```
ab(ab(A(x))) ==> ab(B(ab(x))) ==> A(ab(ab(x))
ab(ab(B(x))) ==> ab(A(ab(x))) ==> B(ab(ab(x))
```
and creates a recursively defined function `ab1` such that calling `ab1(...)`
produces the same result as calling `ab(ab(...))`.
