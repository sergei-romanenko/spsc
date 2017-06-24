See also: [SampleEq2](SampleEq2.md).

## Input code

```
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);

eqZ(Z) = True;
eqZ(S(x)) = False;

eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

eqxx(x) = eq(x, x);
```

## Start function

```
eqxx
```

## Supercompiled code
```
eq1(Z()) = True();
eq1(S(a)) = eq1(a);

eqxx(a) = eq1(a);
```

## Notes

In this case, `False` is never returned by the function `eq1` produced by SPSC.
Hence, SPSC has "proved the theorem" that "X=X cannot be false".

An illustration to the idea that a supercompiler may be used for "theorem proving".
