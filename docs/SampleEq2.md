See also: [SampleEqTheSame](SampleEqTheSame.md).

## Input code

```
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

eq2(x) = eq(S(S(Z)), x);
```

## Start function

```
eq2
```

## Supercompiled code

```
eq2(a) = eqS1(a);

eqS1(Z()) = False();
eqS1(S(a)) = eqS2(a);
eqS2(Z()) = False();
eqS2(S(a)) = eqZ1(a);
eqZ1(Z()) = True();
eqZ1(S(a)) = False();
```

## Notes

This sample shows transforming a binary function into a unary one.

Supposing that non-negative integers are represented as
```
Z, S(Z), S(S(Z)), S(S(S(Z))), ...
```
the function `eq` tests two numbers for equality.
And `eq2` returns True iff its argument is equal to `S(S(Z))`.

SPSC _specializes_ `eq` with respect to its first argument (equal to `S(S(Z))`,
to produce a unary function.

We may consider `eq` as an _interpreter_, its first argument being a "known
program" in the "language of non-negative integers", and the second argument 
"unknown input data". Then the function `eqS1` produced by SPSC can be
considered as the result of "compiling the program `S(S(Z))`" to a program
written in the functional language SPSC deals with. An example of the "first
Futamura projection"...
