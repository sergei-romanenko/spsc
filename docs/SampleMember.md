See also: [SampleMemberFirstArg](SampleMemberFirstArg.md),
[SampleMemberSecondArg](SampleMemberSecondArg.md).

## Input code

```
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

memberSel(Nil, x) = False;
memberSel(Cons(y, ys), x) = memberEq(eq(x, y), x, ys);

memberEq(True, x, ys) = True;
memberEq(False, x, ys) = memberSel(ys, x);

member(x, ys) = memberSel(ys, x);
```

## Start function

```
member
```

## Supercompiled code

```
memberEq1(Z(), a, b, c) = memberEq3(a, b, c);
memberEq1(S(a), b, c, d) = memberEq2(b, a, c, d);
memberEq2(Z(), a, b, c) = memberSel1(c, b);
memberEq2(S(a), b, c, d) = memberEq1(b, a, c, d);
memberEq3(S(a), b, c) = memberSel1(c, b);
memberEq3(Z(), a, b) = True();
memberSel1(Nil(), a) = False();
memberSel1(Cons(a, b), c) = memberEq1(c, a, c, b);

member(a, b) = memberSel1(b, a);
```

## Notes

`member(x, list)` returns `True`, if `x` appears in the list `list`, and `False`
otherwise.

Upon supercompiling the source program, we get a version of `member`
that is "simpler" than the original one. The target program
is "more imperative" in comparison with the "more functional"
source program, because its structure resembles that of a "finite-state machine".
