See also: [SampleMemberSecondArg](SampleMemberSecondArg.md),
[SampleMember](SampleMember.md).

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

memberSZ(list) = member(S(Z), list);
```

## Start function

```
memberSZ
```

## Supercompiled code

```
memberEq1(Z(), a) = memberSel1(a);
memberEq1(S(a), b) = memberEq2(a, b);

memberEq2(Z(), a) = True();
memberEq2(S(a), b) = memberSel1(b);

memberSel1(Nil()) = False();
memberSel1(Cons(a, b)) = memberEq1(a, b);

memberSZ(a) = memberSel1(a);
```

## Notes


`member(x, list)` returns `True`, if `x` appears in the list `list`, and `False`
otherwise.

`memberSZ(list)` returns `True`, if `S(Z)` appears in the list `list`, and `False`
otherwise.

Upon supercompiling the source program, we get a version of `member` "specialized" with
respect to the value of its first argument. Besides, the target program has the structure
that is "simpler" than that of the source program. The target program
is "more imperative" in comparison with the "more functional"
source program, because its structure resembles that of a "finite-state machine".
