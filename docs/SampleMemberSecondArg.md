See also: [SampleMemberFirstArg](SampleMemberFirstArg.md),
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

memberZSZ(x) = member(x, Cons(Z, Cons(S(Z), Nil)));
```

## Start function

```
memberZSZ
```

## Supercompiled code

```
memberEq1(Z()) = True();
memberEq1(S(a)) = memberEq2(a);
memberEq2(Z()) = True();
memberEq2(S(a)) = False();

memberZSZ(a) = memberEq1(a);
```

## Notes

`member(x, list)` returns `True`, if `x` appears in the list `list`,
and `False` otherwise.

`memberZSZ(x)` returns `True`, if `x` appears in the list consisting of `Z` and `S(Z)`,
and `False` otherwise.

Upon supercompiling the source program, we get a version of `member` 
"specialized" with respect to the value of its second argument. Besides, the 
target program has the structure that is "simpler" than that of the source 
program. The source program containes recursive function definitions, while 
there is no recursion in the target program. (Or, in more "imperative" terms, 
there is no "loops".)
