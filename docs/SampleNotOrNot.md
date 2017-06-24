## Input code

```
if(True, x, y) = x;
if(False, x, y) = y;

not(x) = if(x, False, True);

or(x, y) = if(x, True, y);

and(x, y) = if(x, y, False);

notOrNot(x, y) = not(or(not(x), not(y)));
```

## Start function

```
NotOrNot
```

## Supercompiled code

```
if1(True(), a) = if2(a);
if1(False(), a) = False();

if2(False()) = False();
if2(True()) = True();

notOrNot(a, b) = if1(a, b);
```

## Notes

Since the semantics of the language dealt with by SPSC is lazy,
the source program may contain definitions of "control structures"
(`if`, `and`, `or` and like that).

SPSC is able to remove such "control structures", to produce
a "flatter" and "more imperative" program.
