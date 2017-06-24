# The Language the supercompiler deals with

## A Simple Lazy First-Order Language (SLL)

Our supercompiler deals with programs written in a simple first-order
functional language (SLL). The intended operational semantics of the language
is normal-order graph reduction to weak head normal form.

## The lexical structure of SLL

Constructor names, function names and variables are represented by identifiers.

An identifier is a sequence of latin letters and digits, the first character
being a letter.

A function name or a variable must start with a small letter.

A constructor name must start with a capital letter.


## The syntax of programs

In the following {**A**} means the construct **A** repeated zero or more times,
and [**A**] means the construct **A** is optional.


---


_program_ ::= {_functionDefinition_}

_functionDefinition_ ::= _fFunctionDefinition_ | _gFunctionDefinition_

_fFunctionDefinition_ ::= _fRule_`;`

_gFunctionDefinition_ ::= {_gRule_`;`}

_fRule_ ::= _functionName_`(`_fParameters_`) =` _expression_

_gRule_ ::= _functionName_`(`_gParameters_`) =` _expression_

_fParameters_ ::= [_variable_ {`,` _variable_}]

_gParameters_ ::= _pattern_ {`,` _variable_}

_pattern_ ::= _constructorName_`(`[_variable_ {`,` _variable_}]`)`

_expression_ ::= _variable_ | _constructor_ | _fFunctionCall_ | _gFunctionCall_

_constructor_ ::= _constructorName_[`(`[_expression_ {`,` _expression_}]`)`]

_fFunctionCall_ ::= _functionName_`(`[_expression_ {`,` _expression_}]`)`


_gFunctionCall_ ::= _functionName_`(`_expression_ {`,` _expression_}`)`


---


We require that no two patterns in a g-function definition contain
the same constructor `c`, that no variable occur more than once in
a left side of a rule, and that all variables on the right side
of a rule be present in its left side.

In a program, all occurrences of a constructor, an f-function or a g-function
must have the same arity.

Note that if a constructor is a zero-arity one the parentheses can be omitted:
 `B()` and `B` are equivalent.

## Examples

### Appending lists

In the following program, the g-function `Append` appends 2 lists, and
the f-function `Append3` appends 3 lists.

```
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));

append3(xs, ys, zs) = append(append(xs, ys), zs);
```

### Testing natural numbers for equality

Supposing that the natural numbers are represented in unary system
(Z represents zero, and S(n) the number n+1), the function `eq`
tests numbers for equality.

```
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);
```

Note that SLL doesn't allow rules like
```
eq(S(x), S(y)) = eq(x, y);
```

### A program that exploits the lazyness of SLL

The following program defines the function `member(x, ys)` that
tests that the number `x` appears in the list `ys`.

```
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

null(Nil) = True;
null(Cons(x, xs)) = False;

head(Cons(x, xs)) = x;
tail(Cons(x, xs)) = xs;

if(True, x, y) = x;
if(False, x, y) = y;

not(x) = if(x, False, True);
or(x, y) = if(x, True, y);
and(x, y) = if(x, y, False);

member(x, list) = and(not(null(list)), or(eq(x, head(list)), member(x, tail(list))));
```

## Notes

### Calls to undefined functions

As strange as it may seem, a program is allowed to contain calls to undefined functions, on condition that all calls to an undefined function have the same arity.

### Rationale

Partial process tree may have a leaf with label `g(C(...), ...)`, despite the 
fact that there is no rule in the definition of `g` corresponding to `c`. This 
means that an attempt to evaluate such a call would result in abnormal program 
termination. However, since the semantics of the language is lazy, such a call 
may never be evaluated.

For this reason spsc sometimes generates calls to g-functions whose domain is 
empty. Technically, a definition of such a function has zero rules.
