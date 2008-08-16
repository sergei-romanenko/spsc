import unittest
from slparsing import parseAndValidate

text1 = """\
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));
appendXYaZ(xs, ys, zs) = append(append(xs, ys), zs);
"""

text2 = """\
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));
appendXYaX(xs, ys) = append(append(xs, ys), xs);
"""

text3 = """\
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));
appendXaYZ(xs, ys, zs) = append(xs, append(ys, zs));
"""

text4 = """\
ab(A(x)) = B(ab(x));
ab(B(x)) = A(ab(x));
abab(x) = ab(ab(x));
"""

text5 = """\
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);
eqxx(x) = eq(x, x);
"""

text6 = """\
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);
eqxSx(x) = eq(x, S(x));
"""

text7 = """\
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);
eqSxx(x) = eq(S(x), x);
"""

text8 = """\
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);
eqSZx(x) = eq(S(Z), x);
"""

text9 = """\
not(True) = False;
not(False) = True;
mapNot(Nil) = Nil;
mapNot(Cons(x, xs)) = Cons(not(x), mapNot(xs));

mapNotNot(xs) = mapNot(mapNot(xs));
"""

text10 = """\
not(True) = False;
not(False) = True;
mapNot(Nil) = Nil;
mapNot(Cons(x, xs)) = Cons(not(x), mapNot(xs));

mapNotNotNot(xs) = mapNot(mapNot(mapNot(xs)));
"""

text11 = """\
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
"""

text12 = """\
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
"""

text13 = """\
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
"""

text14 = """\
if(True, x, y) = x;
if(False, x, y) = y;
not(x) = if(x, False, True);
or(x, y) = if(x, True, y);
and(x, y) = if(x, y, False);
notornot(x, y) = not(or(not(x), not(y)));
"""

text15 = """\
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

memberLazy(x, list) = and(not(null(list)), or(eq(x, head(list)), memberLazy(x, tail(list))));
"""

text16 = """\
from(n) = Cons(n, from(S(n)));
head(Cons(x, xs)) = x;
mapAdd1(Nil) = Nil;
mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));
tail(Cons(x, xs)) = xs;
take(Z, xs) = Nil;
take(S(n), xs) = Cons(head(xs), take(n, tail(xs)));
takenm(n, m) = take(n, mapAdd1(from(m)));
"""

text17 = """\
from(n) = Cons(n, from(S(n)));
mapAdd1(Nil) = Nil;
mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));
mapfrom(n) = mapAdd1(from(n));
"""

text18 = """\
app(Nil, vs) = vs;
app(Cons(u, us), vs) = Cons(u, app(us, vs));
rev(Nil) = Nil;
rev(Cons(x, xs)) = app(rev(xs), Cons(x, Nil));
reverse(x)=rev(x);
"""

text19 = """\
flip(Leaf(z)) = Leaf(z);
flip(Branch(xt, yt)) = Branch(flip(yt), flip(xt));
flip3(zt) = flip(flip(flip(zt)));
"""


class GrammarTestCase(unittest.TestCase):
    def testCorrectCode(self):
        parseAndValidate(text1)
        parseAndValidate(text2)
        parseAndValidate(text3)
        parseAndValidate(text4)
        parseAndValidate(text5)
        parseAndValidate(text6)
        parseAndValidate(text7)
        parseAndValidate(text8)
        parseAndValidate(text9)
        parseAndValidate(text10)
        parseAndValidate(text11)
        parseAndValidate(text12)
        parseAndValidate(text13)
        parseAndValidate(text14)
        parseAndValidate(text15)
        parseAndValidate(text16)
        parseAndValidate(text17)
        parseAndValidate(text18)
        parseAndValidate(text19)