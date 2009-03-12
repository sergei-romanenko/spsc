gappend(Nil(), vs) = vs;
gappend(Cons(u, us), vs) = Cons(u, gappend(us, vs));

greverse(Nil()) = Nil();
greverse(Cons(x, xs)) = gappend(greverse(xs), Cons(x, Nil()));

ffrom(n) = Cons(n, ffrom(S(n)));

gtake(Z(), xs) = Nil();
gtake(S(n), xs) = Cons(ghead(xs), gtake(n, gtail(xs)));

gmapAdd1(Nil()) = Nil();
gmapAdd1(Cons(x, xs)) = Cons(S(x), gmapAdd1(xs));

gnull(Nil()) = True();
gnull(Cons(x, xs)) = False();

ghead(Cons(x, xs)) = x;

gtail(Cons(x, xs)) = xs;

gab(A(x)) = B(gab(x));
gab(B(x)) = A(gab(x));

greva(Nil(), ys) = ys;
greva(Cons(x, xs), ys) = greva(xs, Cons(x, ys));

geq(Z(), y) = geqZ(y);
geq(S(x), y) = geqS(y, x);

geqZ(Z()) = True();
geqZ(S(x)) = False();

geqS(Z(), x) = False();
geqS(S(y), x) = geq(x, y);

gif(True(), x, y) = x;
gif(False(), x, y) = y;

fnot(x) = gif(x, False(), True());
for(x, y) = gif(x, True(), y);
fand(x, y) = gif(x, y, False());

fmember(x, list) = fand(fnot(gnull(list)), for(geq(x, ghead(list)), fmember(x, gtail(list))));

gmember2(Nil(), x) = False();
gmember2(Cons(y, ys), x) = gif(geq(x, y), True(), gmember2(ys, x));
gmember3(Nil(), x) = False();
gmember3(Cons(y, ys), x) = gmember3if(geq(x, y), x, ys);
gmember3if(True(), x, ys) = True();
gmember3if(False(), x, ys) = gmember3(ys, x);

fTest1() = gappend(Nil(), Nil());
fTest2() = gappend(Cons(A1(), Cons(A2(), Nil())), Cons(A3(), Cons(A4(), Nil())));
fTest3() = greverse(Cons(A1(), Cons(A2(), Cons(A3(), Nil()))));
fTest4() = gmapAdd1(ffrom(Z()));
fTest5(x) = greverse(x);
fTest6(x) = gab(gab(x));
fTest7(xs) = greva(xs, Nil());
fTest8(x) = geq(x, x);
fTest9(x) = geq(x, S(x));
fTest10(x) = geq(S(x), x);
fTest11(x) = geq(S(Z()), x);
fTest12(x, y) = fnot(for(fnot(x), fnot(y)));
fTest13(x, list) = fmember(x, list);
fTest14(list) = fmember(S(Z()), list);
fTest15(x) = fmember(x, Cons(Z(), Cons(S(Z()), Nil())));
fTest16(x, list) = gmember2(list, x);
fTest17(list) = gmember2(list, S(Z()));
fTest18(x) = gmember2(Cons(Z(), Cons(S(Z()), Nil())), x);
fTest19(x, list) = gmember3(list, x);
fTest20(list) = gmember3(list, S(Z()));
fTest21(x) = gmember3(Cons(Z(), Cons(S(Z()), Nil())), x);
