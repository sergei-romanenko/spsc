from(n) = Cons(n, from(S(n)));
head(Cons(x, xs)) = x;
mapAdd1(Nil) = Nil;
mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));
tail(Cons(x, xs)) = xs;
take(Z, xs) = Nil;
take(S(n), xs) = Cons(head(xs), take(n, tail(xs)));
takenm(n, m) = take(n, mapAdd1(from(m)));