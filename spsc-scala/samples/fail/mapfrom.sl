from(n) = Cons(n, from(S(n)));
mapAdd1(Nil) = Nil;
mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));
mapfrom(n) = mapAdd1(from(n));