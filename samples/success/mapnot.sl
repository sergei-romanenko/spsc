not(True) = False;
not(False) = True;
mapNot(Nil) = Nil;
mapNot(Cons(x, xs)) = Cons(not(x), mapNot(xs));

mapNotNot(xs) = mapNot(mapNot(xs));
mapNotNotNot(xs) = mapNot(mapNot(mapNot(xs)));