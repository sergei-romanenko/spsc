app(Nil, vs) = vs;
app(Cons(u, us), vs) = Cons(u, app(us, vs));
rev(Nil) = Nil;
rev(Cons(x, xs)) = app(rev(xs), Cons(x, Nil));
reverse(x)=rev(x);