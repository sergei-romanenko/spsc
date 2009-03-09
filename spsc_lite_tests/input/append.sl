append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));
append2(xs, ys, zs) = append(append(xs, ys), zs);
append3(xs, ys) = append(append(xs, ys), xs);