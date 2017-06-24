append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));

appendXYaZ(xs, ys, zs) = append(append(xs, ys), zs);
appendXYaX(xs, ys) = append(append(xs, ys), xs);
appendXaYZ(xs, ys, zs) = append(xs, append(ys, zs));