flip(Leaf(z)) = Leaf(z);
flip(Branch(xt, yt)) = Branch(flip(yt), flip(xt));
flip2(zt) = flip(flip(zt));
flip3(zt) = flip(flip(flip(zt)));