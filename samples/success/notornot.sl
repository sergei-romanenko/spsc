if(True, x, y) = x;
if(False, x, y) = y;
not(x) = if(x, False, True);
or(x, y) = if(x, True, y);
and(x, y) = if(x, y, False);
notornot(x, y) = not(or(not(x), not(y)));