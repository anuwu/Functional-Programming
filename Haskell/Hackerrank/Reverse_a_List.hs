revhelp [] r = r
revhelp l r = revhelp (tail l) ((head l) : r)
