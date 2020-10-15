-- Pairs of numbers l, r such that the sum of numbers [l, l+1, .. r] is equal to n
n = 100
kashi = [(i,j) | i<-[1..n], j<-[1..n], j>i, (j-i)*(i+j+1) == 2*n]
