let n = 100
let kashi = [(i,j) | i<-[1..n], j<-[1..n], j>i, (j-i)*(i+j+1) == 2*n]