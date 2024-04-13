mytranspose mat = transhelp (length mat) mat []
    where transhelp 0 mat lst = lst
          transhelp n mat lst = transhelp (n-1) mat ([row !! (n-1) | row <- mat]:lst)

myintercalate l1 l2 = concat $ helper l1 l2                               
    where helper ls1 [] = [[]]                                              
          helper ls1 [x] = [x]                                              
          helper ls1 (el:ls2) = [el, ls1] ++ (helper ls1 ls2)