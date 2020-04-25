let zippedNum = zip [1,2,3,4,5,6,7,8,9,10] ["one","two","three","four","five","six","seven","eight","nine","ten"]
let smallNumbigSpell = [(fst tup1, fst tup2) | tup1 <-zippedNum, tup2 <-zippedNum, fst tup1 < fst tup2, length (snd tup1) > length (snd tup2)]
