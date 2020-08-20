myinit lst = if null (tail lst) then [] else (head lst) : myinit (tail lst)
mylast lst = if null (tail lst) then head lst else mylast (tail lst)
myapp l1 l2 = if null l1 then l2 else myapp (init l1) (last l1 : l2)

myind l 0 = head l
myind l i = myind (tail l) (i - 1)

myelem [] x = False
myelem l x = if (head l == x) then True else myelem (tail l) x

myrev [] = []
myrev l = last l : myrev (init l)

mymax x1 x2 = if x1 > x2 then x1 else x2
mymin x1 x2 = if x1 < x2 then x1 else x2

mymaximum lst = if null (tail lst) then head lst
                                   else mymax (head lst) (mymaximum (tail lst))

myminimum lst = if null (tail lst) then head lst
                                   else mymin (head lst) (myminimum (tail lst))