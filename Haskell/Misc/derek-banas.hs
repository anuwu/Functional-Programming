{-
	This is a multiline comment.
	{-
		Nested comment is here!!!
		... Just another line
	-}
-}

import Data.List
import System.IO

minInt :: Int
minInt = minBound :: Int

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

{-
	> let x = 4 :: Int
	> sqrt x

	-- Error is 
	{-
		• No instance for (Floating Int) arising from a use of ‘sqrt’
	    • In the expression: sqrt x
      	In an equation for ‘it’: it = sqrt x
	-}

-}

l1 = [1,2,3,4,5]
l2 = [6,7,8,9,10]
z1 = zipWith (+) l1 l2

c1 = take 21 (cycle [1..7])
c1f = filter (<5) c1
-- [1,2,3,4,1,2,3,4,1,2,3,4]

c2 = c1
c2f = takeWhile (<= 4) c2
-- [1,2,3,4]


flist = [2,3,4,5,6]
ffold = foldr (*) 1 flist