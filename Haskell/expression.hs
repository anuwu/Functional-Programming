import Parser

-- expr --> term +|- expr
--		  | term
-- term --> factor *|/ term
--		  | factor
-- factor --> (expr) | int

expr = do x <- term ;
		  do  
		  {
		  	char '+' ;
		    y <- expr ;
		    return (x+y)
		  }
		  <|>
		  do 
		  {	
		  	char '-' ;
		  	y <- expr ;
		  	return (x-y)
		  }
	   <|> term

term = do x <- factor ;
		  do 
		  { 
		  	char '*' ;
		  	y <- term ;
		  	return (x*y)
		  }
		  <|>
		  do 
		  {
		  	char '/' ;
		  	y <- term ;
		  	return (x `div` y)
		  }
	   <|> factor

factor = do char '(' ;
			x <- expr ;
			char ')' ;
			return x
		 <|> int