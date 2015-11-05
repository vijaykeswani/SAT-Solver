--module Backtrack where
	import Satsolve
        import qualified Control.Monad.State.Lazy as S

	bt :: Formula -> [Variable] -> Bool
	
	bt formula [x] = mor form2 form3
		where 	(form2, ass1) = S.runState stateF [(x,True)]
			(form3, ass2) = S.runState stateF [(x,False)]
			stateF = stateFormula formula

	bt formula (x:xs) = form4 || form5
		where 	(form2, ass1) = S.runState stateF [(x,True)]
			form4 = bt form2 xs
			(form3, ass2) = S.runState stateF [(x,False)]
			form5 = bt form3 xs
			stateF = stateFormula formula

	literate :: [Variable] -> [Literal]
	
	literate [] = []
	literate ((x:[]):xs) = (Same [x]):(literate xs)
	literate ((x:y):xs) = (Negate y):(literate xs)

	printf :: Formula -> [[String]]
	printf (Assign (x:xs)) = (printc x):(printf (Assign xs))
	printf _ = []

	printc :: Clause -> [String]
	printc [] = []
	printc ((Same x):xs) = x : (printc xs)
	printc ((Negate x):xs) = ('~':x):(printc xs)

	getIn 0 = do 
		return []
	getIn num = do
		f <- getLine
		let c1 = literate $ words f
		x2 <- getIn (num-1)
		return (c1:x2)

	main = do
		var <- getLine
		num <- getLine
		form <- getIn (read num)
		print (bt (Assign form) (words var))		
		print $ printf (Assign form)

