	import Satsolve
	import Backtrack
	import qualified Control.Monad.State.Lazy as S
{-
	gsatsolve :: Formula -> Int -> Assignment -> Bool
	gsatsolve formula assignment = 

	gsattry :: Formula -> Assignment -> Int
-}	
	flipV :: Variable -> Assignment -> Assignment
	flipV _ [] = []
	flipV v ((x,y):xs) | x==v = (x, not y):(flipV v xs)
			  | otherwise = (x,y):(flipV v xs)

	gsatrun :: Formula -> Int -> Variables -> Assignment -> Assignment -> (Formula, Assignment)
	gsatrun form n (x:xs) nassgn oassgn 	| n_sat < n = gsatrun form n_sat xs tassgn oassgn
						| otherwise = gsatrun form n xs nassgn oassgn 
		where 	n_sat = getUnsat form tassgn
			tassgn = flipV x oassgn	
	gsatrun form n [] [] oassgn = S.runState stateF oassgn
				where stateF = stateFormula form
	gsatrun form n [] nassgn oassgn = S.runState stateF nassgn
				where stateF = stateFormula form
	

        main = do
                var <- getLine
                num <- getLine
                form <- getIn (read num)
                print (bt (Assign form) (words var))
                print $ printf (Assign form)
                ass <- getLine
                let assgn = formA (words var) (words ass)
                let a = (getUnsat (Assign form) assgn)
                let (c,b) = gsatrun (Assign form) a (words var) [] assgn
                let d = (getUnsat (Assign form) b)
		print a
		print b
		print d
	
