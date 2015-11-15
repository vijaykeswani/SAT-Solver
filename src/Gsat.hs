module Gsat(runMultiple) where
        import Satsolve
	import Backtrack
	import qualified Control.Monad.State.Lazy as S
        import System.Random
        import System.IO.Unsafe
        import System.Random.Mersenne.Pure64 (pureMT)
	flipV :: Variable -> Assignment -> Assignment
	flipV _ [] = []
	flipV v ((x,y):xs) | x==v = (x, not y):(flipV v xs)
			  | otherwise = (x,y):(flipV v xs)

	gsatrun :: Formula -> Variables -> Assignment -> Assignment -> (Formula, Assignment)
	gsatrun form (x:xs) nassgn oassgn 	| n_sat <= n = gsatrun form xs tassgn oassgn
						| otherwise = gsatrun form xs nassgn oassgn 
		where 	n_sat = getUnsat form tassgn
                        n = getUnsat form nassgn
			tassgn = flipV x oassgn	
	gsatrun form [] [] oassgn = S.runState stateF oassgn
				where stateF = stateFormula form
	gsatrun form [] nassgn oassgn = S.runState stateF nassgn
				where stateF = stateFormula form


	gsatrunSt :: Formula -> Variables -> Assignment -> S.State Assignment Formula
        gsatrunSt form lst nassgn = S.state (\ass -> (gsatrun form lst nassgn ass))
      
        
      
        gsat form n var assgn = do
                gsatrunSt form var assgn 
                x <- S.get
                let num = getUnsat form x
                if(num ==0 || n==0)
                    then return x
                    else gsat form (n-1) var x
    


        getRandomAss :: Int -> [Variable] -> Assignment
        getRandomAss _ [] = []
        getRandomAss n (x:xs) = (x,rt):(getRandomAss (n+1) xs)
            where rt = fst (random $ pureMT (fromInteger $ toInteger n))

        
        -- | Runs the GSAT algorithm for a given formula and set of variables, with the two parameters representing the maximum number of tries and flips allowed
        runMultiple form var _ 0 = getRandomAss 100 var
        runMultiple form var m n    | k==0 = y
                            | otherwise = runMultiple form var m (n-1)
            where   (x,y) = S.runState (gsat form m var assgn) assgn
                    assgn = getRandomAss n var
                    k = getUnsat form y

