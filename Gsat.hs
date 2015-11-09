module Gsat where
        import Satsolve
	import Backtrack
	import qualified Control.Monad.State.Lazy as S
        import System.Random
        import System.IO.Unsafe
        import System.Random.Mersenne.Pure64 (pureMT)
{-
	gsatsolve :: Formula -> Int -> Assignment -> Bool
	gsatsolve formula assignment = 

	gsattry :: Formula -> Assignment -> Int
-}	
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
      
        
      
--        gsat :: Formula -> Variables -> Assignment
        gsat form n var assgn = do
                gsatrunSt form var assgn 
                x <- S.get
                let num = getUnsat form x
                if(num ==0 || n==0)
                    then return x
                    else gsat form (n-1) var x
    
--        temp form var lst1 lst2 = y
--            where (x,y) = S.runState (gsat form 100 var lst1) lst2


        getRandomAss :: Int -> [Variable] -> Assignment
        getRandomAss _ [] = []
        getRandomAss n (x:xs) = (x,rt):(getRandomAss (n+1) xs)
            where rt = fst (random $ pureMT (fromInteger $ toInteger n))
                   --         1 -> True
                   --         0 -> False
        
        runMultiple form var _ 0 = getRandomAss 33 var
        runMultiple form var m n    | k==0 = y
                            | otherwise = runMultiple form var m (n-1)
            where   (x,y) = S.runState (gsat form m var assgn) assgn
                    assgn = getRandomAss 22 var
                    k = getUnsat form y

{-        main = do
                var <- getLine
                num <- getLine
                form <- getIn (read num)
                print (bt (Assign form) (words var))
                print $ printf (Assign form)
--                ass <- getLine
--                let assgn = formA (words var) (words ass)
--                let a = (getUnsat (Assign form) assgn)
                --let (c,b) = gsatrun (Assign form) (words var) [] assgn
                --let d = (getUnsat (Assign form) b)
                let y = runMultiple (Assign form) (words var) 100
		--print a
		--print b
		--print d
	        print y
-}            
