module Backtrack(bt) where
	import Satsolve
        import qualified Control.Monad.State.Lazy as S
	import Control.Monad.ST
	import Data.STRef

--	bt :: Formula -> [Variable] -> STRef s1 [([Char], Bool)] -> Bool
{-	
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
-}
	bt form var  = runST $ do
	  sol <- newSTRef []
       	  rx <- bt' form var sol	
	  readSTRef sol	

	  where	bt' form [v] sol = do
			let (form1, ass1) = assignValues form [(v, True)]
			let (form2, ass2) = assignValues form [(v, False)]
			--sol <- newSTRef []
			--s <- readSTRef sol'
			if (same Mtrue form1)
				then modifySTRef sol (putF (v,True))
				else do
					if (same Mtrue form2)
						then modifySTRef sol (putF (v,False))
						else writeSTRef sol []
			return (mor form1 form2)

	  	bt' form (v:vs) sol = do
--		forM_ var \v -> do
			let (form1, ass1) = assignValues form [(v, True)]
			res1 <- bt' form1 vs sol
			if res1
				then do
                                    (modifySTRef sol (putF (v,True)))
                                    return True
			        else do
                                    let (form2, ass2) = assignValues form [(v, False)]
			            res2 <- bt' form2 vs sol
--			let s = readSTRef sol
			--else do
				    if res2
					then do
                                            modifySTRef sol (putF (v,False))
                                            return True
                                        else do
--					    writeSTRef sol []
                                            return False    

--			return (res1 || res2)

	putF x y = x:y
{-	
	main = do
		var <- getLine
		num <- getLine
		form <- getIn (read num)
		print (bt (Assign form) (words var))		
		print $ printf (Assign form)
		ass <- getLine
		let assgn = formA (words var) (words ass)
		let a = (getUnsat (Assign form) assgn)	
		print a
-}
