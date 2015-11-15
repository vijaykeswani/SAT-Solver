module Backtrack(bt) where
	import Satsolve
        import qualified Control.Monad.State.Lazy as S
	import Control.Monad.ST
	import Data.STRef

        
        -- | This function takes a formula and the set of variables and applies the Backtracking algorithm to this formula, returning a satisfying              assignment
        bt :: Formula -> Variables -> Assignment
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
			let (form1, ass1) = assignValues form [(v, True)]
			res1 <- bt' form1 vs sol
			if res1
				then do
                                    (modifySTRef sol (putF (v,True)))
                                    return True
			        else do
                                    let (form2, ass2) = assignValues form [(v, False)]
			            res2 <- bt' form2 vs sol
				    if res2
					then do
                                            modifySTRef sol (putF (v,False))
                                            return True
                                        else do
                                            return False    


	putF x y = x:y
