module Dpll where
    import Satsolve
    import Gsat
    import Backtrack
    import qualified Control.Monad.State.Lazy as S
    import Data.Time
    import Control.Monad.ST
    import Data.STRef

    getLiteral :: Literal -> (Variable, Bool)
    getLiteral (Same x) = (x, True)
    getLiteral (Negate x) = (x, False)

    unitPropogate :: Formula -> Formula -> (Assignment, Formula)
    unitPropogate Mtrue _ = ([], Mtrue)
    unitPropogate Mfalse _ = ([], Mfalse)
    unitPropogate (Assign []) _ = ([], Assign [])
    unitPropogate (Assign ([x]:xs)) form = (af, nf)
                                      -- case  unitPropogate (Assign xs) of
                                      --  (z, Mtrue) -> (z, Mtrue)
                                      --  (z, Mfalse) -> (z, Mfalse)
                                      --  (z, Assign y) -> (na:z, nf)
                                            where   na = getLiteral x
                                                    (nf,af) = S.runState (stateFormula form) [na]
    unitPropogate (Assign (x:xs)) form = case unitPropogate (Assign xs) form of
                                        (z, Mtrue) -> (z, Mtrue)
                                        (z, Mfalse) -> (z, Mfalse)
                                        (z, Assign y) -> (z, Assign (x:y))
  
    findPure :: Formula -> [Variable] -> [(String, Bool)]
    findPure Mtrue _ = []
    findPure Mfalse _ = []
    findPure form [] = []
    findPure form (x:xs) = (checkPure form x):(findPure form xs)

    purify :: Formula -> Variables -> (Assignment, Formula)
    purify form var = (z,y)
		where 	ass = findPure form var
			(y,z) = S.runState stForm ass	 
			stForm = stateFormula form

    checkPure :: Formula -> Variable -> (String, Bool)
    checkPure form x | (getSat form [(x,True)]) == 0 = (x,False)
		     | (getSat form [(x,False)]) == 0 = (x,True) 
                     | otherwise = ("_",True)

    
 
    dpllUnitRunSt :: S.State Formula Assignment
    dpllUnitRunSt = S.state (\form -> (unitPropogate form form))

    dpllPureRunSt :: Variables -> S.State Formula Assignment
    dpllPureRunSt var = S.state (\form -> (purify form var))

    getNext :: Formula -> Variable
    getNext (Assign (((Same y):ys):xs)) = y
    getNext (Assign (((Negate y):ys):xs)) = y
    getNext _ = "_"

    printa = \x ->  do putStrLn x
{-
    dpll var = do
        ass1 <- dpllUnitRunSt
	ass2 <- dpllPureRunSt var
--        printa ass
        form <- S.get
        if( same form Mtrue)
            then return True
            else if(same form Mfalse)
                then return False
                else do
                    let x = getNext form
                    if(x == "_")
                        then return False
                        else do
                            let stForm = stateFormula form
                            let (y,ay) = S.runState stForm [(x,True)]
                            let (z,fz) = S.runState (dpll var) y
                            if z
                                then do
                                    --print ass
                                    return z
                                else do
                                    let (y,ay) = S.runState stForm [(x,False)]
                                    let (z,fz) = S.runState (dpll var) y
                                    --if z
                                    --    then print ass
                                    return z
                    
--            else let x = getNext form
-}               
 

    join a b = a ++ b


    dpll var form = runST $ do
	      sol <- newSTRef []
	      t <- newSTRef $! (dpll' var sol form) 	
	      readSTRef t
	      readSTRef sol

	      where dpll' var sol = S.runState $! do
		     
		    return $! (writeSTRef sol [("a",True)])
		    ass1 <- dpllUnitRunSt
		    ass2 <- dpllPureRunSt var
		    return (modifySTRef sol (join ass1))
		    return (modifySTRef sol (join ass2))
	--        printa ass
		    form <- S.get
		    if( same form Mtrue)
			    then return True
			    else if(same form Mfalse)
				then return False
				else do
				    let x = getNext form
				    if(x == "_")
					then return False
					else do
					    let stForm = stateFormula form
					    let (y,ay) = S.runState stForm [(x,True)]
					    let (z,fz) = (dpll' var sol) y
					    if z
						then do
						    --print ass
						    let ass = [(x,True)]
						    let fg = (modifySTRef sol (++ass))
						    return z
						else do
						    let (y,ay) = S.runState stForm [(x,False)]
						    let (z,fz) = (dpll' var sol) y
						    --if z
						    --    then print ass
						    let ass = [(x,False)]
						    let fg = (modifySTRef sol (++ass))
						    return z
	  

