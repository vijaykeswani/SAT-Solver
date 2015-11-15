module Dpll(unitPropogate, findPure, dpll) where
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

    -- | Given a formula, searches for a clause with only one literal and returns and assignment which will remove such a unit clause and the new formula
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
 
    -- | Given a formula and the set of variables, this function finds the pure variables in the formula, and returns their corresponding true assignment
    findPure :: Formula -> [Variable] -> [(Variable, Bool)]
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

    -- | Runs the DPLL algorithm with Unit Propogation, moving from one assignment state to another in each iteration
    dpll var = do
        ass1 <- dpllUnitRunSt
--	ass2 <- dpllPureRunSt var
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
                                    return z
                                else do
                                    let (y,ay) = S.runState stForm [(x,False)]
                                    let (z,fz) = S.runState (dpll var) y
                                    return z
                    
