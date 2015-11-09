    import Satsolve
    import Gsat
    import Backtrack
    import qualified Control.Monad.State.Lazy as S

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
   
    dpllRunSt :: S.State Formula Assignment
    dpllRunSt = S.state (\form -> (unitPropogate form form))

    getNext :: Formula -> Variable
    getNext (Assign (((Same y):ys):xs)) = y
    getNext (Assign (((Negate y):ys):xs)) = y
    getNext _ = "_"

    printa = \x ->  do putStrLn x

    dpll = do
        ass <- dpllRunSt
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
                            let (z,fz) = S.runState dpll y
                            if z
                                then do
                                    --print ass
                                    return z
                                else do
                                    let (y,ay) = S.runState stForm [(x,False)]
                                    let (z,fz) = S.runState dpll y
                                    --if z
                                    --    then print ass
                                    return z
                    
--            else let x = getNext form

    main = do
                vr <- getLine
                num <- getLine
                frm <- getIn (read num)
                let var = words vr
                let form = Assign frm
                print (bt form var)
               -- print $ printf form
--                ass <- getLine
--                let assgn = formA (words var) (words ass)
--                let a = (getUnsat (Assign form) assgn)
                --let (c,b) = gsatrun (Assign form) (words var) [] assgn
                --let d = (getUnsat (Assign form) b)
                let y = runMultiple form var 100 100
                let ux = getUnsat form y
		if(ux==0)
			then print True
			else print False
		--print a
		--print b
		--print d
	        --print y
                let (d,dl) = S.runState dpll form
                print d
                
 
