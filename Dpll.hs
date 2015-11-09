    import Satsolve
    import Gsat
    import Backtrack
    import qualified Control.Monad.State.Lazy as S

    getLiteral :: Literal -> (Variable, Bool)
    getLiteral (Same x) = (x, True)
    getLiteral (Negate x) = (x, False)

    unitPropogate :: Formula -> (Assignment, Formula)
    unitPropogate Mtrue = ([], Mtrue)
    unitPropogate Mfalse = ([], Mfalse)
    unitPropogate (Assign []) = (Mtrue, Assign [])
    unitPropogate (Assign ([x]:xs)) = ((getLiteral x):z, Assign y)
        where (z,Assign y) = unitPropogate (Assign xs)
    unitPropogate (Assign (x:xs)) = (z, Assign (x:y))
        where (z,Assign y) = unitPropogate (Assign xs)
    
    dpllRunSt :: S.State Formula Assignment
    dpllRunSt = S.state (\form -> (unitPropogate form))

    getNext :: Formula -> Variable
    getNext M

    dpll = do
        ass <- dpllRunSt
        form <- S.get
        if form == Mtrue
            then return Mtrue
        if form == Mfalse
            then return Mfalse
--            else let x = getNext form

    main = do
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
 
