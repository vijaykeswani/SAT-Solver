    import Satsolve
    import Gsat
    import Backtrack
    import Dpll
    import KarloffZwick
    import qualified Control.Monad.State.Lazy as S
    import Control.Exception
    import Data.Time


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
    
    formA :: [Variable] -> [String] -> [(Variable, Bool)]
    formA (v:vs) (x:xs) | x=="1" = (v,True): (formA vs xs)
                    | otherwise = (v,False) : (formA vs xs)
    formA _ _ = []

    main = do
                vr <- getLine
                num <- getLine
                frm <- getIn (read num)
                let var = words vr
                let form = Assign frm
                start <- getCurrentTime
                let z = bt form var
                let uz = getUnsat form z
		if(uz==0)
			then do 
                            print True
--                            print z
			else print False
                end   <- getCurrentTime
                print (diffUTCTime end start)
 
                putStrLn ""
--                print z
	              -- print $ printf form
--                ass <- getLine
--                let assgn = formA (words var) (words ass)
--                let a = (getUnsat (Assign form) assgn)
                --let (c,b) = gsatrun (Assign form) (words var) [] assgn
                --let d = (getUnsat (Assign form) b)
                start <- getCurrentTime
                let y = runMultiple form var 2 2
               
                let ux = getUnsat form y
		if(ux==0)
			then do 
                            print True
--                            print y
			else print False
                end   <- getCurrentTime
                print (diffUTCTime end start)
                putStrLn ""
                --print a
		--print b
		--print d
	        --print y
                start <- getCurrentTime
                let (d,dl) = S.runState (dpll var) form
                print d
                end   <- getCurrentTime
                print (diffUTCTime end start)
		putStrLn ""

		let w = kz form var
		let uw = getUnsat form w 
		print w
		print uw	



