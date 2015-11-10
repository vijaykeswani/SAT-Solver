module Satsolve where
	import qualified Control.Monad.State.Lazy as S
        import Data.Functor.Identity
	
--	newtype State s a = State { runState :: (s -> (a,s)) }
--	type State s  = S.StateT s Identity
     

	type Variable = String

	data BVariable a = Same a | Negate a

	type Literal = BVariable Variable 
	type Clause = [Literal]

	data DoubtFul a = Mtrue 
			| Mfalse 
			| Assign [a]

	type Formula = DoubtFul Clause
	type Variables = [Variable]
	type Assignment = [(Variable,Bool)]

	data Trilean = Tr | Fl | Unav

        same :: Formula -> Formula -> Bool
        same Mtrue Mtrue = True
        same Mfalse Mfalse = True
        same _ _ = False

	assignValues :: Formula -> Assignment -> (Formula, Assignment)
	assignValues Mtrue ass = (Mtrue, ass)
	assignValues Mfalse ass = (Mfalse, ass)
	assignValues (Assign lst) assignment = (assignCl lst assignment [], assignment)

	countClauses :: Formula -> Int
	countClauses (Assign (x:xs)) = 1 + (countClauses (Assign xs))
	countClauses _ = 0

	getUnsat :: Formula -> Assignment -> Int
	getUnsat Mtrue ass = 0
	getUnsat Mfalse ass = 1
	getUnsat (Assign lst) assignment = case (av1 lst assignment [] 0) of
                                            (Assign lst, result) -> 1
                                            (_, result) -> result

	getSat form ass = (countClauses form) - (getUnsat form ass)

	av1 :: [Clause] -> Assignment -> [Clause] -> Int -> (Formula,Int)
 	av1 [] _ [] n = (Mtrue, n)
	av1 [] _ lst n = (Assign lst, n)
	av1 (x:xs) ass lst n = case (av2 x ass []) of
			Mfalse -> av1 xs ass lst (n+1)
			Mtrue -> av1 xs ass lst n
			Assign lst2 -> av1 xs ass (lst2 ++ lst) n

	assignCl :: [Clause] -> Assignment -> [Clause] -> Formula
 	assignCl [] _ [] = Mtrue
	assignCl [] _ lst = Assign lst
	assignCl (x:xs) ass lst = case (av2 x ass []) of
			Mfalse -> Mfalse
			Mtrue -> assignCl xs ass lst
			Assign lst2 -> assignCl xs ass (lst2 ++ lst)

	av2 :: [Literal] -> Assignment -> [Literal] -> Formula
	av2 [] ass [] = Mfalse	
	av2 [] ass lst = Assign [lst]
	av2 (x:xs) ass lst = case (av3 x ass) of
			Mtrue -> Mtrue
			Mfalse -> av2 xs ass lst
			Assign [lst2] -> av2 xs ass (lst2 ++ lst)
	
	av3 :: Literal -> Assignment -> Formula	
	av3 (Same var) ass = case (getVal var ass) of
				Tr -> Mtrue
				Fl -> Mfalse
				Unav -> Assign [[Same var]]
	av3 (Negate var) ass = case (getVal var ass) of
				Tr -> Mfalse
				Fl -> Mtrue
				Unav -> Assign [[Negate var]]

	getVal :: Variable -> [(Variable,Bool)] -> Trilean
	
	getVal var [] = Unav
	getVal var ((x,y):xs) | (var == x) && y = Tr 
	getVal var ((x,y):xs) | (var == x) && (not y) = Fl
	getVal var ((x,y):xs) | otherwise = getVal var xs

        stateFormula :: Formula -> S.State Assignment Formula
        stateFormula formula = do
                                z <- S.get
                                let (y,x) = assignValues formula z
                                return y

        stateUnsat :: Formula -> S.State Assignment Int
        stateUnsat formula = do
                                z <- S.get
                                let y = getUnsat formula z
                                return y



--	stateFormula :: Formula -> S.State Assignment Formula
--	stateFormula formula = S.StateT sFormula 
--            where sFormula = do 
--                                let x = assignValues formula
--                                return x
                                

	mor :: Formula -> Formula -> Bool
	mor Mfalse Mfalse = False
	mor _ _ = True

	
