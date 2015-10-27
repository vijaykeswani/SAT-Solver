module Satsolve where
--	import Control.Monad.State.Strict
--	import Data.Functor.Identity
	
	newtype State s a = State { runState :: (s -> (a,s)) }
--	type State s = StateT s Identity

	type Variable = String

	data BVariable a = Same a | Negate a

	type Literal = BVariable Variable 
	type Clause = [Literal]

	data DoubtFul a =Mtrue 
			| Mfalse 
			| Assign [a]

	type Formula = DoubtFul Clause
	type Variables = [Variable]
	type Assignment = [(Variable,Bool)]

	data Trilean = Tr | Fl | Unav

	assignValues :: Formula -> Assignment -> (Formula, Assignment)
	assignValues Mtrue ass = (Mtrue, ass)
	assignValues Mfalse ass = (Mfalse, ass)
	assignValues (Assign lst) assignment = ((av1 lst assignment []), assignment)

	av1 :: [Clause] -> Assignment -> [Clause] -> Formula
 	av1 [] _ [] = Mtrue
	av1 [] _ lst = Assign lst
	av1 (x:xs) ass lst = case (av2 x ass []) of
			Mfalse -> Mfalse
			Mtrue -> av1 xs ass lst
			Assign lst2 -> av1 xs ass (lst2 ++ lst)

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

	stateFormula :: Formula -> State Assignment Formula
	stateFormula formula = State $ assignValues formula	

	
