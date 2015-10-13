module Satsolve where
	import Control.Monad

	type Variable = String

	data BVariable a = Same a | Negate a

	type Literal = BVariable Variable 
	type Clause = [Literal]

	data DoubtFul a =True 
			| False 
			| Assign [a]

	type Formula = DoubtFul Clause
	type Variables = [String]
	type Assignment = [(String,Bool)]

	assignValues :: Formula -> Assignment -> (Formula, Assignment)
	assignValues True _ = (True, _)
	assignValues False _ = (False, _)
	assignValues Assign lst assignment = av1 lst assignment

	av1 :: [Clauses] -> assignment -> Formula
	

	
