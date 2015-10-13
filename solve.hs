module Satsolve where
	import Control.Monad

	type Literal = Bool
	type Clause = [Literal]
	data DoubtFul a =True 
			| False 
			| Assign [a]

	type Formula = DoubtFul Clause

	type Variables = [String]
	
	
