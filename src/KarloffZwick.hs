module KarloffZwick(kz) where
	import Satsolve
	import Backtrack
	import Control.Monad.ST
	import Data.STRef

        -- | Runs the Karloff-Zwick algorithm on a 3SAT formula, returning the appropriate approximate assignment
        kz :: Formula -> Variables -> Assignment
	kz form var = runST $ do
		sol <- newSTRef []
		kz' form var sol
		readSTRef sol

		where 
		kz' form [v] sol = do
			let (form1, ass1) = assignValues form [(v, True)]
			let (form2, ass2) = assignValues form [(v,False)]

			let s1 = getSat form [(v,True)]
			let u1 = getUnsat form [(v,True)]

			let s2 = getSat form [(v,False)]
			let u2 = getUnsat form [(v,False)]
		
			let su1 = s1 + (div ((fromIntegral u1)*7) 8)
			let su2 = s2 + (div ((fromIntegral u2)*7) 8)
			
			if (su1	>= su2)
				then do
					(modifySTRef sol (putF (v,True)))
				else do
					(modifySTRef sol (putF (v,False))) 
	
			
		kz' form (v:vs) sol = do
			let (form1, ass1) = assignValues form [(v, True)]
			let (form2, ass2) = assignValues form [(v,False)]

			let s1 = getSat form [(v,True)]
			let u1 = getUnsat form [(v,True)]

			let s2 = getSat form [(v,False)]
			let u2 = getUnsat form [(v,False)]
		
			let su1 = s1 + (div ((fromIntegral u1)*7) 8)
			let su2 = s2 + (div ((fromIntegral u2)*7) 8)
			
			if (su1	> su2)
				then do
					kz' form1 vs sol
					(modifySTRef sol (putF (v,True)))
				else do
					kz' form2 vs sol
					(modifySTRef sol (putF (v,False))) 
			
		putF x y = x:y			
