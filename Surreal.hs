import qualified Data.Set as S

data Surreal = Surreal { left  :: S.Set Surreal
                       , right :: S.Set Surreal
                       } deriving (Show)

surreal :: [Surreal] -> [Surreal] -> Surreal
l `surreal` r = (S.fromList l) `Surreal` (S.fromList r)

(~|~) :: S.Set Surreal -> S.Set Surreal -> Surreal
(~|~) l r = Surreal l r

sNull = S.empty
sZero = sNull ~|~ sNull
sOne = (S.singleton sZero) ~|~ sNull

instance Ord Surreal where
    x <= y
        | S.null (left x) && S.null (right x) && S.null (left y) && S.null (right y) = True
        | otherwise = S.notMember False (S.map (\yi -> not (yi <= x)) (right y))
                    && S.notMember False (S.map (\xi -> not (y <= xi)) (left x))

instance Eq Surreal where
    x == y = (x <= y) && (y <= x)

slmax, srmin :: Surreal -> Maybe Surreal
slmax srl = if not (S.null (left srl)) then Just (S.findMax (left srl)) else Nothing
srmin srl = if not (S.null (right srl)) then Just (S.findMin (right srl)) else Nothing

numeric :: Surreal -> Bool
numeric s = (S.intersection (left s) (right s) == S.empty)
    && (\lmax rmin -> if lmax == Nothing then True else (if rmin == Nothing then True else lmax < rmin)) (slmax s) (srmin s)

-- Only numeric surreals will be used entirely correctly here, though the
-- effects of numeric functions may have useful effects on non-numeric surreals
instance Num Surreal where
	negate s
		| s == sZero 	= sZero 
		| otherwise 	= Surreal (S.map negate (right s)) (S.map negate (left s))
	(+) sx sy
		| (sx == sZero) && (sy == sZero) = sZero
		| (sx == sZero) = sy
		| (sx == sZero) = sx
		| otherwise = Surreal (S.union (S.map (+sy) (left sx)) (S.map (+sx) (left sy))) (S.union (S.map (+sy) (right sx)) (S.map (+sx) (right sy)))
	(*) sx sy
		| (sx == sOne) = sy
		| (sy == sOne) = sx
		| (sx == -sOne)	= -sy
		| (sy == -sOne)	= -sx
		| (sx == sZero) || (sy == sZero) = sZero
		| otherwise	= Surreal (S.union (l1) (l2)) (S.union (r1) (r2)) where
			map2 op s1 s2 = (S.unions . S.toList) (S.map (\setelem -> S.map (op setelem) s2) s1)
			multsets set1 set2 = map2 (*) set1 set2
			multnumset sn set = if set == sNull then sNull else S.map (*sn) set
			addsets set1 set2 = if set1 == sNull || set2 == sNull then sNull else map2 (+) set1 set2
			subtractsets set1 set2 = if set1 == sNull || set2 == sNull then sNull else map2 (-) set1 set2
			l1 = ((multnumset sy (left sx)) `addsets` ((multnumset sx (left sy)) `subtractsets` (multsets (left sx) (left sy))))
			l2 = ((multnumset sy (right sx)) `addsets` ((multnumset sx (right sy)) `subtractsets` (multsets (right sx) (right sy))))
			r1 = ((multnumset sy (left sx)) `addsets` ((multnumset sx (right sy)) `subtractsets` (multsets (left sx) (right sy))))
			r2 = ((multnumset sy (right sx)) `addsets` ((multnumset sx (left sy)) `subtractsets` (multsets (right sx) (left sy))))
			
	fromInteger i
		| i < 0 	= (-sOne) + fromInteger (i+1)
		| i == 0	= sZero
		| i > 0		= sOne + fromInteger (i-1)
	abs s
		| s < sZero = -s
		| otherwise = s
	signum s
		| s < sZero		= -sOne
		| s == sZero	= sZero
		| s > sZero		= sOne
