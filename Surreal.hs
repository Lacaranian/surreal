import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.Ratio

data Surreal = Surreal { left  :: S.Set Surreal , right :: S.Set Surreal}

surreal :: [Surreal] -> [Surreal] -> Surreal
l `surreal` r = (S.fromList l) `Surreal` (S.fromList r)

(~|~) :: Surreal -> Surreal -> Surreal
(~|~) l r = Surreal (S.singleton l) (S.singleton r)

sNull = S.empty
sZero = Surreal sNull sNull
sOne = Surreal (S.singleton sZero) sNull

slmax, srmin :: Surreal -> Maybe Surreal
slmax srl = if not (S.null (left srl)) then Just (S.findMax (left srl)) else Nothing
srmin srl = if not (S.null (right srl)) then Just (S.findMin (right srl)) else Nothing
--"Unsafe" version for when a side set is non-empty
slmax' srl =  S.findMax (left srl)
srmin' srl =  S.findMin (right srl)

cleanSurreal :: Surreal -> Surreal
cleanSurreal s
    | s == sZero         = sZero
    | (left s)==sNull     = Surreal sNull ((S.singleton . cleanSurreal . srmin') s)
    | (right s)==sNull    = Surreal ((S.singleton . cleanSurreal . slmax') s) sNull
    | (slmax' s)<sZero && (srmin' s)>sZero        = sZero
    | otherwise    = Surreal ((S.singleton . cleanSurreal . slmax') s) ((S.singleton . cleanSurreal . srmin') s)

numeric :: Surreal -> Bool
numeric s = (S.intersection (left s) (right s) == S.empty)
    && (\lmax rmin -> if lmax == Nothing then True else (if rmin == Nothing then True else lmax < rmin)) (slmax s) (srmin s)
    
instance Show Surreal where
    show s
        | s == sZero         = "{|}"
        | (left s)==sNull     = "{|"++((intercalate ",". S.toList . S.map show)(right s))++"}"
        | (right s)==sNull    = "{"++((intercalate ",". S.toList . S.map show) (left s))++"|}"
        | otherwise    = "{"++((intercalate ",". S.toList . S.map show) (left s))++"|"++((intercalate ",". S.toList . S.map show)(right s))++"}"

instance Ord Surreal where
    x <= y
        | S.null (left x) && S.null (right x) && S.null (left y) && S.null (right y) = True
        | otherwise = S.notMember False (S.map (\yi -> not (yi <= x)) (right y))
                    && S.notMember False (S.map (\xi -> not (y <= xi)) (left x))

instance Eq Surreal where
    x == y = (x <= y) && (y <= x)

-- Only numeric surreals will be used entirely correctly here, though the
-- effects of numeric functions may have useful effects on non-numeric surreals
instance Num Surreal where
    negate s
        | s == sZero    = sZero 
        | otherwise     = Surreal (S.map negate (right s)) (S.map negate (left s))
    (+) sx sy
        | (sx == sZero) && (sy == sZero) = sZero
        | (sx == sZero) = sy
        | (sx == sZero) = sx
        | otherwise = Surreal (S.union (S.map (+sy) (left sx)) (S.map (+sx) (left sy))) (S.union (S.map (+sy) (right sx)) (S.map (+sx) (right sy)))
    (*) sx sy
        | (sx == sOne) = sy
        | (sy == sOne) = sx
        | (sx == -sOne)    = -sy
        | (sy == -sOne)    = -sx
        | (sx == sZero) || (sy == sZero) = sZero
        | otherwise    = Surreal (S.union (l1) (l2)) (S.union (r1) (r2)) where
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
        | i < 0     = (-sOne) + fromInteger (i+1)
        | i == 0    = sZero
        | i > 0     = sOne + fromInteger (i-1)
    abs s
        | s < sZero = -s
        | otherwise = s
    signum s
        | s < sZero     = -sOne
        | s == sZero    = sZero
        | s > sZero     = sOne

instance Real Surreal where
    toRational s
        | s == sZero            = 0 % 1
        | (left s) == sNull     = toRational (srmin' s) - 1
        | (right s) == sNull    = toRational (slmax' s) + 1
        | otherwise             = (toRational (slmax' s)) + ((toRational (srmin' s) - toRational (slmax' s))/2)

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo x = x == last (takeWhile (<=x) [(2^n)|n<-[1..]])

--Only dyadic rationals can be converted into Surreal numbers in finite time, currently
instance Fractional Surreal where
    fromRational r
        | (numerator r)   == 0           = sZero
        | (denominator r) == 1           = fromInteger (numerator r)
        | isPowerOfTwo (denominator r)   = (fromRational $ ((numerator r)-1)%(denominator r)) ~|~ (fromRational $ ((numerator r)+1)%(denominator r))
        | otherwise                      = undefined
    (/) sn sd
        | sd == sZero   = undefined
        | sd == sOne    = sn
        | isPowerOfTwo (denominator $ toRational sn)*(numerator $ toRational sd) = fromRational $ (toRational sn)/(toRational sd)
        | otherwise     = undefined

surrealWithPrecision = undefined
        
instance Floating Surreal where
    pi      = undefined
    exp s   = undefined
    log s   = undefined
    sin s   = undefined
    cos s   = undefined
    asin s  = undefined
    atan s  = undefined
    acos s  = undefined
    sinh s  = undefined
    cosh s  = undefined
    asinh s = undefined
    atanh s = undefined
    acosh s = undefined

instance RealFrac Surreal where
    properFraction s = undefined

instance RealFloat Surreal where
    floatRadix s        = undefined
    floatDigits s       = undefined
    floatRange s        = undefined
    decodeFloat s       = undefined
    encodeFloat s       = undefined
    isNaN s             = undefined
    isInfinite s        = undefined
    isDenormalized s    = undefined
    isNegativeZero s    = undefined
    isIEEE s            = undefined
