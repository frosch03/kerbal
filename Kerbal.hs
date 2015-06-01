{-# LANGUAGE GADTs, FlexibleInstances #-}
module Kerbal
where

type GravConst = Double

var_G :: GravConst
var_G = 6.674e-11



data Object
    = Object { name :: String }
      deriving (Eq)

data Celestial
    = Celestial { r   :: Double
                , m   :: Double
                , soi :: Double}
    deriving (Eq)

type DeltaV = Double

type Railed  = (Object, Celestial)
type Movable = (Object, DeltaV)

data Body a where
    St :: Object -> Celestial -> Body Railed
    Pl :: Object -> Celestial -> Body Railed
    Mn :: Object -> Celestial -> Body Railed
    Sh :: Object -> DeltaV    -> Body Movable

object :: Body a -> Object
object (St o _) = o
object (Pl o _) = o
object (Mn o _) = o
object (Sh o _) = o

isRailed :: Body a -> Bool
isRailed (St _ _) = True
isRailed (Pl _ _) = True
isRailed (Mn _ _) = True
isRailed (Sh _ _) = False

isMovable :: Body a -> Bool
isMovable (St _ _) = False
isMovable (Pl _ _) = False
isMovable (Mn _ _) = False
isMovable (Sh _ _) = True

celestial :: Body Railed -> Celestial
celestial (St _ c) = c
celestial (Pl _ c) = c
celestial (Mn _ c) = c

deltaV :: Body Movable -> DeltaV
deltaV (Sh _ d) = d


instance Eq (Body Railed) where
    (St o c) == x = o == object x && c == celestial x
    (Pl o c) == x = o == object x && c == celestial x
    (Mn o c) == x = o == object x && c == celestial x

instance Eq (Body Movable) where
    (Sh o d) == x = o == object x && d == deltaV x

data Orbit
    = Landed
    | O { centerBody  :: Body Railed
        , apoapsis    :: Double
        , periapsis   :: Double
        , inclination :: Maybe Double
        , omega_big   :: Maybe Double -- Longitude of Ascending Node
        , omega_small :: Maybe Double -- Argument of Periapsis
        }
    deriving (Eq)

type System a = [(Body a, Orbit)]

type Speed = Double

instance Show (Object)
    where
      show = name

instance Show (Body a)
    where
      show x@(St _ _) = '[':(show . object $ x) ++ "]"
      show x@(Pl _ _) = '<':(show . object $ x) ++ ">"
      show x@(Mn _ _) = '|':(show . object $ x) ++ "|"
      show x@(Sh _ _) = '{':(show . object $ x) ++ "}"

instance Show (Orbit)
    where
      show Landed = "Landed"
      show obt =    "Around " ++ (show . centerBody $ obt)
                 ++ " ( " ++ (show . apoapsis $ obt)
                 ++ " / " ++ (show . periapsis $ obt)
                 ++ ")"



type From   = Body Railed
type To     = Body Railed
type Radius = Double -- Radius of Orbit at the current Position
semiMajor :: Orbit -> Double
semiMajor o
    = 0.5 * ((apoapsis o) + (periapsis o) + (2 * (r . celestial . centerBody $ o)))

v :: Orbit -> Radius -> Speed
v o radius =  sqrt $ mue * ( (2/radius) - (1/(semiMajor o)) ) 
    where
      mue = var_G * (m . celestial . centerBody $ o)

-- get that transition to the (System a) type working

getNextUp :: System a -> From -> Maybe To
getNextUp s f
    | (soi . celestial $ f) < (1/0)
    = Just . centerBody . snd . head $ filter ((== f) . fst) s

    | otherwise
    = Nothing

getPathUp :: System a -> From -> [To]
getPathUp s f
    = func (Just f)
      where
        func (Nothing) = []
        func (Just b)  = b : func (getNextUp s b)
                          

getDivid :: System a -> From -> To -> (Int, To)
getDivid s f t
    = ((fst res), (fst . snd $ res))
      where fU  = reverse $ getPathUp s f
            tU  = reverse $ getPathUp s t
            ftU = zip fU tU
            res = head $ reverse $ filter (\(_,(x1,x2)) -> x1 == x2) $ zip [1..] ftU


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x:xs)
    | not (f x)
    = x : (takeUntil f xs)

    | otherwise
    = []

pathBetween :: System a -> From -> To -> [To]
pathBetween s f t
    = fU ++ (d:tUR)
      where (fU, d, tUR) = pathBetween' s f t

pathOBetween :: System a -> From -> To -> [(Body Railed, Orbit)]
pathOBetween s f t
    = (zip fU fU') ++ (zip tUR tUR')
      where (fU, d, tUR) = pathBetween' s f t
            fU', tUR' :: [Orbit]
            fU'  = map (\x -> snd . head $ filter ((== x) . fst) s) fU
            tUR' = map (\x -> snd . head $ filter ((== x) . fst) s) tUR
            d'   = (\x -> snd . head $ filter ((== x) . fst) s) d

pathBetween_ :: System a -> From -> To -> [To]
pathBetween_ s f t
    = fU ++ tUR
      where (fU, d, tUR) = pathBetween' s f t

                           
pathBetween' :: System a -> From -> To -> ([To], To, [To])
pathBetween' s f t
    = (fU, d, (reverse tU))
      where fU   = takeUntil (== d) $ getPathUp s f
            tU   = takeUntil (== d) $ getPathUp s t
            d    = snd $ getDivid s f t 

speeds :: [(Body Railed, Orbit)] -> [Speed]
speeds = map (\(_, x) -> v x (semiMajor x))
