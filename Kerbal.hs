{-# LANGUAGE GADTs, FlexibleInstances #-}
module Kerbal
where

-- import Debug.Trace
import Data.Maybe (fromJust)

type GravConst = Double

var_G :: GravConst
var_G = 6.674e-11


data Object
    = Object { name :: String }
      deriving (Eq)

data Celestial
    = Celestial { r      :: Double
                , m      :: Double
                , soi    :: Double
                , system :: System
                } deriving (Eq)

type DeltaV = Double

data Body
    = Railed  { object :: Object, celestial :: Celestial }
    | Movable { object :: Object, celestial :: Celestial, deltaV :: DeltaV }
    deriving (Eq)

data Orbit
    = Landed
    | O { centerBody  :: Body
        , apoapsis    :: Double
        , periapsis   :: Double
        , inclination :: Maybe Double
        , omega_big   :: Maybe Double -- Longitude of Ascending Node
        , omega_small :: Maybe Double -- Argument of Periapsis
        }
   deriving (Eq)

type System = [(Body, Orbit)]

type Speed = Double

instance Show Object
    where
      show = name

instance Show Celestial
    where
      showsPrec d (Celestial r m soi s)
          = (showString "r:")
          . ((show r) ++)
          . (showString ", ")
          . (showString "m:")
          . ((show m) ++)
          . (showString ", soi:")
          . (if (soi < (1/0))
             then ((show soi) ++)
             else (showString "inf"))
          . (fun s)
          where
            fun []     = (showString "")
            fun (x:xs) = (showChar '\n') . (showsPrec d x) . (fun xs)
                           

instance Show Body
    where
      showsPrec p (Railed  o c)
          =  ((name o) ++)
          . (showString ": ")
          . ((show c) ++)
      showsPrec p (Movable o c d)
          =  ((name o) ++)
          . (showString ", dV:")
          . ((show d) ++)
          . ((show c) ++)

instance Show Orbit
    where
      show Landed = "Landed"
      showsPrec _ obt
          = (showString "Around ")
          . (showString $ show . object . centerBody $ obt)
          . (showString " ( ")
          . (showString $ show . apoapsis $ obt)
          . (showString " / ")
          . (showString $ show . periapsis $ obt)
          . (showString ")")

type Radius = Double -- Radius of Orbit at the current Position
semiMajor   :: Orbit -> Double
semiMajor o
    = 0.5 * ((apoapsis o) + (periapsis o) + (2 * (r . celestial . centerBody $ o)))

      

v :: Orbit -> Radius -> Speed
v o radius =  sqrt $ mue * ( (2/radius) - (1/(semiMajor o)) ) 
    where
      mue = var_G * (m . celestial . centerBody $ o)

getNextUp :: System -> Body -> Maybe (Body)
getNextUp s f
    | (soi . celestial $ f) < (1/0)
    = Just $ centerBody . snd . head $ filter ((== f) . fst) s

    | otherwise
    = Nothing

getPathUp :: System -> Body -> [(Body)]
getPathUp s f
    = func (Just f)
      where
        func (Nothing) = []
        func (Just b)  = b : func (getNextUp s b)
                          

getDivid :: System -> Body -> (Body) -> (Int, (Body))
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

pathBetween :: System -> Body -> (Body) -> [(Body)]
pathBetween s f t
    = fU ++ (d:tUR)
      where (fU, d, tUR) = pathBetween' s f t

pathOBetween :: System -> Body -> (Body) -> [(Body, Orbit)]
pathOBetween s f t
    = (zip fU fU') ++ (zip tUR tUR')
      where (fU, d, tUR) = pathBetween' s f t
            fU', tUR' :: [Orbit]
            fU'  = map (\x -> snd . head $ filter ((== x) . fst) s) fU
            tUR' = map (\x -> snd . head $ filter ((== x) . fst) s) tUR
            d'   = (\x -> snd . head $ filter ((== x) . fst) s) d

pathBetween_ :: System -> Body -> (Body) -> [(Body)]
pathBetween_ s f t
    = fU ++ tUR
      where (fU, d, tUR) = pathBetween' s f t

                           
pathBetween' :: System -> Body -> (Body) -> ([(Body)], (Body), [(Body)])
pathBetween' s f t
    = (fU, d, (reverse tU))
      where fU   = takeUntil (== d) $ getPathUp s f
            tU   = takeUntil (== d) $ getPathUp s t
            d    = snd $ getDivid s f t 

speeds :: [(Body, Orbit)] -> [Speed]
speeds = map (\(_, x) -> v x (semiMajor x))
