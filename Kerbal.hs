{-# LANGUAGE GADTs, FlexibleInstances #-}
module Kerbal
where

-- import Debug.Trace
import Data.Maybe (fromJust, catMaybes)
import Data.List (find)

type GravConst = Double

var_G :: GravConst
var_G = 6.674e-11


data Object
    = Object { name :: String }
      deriving (Eq)

data Celestial a
    = Celestial { r      :: Double
                , m      :: Double
                , soi    :: Double
                , system :: System a
                } deriving (Eq)

type DeltaV = Double

data Body
    = Railed  { object :: Object, celestial :: Celestial Body }
    | Movable { object :: Object, celestial :: Celestial Body, deltaV :: DeltaV }

instance Eq Body where
    (Railed  o1 _)   == (Railed  o2 _)   = o1 == o2
    (Movable o1 _ _) == (Movable o2 _ _) = o1 == o2

data Orbit a
    = Landed
    | O { centerBody  :: a
        , apoapsis    :: Double
        , periapsis   :: Double
        , inclination :: Maybe Double
        , omega_big   :: Maybe Double -- Longitude of Ascending Node
        , omega_small :: Maybe Double -- Argument of Periapsis
        }
  deriving (Eq)

type Height = Double
mkOrbit :: Body -> Height -> Height -> Orbit Body
mkOrbit b ap per
    = O { centerBody  = b
        , apoapsis    = ap
        , periapsis   = per
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
      
mkCircOrbit :: Body -> Height -> Orbit Body
mkCircOrbit b h
    = O { centerBody  = b
        , apoapsis    = h
        , periapsis   = h
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }

data System a
    = Empty
    | System [(Orbit Body, a)]
    deriving (Eq)

type Speed = Double

instance Show Object
    where
      show = name

instance Show (Celestial a)
    where
      showsPrec d (Celestial r m soi Empty)
          = (showString "r:")
          . ((show r) ++)
          . (showString ", ")
          . (showString "m:")
          . ((show m) ++)
          . (showString ", soi:")
          . (if (soi < (1/0))
             then ((show soi) ++)
             else (showString "inf"))
      showsPrec d (Celestial r m soi (System s))
          = (showChar '(')
          . (showString . show $ length s)
          . (showChar ')')
          . (showString "r:")
          . ((show r) ++)
          . (showString ", ")
          . (showString "m:")
          . ((show m) ++)
          . (showString ", soi:")
          . (if (soi < (1/0))
             then ((show soi) ++)
             else (showString "inf"))
          where
            -- fun []     = (showChar '\n')
            -- fun (x:xs) = (showChar '\n') . (showsPrec d $ snd x) . (fun xs)
                           

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

instance (Show a) => Show (Orbit a)
    where
      showsPrec _  Landed = showString "Landed"
      showsPrec _ obt
          = (showString "Around ")
          . (showString $ show . centerBody $ obt)
          . (showString " ( ")
          . (showString $ show . apoapsis $ obt)
          . (showString " / ")
          . (showString $ show . periapsis $ obt)
          . (showString ")")

instance (Show a) => Show (System a)
    where
      showsPrec _ (Empty)     = (showString "")
      showsPrec _ (System []) = (showString "")
      showsPrec _ (System (sys:syss))
          = (showString . show . snd $ sys)
          . (showString . show . fst $ sys)
          . (showChar '\n')
          . (showString . show $ (System syss))

instance Functor System where
    fmap f (System xs) = System $ zip os (fmap f xs')
         where
           (os, xs') = unzip xs
                           

type Radius = Double -- Radius of Orbit at the current Position
semiMajor   :: Orbit Body -> Double
semiMajor o
    = 0.5 * ((apoapsis o) + (periapsis o) + (2 * (r . celestial . centerBody $ o)))

      
firstJust :: [Maybe a] -> Maybe a
firstJust x = find (const True) (catMaybes x)

v :: Orbit Body -> Radius -> Speed
v o radius =  sqrt $ mue * ( (2/radius) - (1/(semiMajor o)) ) 
    where
      mue = var_G * (m . celestial . centerBody $ o)

v_e :: Body -> Speed
v_e b =  sqrt $ 2 * mue / (r . celestial $ b) 
    where
      mue = var_G * (m . celestial $ b)


-- v1 = sqrt(mue/r1) * (sqrt((2*r2)/(r1+r2)) -1)
-- v2 = sqrt(mue/r2) * (1 - (sqrt((2*r1)/(r1+r2))))

hohmann :: Orbit Body -> Orbit Body -> (Double, Double)
hohmann o1 o2
    |   centerBody o1 /= centerBody o2
      || o1 == o2
    = (0,0)
hohmann o1 o2
    = ( sqrt(mue/r1) * (sqrt((2*r2)/(r1+r2)) - 1) 
      , sqrt(mue/r2) * (1 - (sqrt((2*r1)/(r1+r2))))
      )
    where 
      mue = var_G * (m . celestial . centerBody $ o1)
      r1 = (r . celestial . centerBody $ o1) + (apoapsis o1)
      r2 = (r . celestial . centerBody $ o2) + (apoapsis o2)



getNextUp :: System Body -> Body -> Maybe Body
getNextUp Empty _ = Nothing
getNextUp (System s) f
    | (length $ filter ((== f) . snd) s) > 0
    = Just $ centerBody . fst . head $ filter ((== f) . snd) s

    | (length $ filter ((== f) . snd) s) == 0
    = firstJust $ map (((flip getNextUp) f) . system . celestial . snd) s

    | otherwise
    = Nothing


-- data System a
--     = Empty
--     | System [(Orbit, a)]
--     deriving (Eq)
      

getPathUp :: System Body -> Body -> [Body]
getPathUp s f
    = func (Just f)
      where
        func (Nothing) = []
        func (Just b)  = b : func (getNextUp s b)
                          

getDivid :: System Body -> Body -> Body -> (Int, Body)
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

sOrbitInSystem :: Body -> System Body -> Maybe (Orbit Body)
sOrbitInSystem b Empty = Nothing
sOrbitInSystem b s@(System sys)
    | (length $ filter ((== b) . snd) sys) > 0
    = Just . fst . head $ (filter ((== b) . snd) sys)

    | (length $ filter ((== b) . snd) sys) == 0
    = firstJust $ map (\x -> (sOrbitInSystem b) . system . celestial . snd $ x) sys

pathOBetween :: System Body -> Body -> Body -> [(Body, (Orbit Body))]
pathOBetween Empty _ _ = []
pathOBetween s f t
    = (zip fU fU') ++ (zip tUR tUR')
      where (fU, d, tUR) = pathBetween' s f t
            fU', tUR' :: [Orbit Body]
            fU'  = map (\x -> fromJust $ x `sOrbitInSystem` s) fU
            tUR' = map (\x -> fromJust $ x `sOrbitInSystem` s) tUR
            d'   =     (\x -> fromJust $ x `sOrbitInSystem` s) d

pathBetween_ :: System Body -> Body -> Body -> [Body]
pathBetween_ s f t
    = fU ++ tUR
      where (fU, d, tUR) = pathBetween' s f t

                           
pathBetween' :: System Body -> Body -> Body -> ([Body], Body, [Body])
pathBetween' s f t
    = (fU, d, (reverse tU))
      where fU   = takeUntil (== d) $ getPathUp s f
            tU   = takeUntil (== d) $ getPathUp s t
            d    = snd $ getDivid s f t 

speeds :: [(Body, (Orbit Body))] -> [Speed]
speeds = map (\(_, x) -> v x (semiMajor x))
