module Kerbal
where

data BodyMeta
    = BM { name  :: String}
    deriving (Eq)

data Body
    = St { meta   :: BodyMeta
         , celest :: Celestial}
    | Pl { meta   :: BodyMeta
         , celest :: Celestial}
    | Mn { meta   :: BodyMeta
         , celest :: Celestial}
    | Sh { meta   :: BodyMeta
         , dV     :: Float}
    deriving (Eq)

data Orbit
    = Landed
    | O { centerBody  :: Body
        , apoapsis    :: Float
        , periapsis   :: Float
        , inclination :: Maybe Float
        , omega_big   :: Maybe Float
        , omega_small :: Maybe Float
        }
    deriving (Eq)

-- instance Eq (Body)
--     where
--       b1 == b2 = (meta b1) == (meta b2)

-- instance Eq (BodyMeta)
--     where
--       bm1 == bm2 = (name bm1) == (name bm2)
                

instance Show (BodyMeta)
    where
      show = name

instance Show (Body)
    where
      show x@(St _ _) = '[':(show . meta $ x) ++ "]"
      show x@(Pl _ _) = '<':(show . meta $ x) ++ ">"
      show x@(Mn _ _) = '|':(show . meta $ x) ++ "|"
      show x@(Sh _ _) = '{':(show . meta $ x) ++ "}"

instance Show (Orbit)
    where
      show Landed = "Landed"
      show obt =    "Around " ++ (show . centerBody $ obt)
                 ++ " ( " ++ (show . apoapsis $ obt)
                 ++ " / " ++ (show . periapsis $ obt)
                 ++ ")"

data Celestial
    = Celestial { r      :: Float
                , mass   :: Float
                , soi    :: Float}
    deriving (Eq)

type System = [(Body, Orbit)]



type From = Body
type To   = Body
getNextUp :: System -> From -> Maybe Body
getNextUp s f
    | (soi . celest $ f) < (1/0)
    = Just . centerBody . snd . head $ filter ((== (name . meta $ f)) . name . meta . fst) s

    | otherwise
    = Nothing

getPathUp :: System -> From -> [Body]
getPathUp s f
    = func (Just f)
      where func (Nothing) = []
            func (Just b)  = b : func (getNextUp s b)
                          

getDivid :: System -> From -> To -> (Int, Body)
getDivid s f t
    = (,) (fst res) (fst . snd $ res)
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

pathBetween :: System -> From -> To -> [Body]
pathBetween s f t
    = fU ++ d:(reverse tU)
      where fU   = takeUntil (== d) $ getPathUp s f
            tU   = takeUntil (== d) $ getPathUp s t
            d    = snd $ getDivid s f t 
