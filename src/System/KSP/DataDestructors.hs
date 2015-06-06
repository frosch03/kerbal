module System.KSP.DataDestructors
    ( getNextUp
    , getPathUp
    , getDivid
    , sOrbitInSystem
    , pathOBetween
    , pathBetween_
    , pathBetween'
    , speeds
    )
where

import System.KSP.DataConstructors
import System.KSP.Auxiliary
import System.KSP.OrbitalMechanics

import Data.Maybe (fromJust)

getNextUp :: KSystem Body -> Body -> Maybe Body
getNextUp Empty _ = Nothing
getNextUp (System s) f
    | (length $ filter ((== f) . snd) s) > 0
    = Just $ centerBody . fst . head $ filter ((== f) . snd) s

    | (length $ filter ((== f) . snd) s) == 0
    = firstJust $ map (((flip getNextUp) f) . system . celestial . snd) s

    | otherwise
    = Nothing

      
getPathUp :: KSystem Body -> Body -> [Body]
getPathUp s f
    = func (Just f)
      where
        func (Nothing) = []
        func (Just b)  = b : func (getNextUp s b)
                          

getDivid :: KSystem Body -> Body -> Body -> (Int, Body)
getDivid s f t
    = ((fst res), (fst . snd $ res))
      where fU  = reverse $ getPathUp s f
            tU  = reverse $ getPathUp s t
            ftU = zip fU tU
            res = head $ reverse $ filter (\(_,(x1,x2)) -> x1 == x2) $ zip [1..] ftU


sOrbitInSystem :: Body -> KSystem Body -> Maybe (Orbit Body)
sOrbitInSystem b Empty = Nothing
sOrbitInSystem b s@(System sys)
    | (length $ filter ((== b) . snd) sys) > 0
    = Just . fst . head $ (filter ((== b) . snd) sys)

    | (length $ filter ((== b) . snd) sys) == 0
    = firstJust $ map (\x -> (sOrbitInSystem b) . system . celestial . snd $ x) sys

pathOBetween :: KSystem Body -> Body -> Body -> [(Body, (Orbit Body))]
pathOBetween Empty _ _ = []
pathOBetween s f t
    = (zip fU fU') ++ (zip tUR tUR')
      where (fU, d, tUR) = pathBetween' s f t
            fU', tUR' :: [Orbit Body]
            fU'  = map (\x -> fromJust $ x `sOrbitInSystem` s) fU
            tUR' = map (\x -> fromJust $ x `sOrbitInSystem` s) tUR
            d'   =     (\x -> fromJust $ x `sOrbitInSystem` s) d

pathBetween_ :: KSystem Body -> Body -> Body -> [Body]
pathBetween_ s f t
    = fU ++ tUR
      where (fU, d, tUR) = pathBetween' s f t

                           
pathBetween' :: KSystem Body -> Body -> Body -> ([Body], Body, [Body])
pathBetween' s f t
    = (fU, d, (reverse tU))
      where fU   = takeUntil (== d) $ getPathUp s f
            tU   = takeUntil (== d) $ getPathUp s t
            d    = snd $ getDivid s f t 

speeds :: [(Body, (Orbit Body))] -> [Speed]
speeds = map (\(_, x) -> v x (semiMajor x))


