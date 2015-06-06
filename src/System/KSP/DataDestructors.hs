{-|
Module      : DataDestructors
Description : This module defines functions to deconstruct the types
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
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


-- | 'getNextUp' takes a 'KSystem' system and a 'Body'. It returns the
-- body that the supplied body orbits. 'Nothing' is returned if the
-- supplied body is not found within the system.
getNextUp :: KSystem Body -> Body -> Maybe Body
getNextUp Empty _ = Nothing
getNextUp (System s) f
    | (length $ filter ((== f) . snd) s) > 0
    = Just $ centerBody . fst . head $ filter ((== f) . snd) s

    | (length $ filter ((== f) . snd) s) == 0
    = firstJust $ map (((flip getNextUp) f) . system . celestial . snd) s

    | otherwise
    = Nothing

      
-- | 'getPathUp' takes a 'KSystem' system and a 'Body'. It returns the
-- chain of body's that are orbiting each other. 
getPathUp :: KSystem Body -> Body -> [Body]
getPathUp s f
    = func (Just f)
      where
        func (Nothing) = []
        func (Just b)  = b : func (getNextUp s b)
                          

-- | 'getDivid' takes a 'KSystem' system and a /from/ 'Body' and a
-- /to/ 'Body'. For both bodys, the path up is calculated by
-- 'getPathUp' and the position, where they reach the same body is
-- returned (as Position and with the actual body).
getDivid :: KSystem Body -> Body -> Body -> (Int, Body)
getDivid s f t
    = ((fst res), (fst . snd $ res))
      where fU  = reverse $ getPathUp s f
            tU  = reverse $ getPathUp s t
            ftU = zip fU tU
            res = head $ reverse $ filter (\(_,(x1,x2)) -> x1 == x2) $ zip [1..] ftU


-- | 'sOrbitInSystem' takes a body, a 'KSystem' system and maybe
-- returns the orbit of the body. 
sOrbitInSystem :: Body -> KSystem Body -> Maybe (Orbit Body)
sOrbitInSystem b Empty = Nothing
sOrbitInSystem b s@(System sys)
    | (length $ filter ((== b) . snd) sys) > 0
    = Just . fst . head $ (filter ((== b) . snd) sys)

    | (length $ filter ((== b) . snd) sys) == 0
    = firstJust $ map (\x -> (sOrbitInSystem b) . system . celestial . snd $ x) sys

-- | 'pathOBetween' takes a 'KSystem' system and a /from/ 'Body' and a
-- /to/ 'Body'. If one want's to reach the /to/ body from /from/ body
-- the bodys between and their orbit around their centerBody are
-- returnd.
pathOBetween :: KSystem Body -> Body -> Body -> [(Body, (Orbit Body))]
pathOBetween Empty _ _ = []
pathOBetween s f t
    = (zip fU fU') ++ (zip tUR tUR')
      where (fU, d, tUR) = pathBetween' s f t
            fU', tUR' :: [Orbit Body]
            fU'  = map (\x -> fromJust $ x `sOrbitInSystem` s) fU
            tUR' = map (\x -> fromJust $ x `sOrbitInSystem` s) tUR
            d'   =     (\x -> fromJust $ x `sOrbitInSystem` s) d

-- | 'pathBetween_' takes a 'KSystem' system and a /from/ 'Body' and a
-- /to/ 'Body'. If one want's to reach the /to/ body from /from/ body
-- the bodys between are returnd.
pathBetween_ :: KSystem Body -> Body -> Body -> [Body]
pathBetween_ s f t
    = fU ++ tUR
      where (fU, d, tUR) = pathBetween' s f t

                           
-- | 'pathBetween' takes a 'KSystem' system and /from/ 'Body' and a
-- /to/ 'Body'. If one want's to reach the /to/ body from /from/ body
-- the bodys between and their orbit around their centerBody are
-- returnd. They are returnd as triple, with the overlapping body in
-- the center of the triple and the path of the /from/ body in the
-- first element of the triple. The path of the /to/ body in the last
-- element of the triple.
pathBetween' :: KSystem Body -> Body -> Body -> ([Body], Body, [Body])
pathBetween' s f t
    = (fU, d, (reverse tU))
      where fU   = takeUntil (== d) $ getPathUp s f
            tU   = takeUntil (== d) $ getPathUp s t
            d    = snd $ getDivid s f t 

-- | 'speeds' takes the result of 'pathOBetween' and calculates the
-- corresponding orbital speeds.
speeds :: [(Body, (Orbit Body))] -> [Speed]
speeds = map (\(_, x) -> v x (semiMajor x))


