{-|
Module      : Auxiliary
Description : This module contains some auxiliary functions.
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
module System.KSP.Auxiliary
    ( takeUntil
    , firstJust
    )
where

import Data.Maybe (fromJust, catMaybes)
import Data.List (find)


    
-- | The function 'takeUntil' takes a test function and a list of
-- values. It returns all elements that do not pass the test function
-- until it reaches the first one that passes the test function,
-- otherwise it returns the empty list.
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x:xs)
    | not (f x)
    = x : (takeUntil f xs)

    | otherwise
    = []

-- | 'firstJust' returns of a list of 'Maybe' values the first actual
-- 'Just' value or nothing if none is within the list.
firstJust :: [Maybe a] -> Maybe a
firstJust x = find (const True) (catMaybes x)
  
