module System.KSP.Auxiliary
    ( takeUntil
    , firstJust
    )
where

import Data.Maybe (fromJust, catMaybes)
import Data.List (find)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x:xs)
    | not (f x)
    = x : (takeUntil f xs)

    | otherwise
    = []

firstJust :: [Maybe a] -> Maybe a
firstJust x = find (const True) (catMaybes x)
  
