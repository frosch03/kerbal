{-|
Module      : Object
Description : The Object module contains the type definition of a Object. 
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
module System.KSP.Datatype.Object
where



 ----------
-- Object --
 ----------

-- | An 'Object' is something named.
data Object
    = Object { name :: String }
      deriving (Eq)


-- | Make 'Object' an instance of 'Show'
instance Show Object
    where
      show = name
               
