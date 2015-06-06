{-|
Module      : System
Description : This module defines a system type.
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
module System.KSP.Datatype.System
where


import System.KSP.Datatype.Orbit

-- | The "System" module contains the type definition of a 'System'. 
data System b a
    = Empty
    | System [(Orbit b, a)]
    deriving (Eq)


-- | A 'System' could be shown, if both type parameters are also
-- filled with types, that are showable.
instance (Show a, Show b) => Show (System b a)
    where
      showsPrec _ (Empty)     = (showString "")
      showsPrec _ (System []) = (showString "")
      showsPrec _ (System (sys:syss))
          = (showString . show . snd $ sys)
          . (showString . show . fst $ sys)
          . (showChar '\n')
          . (showString . show $ (System syss))

-- | Making 'System b' a member of class functor. With this instance,
-- one could use map over 'System b'. 
instance Functor (System b) where
    fmap f (System xs) = System $ zip os (fmap f xs')
         where (os, xs') = unzip xs 
