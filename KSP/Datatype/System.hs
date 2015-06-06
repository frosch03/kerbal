module KSP.Datatype.System
where

import KSP.Datatype.Orbit


data System b a
    = Empty
    | System [(Orbit b, a)]
    deriving (Eq)


instance (Show a, Show b) => Show (System b a)
    where
      showsPrec _ (Empty)     = (showString "")
      showsPrec _ (System []) = (showString "")
      showsPrec _ (System (sys:syss))
          = (showString . show . snd $ sys)
          . (showString . show . fst $ sys)
          . (showChar '\n')
          . (showString . show $ (System syss))

instance Functor (System b) where
    fmap f (System xs) = System $ zip os (fmap f xs')
         where (os, xs') = unzip xs 
