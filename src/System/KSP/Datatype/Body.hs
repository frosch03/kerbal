module System.KSP.Datatype.Body
    ( Body(..)
    , DeltaV
    , Celestial(..)
    , Object(..)
    )
where

import System.KSP.Datatype.Object
import System.KSP.Datatype.System

 --------
-- Body --
 --------

type DeltaV = Double

data Body
    = Railed  { object :: Object, celestial :: Celestial }
    | Movable { object :: Object, celestial :: Celestial, deltaV :: DeltaV }

instance Eq Body where
    (Railed  o1 _)   == (Railed  o2 _)   = o1 == o2
    (Movable o1 _ _) == (Movable o2 _ _) = o1 == o2

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


 -------------
-- Celestial --
 -------------
data Celestial
    = Celestial { r      :: Double
                , m      :: Double
                , soi    :: Double
                , system :: System Body Body
                } deriving (Eq)



instance Show Celestial
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
                           
      
            
                                          
