{-|
Module      : Body
Description : The Body module contains the type definition of a Body. 
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
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

-- | 'DeltaV' is an alias for a 'Double' value.
type DeltaV = Double

-- | The 'Body' type is the central data type within the KSP
-- library. It could be either 'Railed' or 'Movable'
-- 
--     * 'Railed' is everything withing ksp that is a star, a planet
--     or a moon. They are static in their orbits and are therefore
--     called railed.
-- 
--     * 'Movable' is every other part in ksp, that is physical. Here
--     they include deltaV counter, but are also modelled as celestial
--     objects. Actually in ksp orbits around movable parts arent
--     possible. Anyhow, within this data type such constructs are
--     doable.
data Body
    = Railed  { object :: Object, celestial :: Celestial }
    | Movable { object :: Object, celestial :: Celestial, deltaV :: DeltaV }

-- | Make 'Body' an instance of Equalty
instance Eq Body where
    (Railed  o1 _)   == (Railed  o2 _)   = o1 == o2
    (Movable o1 _ _) == (Movable o2 _ _) = o1 == o2

-- | Make 'Body' an instance of Show
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

-- | The 'Celestial' type defines a celestial object within this
-- library. Every celestial object has
-- 
--     * 'r' [m] the radius (from the bodys center) in meter
-- 
--     * 'm' [kg] the mass of the object in kilo gramms
-- 
--     * 'soi' [m] the /s/phere /o/f /i/nfluence of the object in meter
-- 
--     * 'system' empty or a system of surronding objects.
-- 
data Celestial
    = Celestial { r      :: Double
                , m      :: Double
                , soi    :: Double
                , system :: System Body Body
                } deriving (Eq)



-- | Make 'Celestial' an instance of Show
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
                           
      
            
                                          
