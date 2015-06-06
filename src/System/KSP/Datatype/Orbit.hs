{-|
Module      : Orbit
Description : This module the defines the orbit type.
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
module System.KSP.Datatype.Orbit
where

-- | The "Orbit" module contains the type definition of a 'Orbit'. 


 ---------
-- Orbit --
 ---------

data Orbit a
    = Landed
    | O { centerBody  :: a
        , apoapsis    :: Double
        , periapsis   :: Double
        , inclination :: Maybe Double
        , omega_big   :: Maybe Double -- Longitude of Ascending Node
        , omega_small :: Maybe Double -- Argument of Periapsis
        }
  deriving (Eq)

-- | The 'Height' is the height above ground level.
type Height = Double

-- | 'mkOrbit' takes a body as well as apoapsis and periapsis and
-- of that creates an orbit.
mkOrbit :: a -> Height -> Height -> Orbit a
mkOrbit b ap per
    = O { centerBody  = b
        , apoapsis    = ap
        , periapsis   = per
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
      
-- | 'mkCircOrbit' is similar to 'mkOrbit' with the difference that it
-- creates a circular orbit.
mkCircOrbit :: a -> Height -> Orbit a
mkCircOrbit b h = mkOrbit b h h 
  

-- | An orbit is only showable if the type of the centerBody is also
-- showable. As @ a @ should normaly be a 'Body' that should be not a
-- problem.
instance (Show a) => Show (Orbit a)
    where
      showsPrec _  Landed = showString "Landed"
      showsPrec _ obt
          = (showString "Around ")
          . (showString $ show . centerBody $ obt)
          . (showString " ( ")
          . (showString $ show . apoapsis $ obt)
          . (showString " / ")
          . (showString $ show . periapsis $ obt)
          . (showString ")")
