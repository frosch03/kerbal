module KSP.Datatype.Orbit
where

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

type Height = Double
mkOrbit :: a -> Height -> Height -> Orbit a
mkOrbit b ap per
    = O { centerBody  = b
        , apoapsis    = ap
        , periapsis   = per
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
      
mkCircOrbit :: a -> Height -> Orbit a
mkCircOrbit b h
    = O { centerBody  = b
        , apoapsis    = h
        , periapsis   = h
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
  

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
