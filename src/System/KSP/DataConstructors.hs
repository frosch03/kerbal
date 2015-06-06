module System.KSP.DataConstructors
    ( Object(..)
    , Celestial(..)
    , Body(..)
    , Orbit(..)
    , System(..)
    , GravConst
    , KSystem
    )
where

-- | The Module "System.KSP.DataConstructors" imports everything that is a
-- data constructor and exports them. 

import System.KSP.Datatype.Body
import System.KSP.Datatype.Orbit
import System.KSP.Datatype.System

-- | 'GravConst' is the type of the gravitation constant, which is
-- obious a double.
type GravConst = Double

-- | 'KSystem' creates a data constructor, that binds the
-- 'System.KSP.Datatype.System' to a 'System.KSP.Datatype.System' of
-- 'System.KSP.Datatype.Orbit's around 'System.KSP.Datatype.Body's.
type KSystem = System Body
