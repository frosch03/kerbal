{-# LANGUAGE GADTs, FlexibleInstances #-}
module KSP.DataConstructors
    ( Object(..)
    , Celestial(..)
    , Body(..)
    , Orbit(..)
    , System(..)
    , GravConst
    , KSystem
    )
where

import KSP.Datatype.Body
import KSP.Datatype.Orbit
import KSP.Datatype.System

type GravConst = Double
type KSystem = System Body
