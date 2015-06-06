{-|
Module      : Datatypes
Description : Helper module to get data types and functions on them.
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
module System.KSP.Datatypes
    ( Object(..)
    , Celestial(..)
    , Body(..)
    , Orbit(..)
    , System(..)
    , GravConst
    , KSystem
    , getNextUp
    , getPathUp
    , getDivid
    , sOrbitInSystem
    , pathOBetween
    , pathBetween_
    , pathBetween'
    , speeds
    )

where

import System.KSP.DataConstructors
import System.KSP.DataDestructors
