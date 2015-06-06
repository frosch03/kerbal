{-|
Module      : Universe
Description : This module contains planets moons and kerbol 
              as well as the corresponding orbits
License     : CC0
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}
module System.KSP.Universe
where

import System.KSP.Datatypes

-- | 'kMoho' is the definition of Moho
kMoho
    = Railed (Object "Moho"  )
      (Celestial   250e3 2.5263617e21  9646663 Empty)

-- | 'kGilly' is the definition of Gilly
kGilly
    = Railed (Object "Gilly" )
      (Celestial 13e3 1.2420512e17 126123.27 Empty) 

-- | 'kEve' is the definition of Eve
kEve
    = Railed (Object "Eve")
      (Celestial 700e3 1.2244127e23 85109365
             (System [ (kGillyOrbit, kGilly) ])) 

-- | 'kMun' is the definition of Mun
kMun
    = Railed (Object "Mun" )
      (Celestial 200e3 9.7600236e20 2429559.1 Empty)

-- | 'kMinmus' is the definition of Minmus
kMinmus
    = Railed (Object "Minmus")
      (Celestial 60e3 2.6457897e19 2247428.4 Empty)

-- | 'kKerbin' is the definition of Kerbin
kKerbin
    = Railed (Object "Kerbin")
      (Celestial 600e3 5.2915793e22 84159286
             (System [ (kMunOrbit, kMun)
                     , (kMinmusOrbit, kMinmus) ]))

-- | 'kIke' is the definition of Ike
kIke
    = Railed (Object "Ike" )
      (Celestial 130e3 2.7821949e20 1049598.9 Empty)

-- | 'kDres' is the definition of Dres
kDres
    = Railed (Object "Dres" )
      (Celestial 138e3 3.2191322e20 32832840 Empty)

-- | 'kDuna' is the definition of Duna
kDuna
    = Railed (Object "Duna" )
      (Celestial 320e3 4.5154812e21 47921949
             (System [ (kIkeOrbit, kIke)
                     , (kDresOrbit, kDres) ]))

-- | 'kLaythe' is the definition of Laythe
kLaythe
    = Railed (Object "Laythe")
      (Celestial 500e3 2.9397663e22 3723645.8 Empty)

-- | 'kVall' is the definition of Vall
kVall
    = Railed (Object "Vall" )
      (Celestial 300e3 3.1088028e21 2406401.4 Empty)

-- | 'kTylo' is the definition of Tylo
kTylo
    = Railed (Object "Tylo" )
      (Celestial 600e3 4.2332635e22 10856518 Empty)

-- | 'kBop' is the definition of Bop
kBop
    = Railed (Object "Bop" )
      (Celestial 65e3 3.7261536e19 1221060.9 Empty)

-- | 'kPol' is the definition of Pol
kPol
    = Railed (Object "Pol" )
      (Celestial 44e3 1.0813636e19 1042138.9 Empty)

-- | 'kJool' is the definition of Jool
kJool
    = Railed (Object "Jool" )
      (Celestial 6e6 4.2332635e24 2.4559852e9
             (System [ (kLaytheOrbit, kLaythe)
                     , (kVallOrbit, kVall)
                     , (kTyloOrbit, kTylo)
                     , (kBopOrbit, kBop)
                     , (kPolOrbit, kPol) ]))

-- | 'kEeloo' is the definition of Eeloo
kEeloo
    = Railed (Object "Eeloo" )
      (Celestial 210e3 1.1149358e21 1.1908294e8 Empty)

-- | 'kKerbol' is the definition of Kerbol
kKerbol
    = Railed (Object "Kerbol")
      (Celestial 261.6e6 1.7565670e28 (1/0)
             (System [ (kMohoOrbit, kMoho)
                     , (kEveOrbit, kEve)
                     , (kKerbinOrbit, kKerbin)
                     , (kDunaOrbit, kDuna)
                     , (kJoolOrbit, kJool)
                     , (kEelooOrbit, kEeloo) ]))



-- | 'kGillyOrbit' is the definition of the Orbit of Gilly
kGillyOrbit
    = O { centerBody = kEve
        , apoapsis = ( 8.825e6 - 700e3)
        , periapsis = (14.175e6 - 700e3)
        , inclination = Just 12
        , omega_big   = Just 80
        , omega_small = Just 10 }
              

-- | 'kMunOrbit' is the definition of the Orbit of Mun
kMunOrbit
    = O { centerBody = kKerbin
        , apoapsis = ( 12e6 - 600e3)
        , periapsis = (12e6 - 600e3)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
            

-- | 'kMinmusOrbit' is the definition of the Orbit of Minmus
kMinmusOrbit
    = O { centerBody = kKerbin
        , apoapsis = ( 47e6 - 600e3)
        , periapsis = (47e6 - 600e3)
        , inclination = Just 6
        , omega_big   = Just 78
        , omega_small = Just 38 }
               

-- | 'kIkeOrbit' is the definition of the Orbit of Ike
kIkeOrbit
    = O { centerBody = kDuna
        , apoapsis = ( 3.296e6 - 320e3)
        , periapsis = (3.104e6 - 320e3)
        , inclination = Just 0.03
        , omega_big   = Nothing
        , omega_small = Nothing }
            

-- | 'kLaytheOrbit' is the definition of the Orbit of Laythe
kLaytheOrbit
    = O { centerBody = kJool
        , apoapsis = ( 27.184e6 - 6e6)
        , periapsis = ( 27.184e6 - 6e6)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
               

-- | 'kVallOrbit' is the definition of the Orbit of Vall
kVallOrbit
    = O { centerBody = kJool
        , apoapsis = ( 43.152e6 - 6e6)
        , periapsis = ( 43.152e6 - 6e6)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
             

-- | 'kTyloOrbit' is the definition of the Orbit of Tylo
kTyloOrbit
    = O { centerBody = kJool
        , apoapsis = ( 68.5e6 - 6e6)
        , periapsis = ( 68.5e6 - 6e6)
        , inclination = Just 0.025
        , omega_big   = Nothing
        , omega_small = Nothing }
             

-- | 'kBopOrbit' is the definition of the Orbit of Bop
kBopOrbit
    = O { centerBody = kJool
        , apoapsis = (158.6975e6 - 6e6)
        , periapsis = ( 98.3025e6 - 6e6)
        , inclination = Just 15
        , omega_big   = Just 10
        , omega_small = Just 25 }
            

-- | 'kPolOrbit' is the definition of the Orbit of Pol
kPolOrbit
    = O { centerBody = kJool
        , apoapsis = (210.624206e6 - 6e6)
        , periapsis = (149.155794e6 - 6e6)
        , inclination = Just 4.25
        , omega_big   = Just 2
        , omega_small = Just 15 }
            

-- | 'kMohoOrbit' is the definition of the Orbit of Moho
kMohoOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 6315765980 -261.6e6)
        , periapsis = ( 4210510628 -261.6e6)
        , inclination = Just 7
        , omega_big   = Just 70
        , omega_small = Just 15 }
             

-- | 'kEveOrbit' is the definition of the Orbit of Eve
kEveOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 9931011387 -261.6e6)
        , periapsis = ( 9734357701 -261.6e6)
        , inclination = Just 2.1
        , omega_big   = Just 15
        , omega_small = Nothing }
            

-- | 'kKerbinOrbit' is the definition of the Orbit of Kerbin
kKerbinOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 13599840256 -261.6e6)
        , periapsis = (13599840256 -261.6e6)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
               

-- | 'kDunaOrbit' is the definition of the Orbit of Duna
kDunaOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 21783189163 -261.6e6)
        , periapsis = (19669121365 -261.6e6)
        , inclination = Just 0.06
        , omega_big   = Just 135.5
        , omega_small = Nothing }
             

-- | 'kDresOrbit' is the definition of the Orbit of Dres
kDresOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 46761053522 -261.6e6)
        , periapsis = (34917642884 -261.6e6)
        , inclination = Just 5
        , omega_big   = Just 280
        , omega_small = Just 90 }
             

-- | 'kJoolOrbit' is the definition of the Orbit of Jool
kJoolOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 72212238387 -261.6e6)
        , periapsis = (65334882253 -261.6e6)
        , inclination = Just 1.302
        , omega_big   = Just 52
        , omega_small = Nothing }
             

-- | 'kEelooOrbit' is the definition of the Orbit of Eeloo
kEelooOrbit
    = O { centerBody = kKerbol
        , apoapsis = (113549713200 -261.6e6)
        , periapsis = (66687926800 -261.6e6)
        , inclination = Just 6.15
        , omega_big   = Just 50
        , omega_small = Just 260 }
