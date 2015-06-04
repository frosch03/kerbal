module KerbalSystem
where

import Kerbal

kMoho
    = Railed (Object "Moho"  )
      (Celestial   250e3 2.5263617e21  9646663 [])
kGilly
    = Railed (Object "Gilly" )
      (Celestial 13e3 1.2420512e17 126123.27 []) 
kEve
    = Railed (Object "Eve")
      (Celestial 700e3 1.2244127e23 85109365
                     [ (kGilly, kGillyOrbit) ]) 
kMun
    = Railed (Object "Mun" )
      (Celestial 200e3 9.7600236e20 2429559.1 [])
kMinmus
    = Railed (Object "Minmus")
      (Celestial 60e3 2.6457897e19 2247428.4 [])
kKerbin
    = Railed (Object "Kerbin")
      (Celestial 600e3 5.2915793e22 84159286
                     [ (kMun, kMunOrbit)
                     , (kMinmus, kMinmusOrbit) ])
kIke
    = Railed (Object "Ike" )
      (Celestial 130e3 2.7821949e20 1049598.9 [])
kDres
    = Railed (Object "Dres" )
      (Celestial 138e3 3.2191322e20 32832840 [])
kDuna
    = Railed (Object "Duna" )
      (Celestial 320e3 4.5154812e21 47921949
                     [ (kIke, kIkeOrbit)
                     , (kDres, kDresOrbit) ])
kLaythe
    = Railed (Object "Laythe")
      (Celestial 500e3 2.9397663e22 3723645.8 [])
kVall
    = Railed (Object "Vall" )
      (Celestial 300e3 3.1088028e21 2406401.4 [])
kTylo
    = Railed (Object "Tylo" )
      (Celestial 600e3 4.2332635e22 10856518 [])
kBop
    = Railed (Object "Bop" )
      (Celestial 65e3 3.7261536e19 1221060.9 [])
kPol
    = Railed (Object "Pol" )
      (Celestial 44e3 1.0813636e19 1042138.9 [])
kJool
    = Railed (Object "Jool" )
      (Celestial 6e6 4.2332635e24 2.4559852e9
                     [ (kLaythe, kLaytheOrbit)
                     , (kVall, kVallOrbit)
                     , (kTylo, kTyloOrbit)
                     , (kBop, kBopOrbit)
                     , (kPol, kPolOrbit) ])
kEeloo
    = Railed (Object "Eeloo" )
      (Celestial 210e3 1.1149358e21 1.1908294e8 [])
kKerbol
    = Railed (Object "Kerbol")
      (Celestial 261.6e6 1.7565670e28 (1/0)
                     [ (kMoho, kMohoOrbit)
                     , (kEve, kEveOrbit)
                     , (kKerbin, kKerbinOrbit)
                     , (kDuna, kDunaOrbit)
                     , (kJool, kJoolOrbit)
                     , (kEeloo, kEelooOrbit) ])

kGillyOrbit
    = O { centerBody = kEve
        , apoapsis = ( 8.825e6 - 700e3)
        , periapsis = (14.175e6 - 700e3)
        , inclination = Just 12
        , omega_big   = Just 80
        , omega_small = Just 10 }
              
kMunOrbit
    = O { centerBody = kKerbin
        , apoapsis = ( 12e6 - 600e3)
        , periapsis = (12e6 - 600e3)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
            
kMinmusOrbit
    = O { centerBody = kKerbin
        , apoapsis = ( 47e6 - 600e3)
        , periapsis = (47e6 - 600e3)
        , inclination = Just 6
        , omega_big   = Just 78
        , omega_small = Just 38 }
               
kIkeOrbit
    = O { centerBody = kDuna
        , apoapsis = ( 3.296e6 - 320e3)
        , periapsis = (3.104e6 - 320e3)
        , inclination = Just 0.03
        , omega_big   = Nothing
        , omega_small = Nothing }
            
kLaytheOrbit
    = O { centerBody = kJool
        , apoapsis = ( 27.184e6 - 6e6)
        , periapsis = ( 27.184e6 - 6e6)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
               
kVallOrbit
    = O { centerBody = kJool
        , apoapsis = ( 43.152e6 - 6e6)
        , periapsis = ( 43.152e6 - 6e6)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
             
kTyloOrbit
    = O { centerBody = kJool
        , apoapsis = ( 68.5e6 - 6e6)
        , periapsis = ( 68.5e6 - 6e6)
        , inclination = Just 0.025
        , omega_big   = Nothing
        , omega_small = Nothing }
             
kBopOrbit
    = O { centerBody = kJool
        , apoapsis = (158.6975e6 - 6e6)
        , periapsis = ( 98.3025e6 - 6e6)
        , inclination = Just 15
        , omega_big   = Just 10
        , omega_small = Just 25 }
            
kPolOrbit
    = O { centerBody = kJool
        , apoapsis = (210.624206e6 - 6e6)
        , periapsis = (149.155794e6 - 6e6)
        , inclination = Just 4.25
        , omega_big   = Just 2
        , omega_small = Just 15 }
            
kMohoOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 6315765980 -261.6e6)
        , periapsis = ( 4210510628 -261.6e6)
        , inclination = Just 7
        , omega_big   = Just 70
        , omega_small = Just 15 }
             
kEveOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 9931011387 -261.6e6)
        , periapsis = ( 9734357701 -261.6e6)
        , inclination = Just 2.1
        , omega_big   = Just 15
        , omega_small = Nothing }
            
kKerbinOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 13599840256 -261.6e6)
        , periapsis = (13599840256 -261.6e6)
        , inclination = Nothing
        , omega_big   = Nothing
        , omega_small = Nothing }
               
kDunaOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 21783189163 -261.6e6)
        , periapsis = (19669121365 -261.6e6)
        , inclination = Just 0.06
        , omega_big   = Just 135.5
        , omega_small = Nothing }
             
kDresOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 46761053522 -261.6e6)
        , periapsis = (34917642884 -261.6e6)
        , inclination = Just 5
        , omega_big   = Just 280
        , omega_small = Just 90 }
             
kJoolOrbit
    = O { centerBody = kKerbol
        , apoapsis = ( 72212238387 -261.6e6)
        , periapsis = (65334882253 -261.6e6)
        , inclination = Just 1.302
        , omega_big   = Just 52
        , omega_small = Nothing }
             
kEelooOrbit
    = O { centerBody = kKerbol
        , apoapsis = (113549713200 -261.6e6)
        , periapsis = (66687926800 -261.6e6)
        , inclination = Just 6.15
        , omega_big   = Just 50
        , omega_small = Just 260 }
