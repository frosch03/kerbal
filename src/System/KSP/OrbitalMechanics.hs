module System.KSP.OrbitalMechanics
where

import System.KSP.DataConstructors


type Radius = Double -- Radius of Orbit at the current Position
type Speed  = Double


var_G :: GravConst
var_G = 6.674e-11


semiMajor   :: Orbit Body -> Double
semiMajor o
    = 0.5 * ((apoapsis o) + (periapsis o) + (2 * (r . celestial . centerBody $ o)))


v :: Orbit Body -> Radius -> Speed
v o radius =  sqrt $ mue * ( (2/radius) - (1/(semiMajor o)) ) 
    where
      mue = var_G * (m . celestial . centerBody $ o)


v_e :: Body -> Speed
v_e b =  sqrt $ 2 * mue / (r . celestial $ b) 
    where
      mue = var_G * (m . celestial $ b)

      

hohmann :: Orbit Body -> Orbit Body -> (Double, Double)
hohmann o1 o2
    |   centerBody o1 /= centerBody o2
      || o1 == o2
    = (0,0)
hohmann o1 o2
    = ( sqrt(mue/r1) * (sqrt((2*r2)/(r1+r2)) - 1) 
      , sqrt(mue/r2) * (1 - (sqrt((2*r1)/(r1+r2))))
      )
    where 
      mue = var_G * (m . celestial . centerBody $ o1)
      r1 = (r . celestial . centerBody $ o1) + (apoapsis o1)
      r2 = (r . celestial . centerBody $ o2) + (apoapsis o2)
