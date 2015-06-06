module KSP.Datatype.Celestial
    ( Celestial(..)
    )
where

import KSP.Datatype.System


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
