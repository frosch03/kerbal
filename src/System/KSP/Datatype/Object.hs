module System.KSP.Datatype.Object
where

 ----------
-- Object --
 ----------

data Object
    = Object { name :: String }
      deriving (Eq)


instance Show Object
    where
      show = name
               
