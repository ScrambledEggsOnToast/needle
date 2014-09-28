
{-# LANGUAGE QuasiQuotes #-}
module Test where
import Control.Arrow.Needle


nTest :: (Int, Int, Int) -> (Int, Int)
nTest = [nd|
                        aLabel:==={div 2}===\
    }====\                                  \
         {uncurry (+)}==\=================) \ (==>
    }====/              \                   \
                        \                   \===>
                        \               
                     }=={uncurry (-)}====:aLabel
|]

nTriple :: a -> (a,a,a)
nTriple = [nd|
    }==\==>
       \==>
       \==>
|]
