module Control.Arrow.Needle.TH (
    nd
  , ndFile
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta

-- | The inline needle quasi-quoter.
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > 
-- > exampleArrow :: Num c => (a,b,c) -> (a,a,b,c,c)
-- > exampleArrow = [nd|
-- >    }===========>
-- >       \========>
-- >    }===========>
-- >    }==={negate}>
-- >       \========>
-- >  |]

nd :: QuasiQuoter
nd = QuasiQuoter { 
    quoteExp = undefined 
  , quotePat = error "Needles cannot be patterns."
  , quoteDec = error "Needles cannot be declarations."
  , quoteType = error "Needles cannot be types."
  }

-- | Load a needle from a file.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > exampleArrow :: Float -> Float
-- > exampleArrow = $(ndFile "example.nd")

ndFile :: FilePath -> ExpQ
ndFile = undefined
