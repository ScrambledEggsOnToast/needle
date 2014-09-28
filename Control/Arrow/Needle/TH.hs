{-|
Module      : Control.Arrow.Needle.TH
Description : Template Haskell for needle
Copyright   : (c) 2014 Josh Kirklin
License     : MIT
Maintainer  : jjvk2@cam.ac.uk

This module combines the parsing from "Control.Arrow.Needle.Parse" with Template Haskell.
-}

{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Control.Arrow.Needle.TH (
    nd
  , ndFile
  ) where

import Prelude as Pre

import Control.Arrow.Needle.Parse
import Control.Arrow

import Control.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta

import Data.Either
import Data.Text as T
import Data.Map.Strict as M

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
    quoteExp = \str -> case (parseNeedle str) of
        Left e -> error . presentNeedleError $ e
        Right n -> arrowQ n
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
ndFile fp = do
    str <- runIO $ readFile fp
    case (parseNeedle str) of
        Left e -> error . presentNeedleError $ e
        Right n -> arrowQ n

-- | Convert NeedleArrow to ExpQ

arrowQ :: NeedleArrow -> ExpQ
arrowQ arrow = do
    let is = inputs arrow
    iNameMap <- M.fromList <$> mapM (\(a,b) -> ((a,b),) <$> newName ("_" ++ show a ++ "_" ++ show b)) is

    let iNames = Pre.map snd $ M.toList iNameMap
        iName (Input a b) = iNameMap ! (a,b)

        f i@(Input a b) = return 
            $ AppE (VarE $ mkName "arr") 
            $ LamE [TupP $ Pre.map VarP iNames] (VarE (iName i))

        f (Through a t) = do
            let ea = either error id $ parseExp . T.unpack $ t
            b <- f a
            return $ InfixE (Just b) (VarE $ mkName ">>>") (Just ea)

        f (Join as) = do
            aNames <- mapM (\n -> newName ("_" ++ show n)) [0..(Pre.length as - 1)]
            
            let tupleArrows [c] = f c
                tupleArrows (c:cs) = [| $(f c) &&& $(tupleArrows cs) |]

                tupleNames [n] = VarP n
                tupleNames (n:ns) = TupP [VarP n, tupleNames ns]

            b <- tupleArrows as

            return $ InfixE (Just b) (VarE $ mkName ">>>") $ Just
                $ AppE (VarE $ mkName "arr") 
                $ LamE [tupleNames aNames] (TupE $ Pre.map VarE aNames)

    f arrow

inputs :: NeedleArrow -> [(Int, Int)]
inputs (Input a b) = [(a,b)]
inputs (Through a _) = inputs a
inputs (Join as) = as >>= inputs
