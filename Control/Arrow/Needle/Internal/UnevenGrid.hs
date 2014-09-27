{-|
Module      : Control.Arrow.Needle.Internal.UnevenGrid
Description : Data structure used in parsing needle diagrams
Copyright   : (c) Josh Kirklin
License     : MIT
Maintainer  : jjvk2@cam.ac.uk

Implements a 'Grid' data structure which can be examined and moved around with a monad 'GridExamine', used in needle parsing.
-}

{-# LANGUAGE DeriveFunctor, OverloadedLists, GeneralizedNewtypeDeriving, TupleSections#-}

module Control.Arrow.Needle.Internal.UnevenGrid (
    -- * Grids
    Grid ()
  , grid
  , prettyGrid
  , height
  , GridPosition
  , (!!?)
  , findPositions
    -- * Examining
  , GridExamine ()
  , gridExamine
  , branch
  , getGrid
  , getPosition
  , putPosition
  , modifyPosition
  , width
    -- ** Moving and accessing
  , hereGet
  , leftGet
  , rightGet
  , lUpGet
  , lDownGet
  , rUpGet
  , rDownGet
  ) where

import Prelude as Pre

import Data.Foldable as F

import Data.Vector as V
import Data.Maybe 
import Data.List 
import Data.Sequence as S

import Control.Applicative
import Control.Monad.State as St
import Control.Monad.Reader

import Control.Arrow

-- | A 'Grid' is a list of rows, the elements of which are not necessarily 
-- aligned, with the exception that the left edge of the grid is aligned.
--
-- So for example, the following is a valid shape for a 'Grid':
--
-- >  ____________
-- > |___|___|_|__|
-- > |__|_|_|__|_|__
-- > |_|______|_____|
-- >
--

newtype Grid e = Grid { rows :: Vector (GridRow e) }
    deriving (Eq, Functor, Show)

newtype GridRow e = GridRow { elements :: Vector (GridElem e) } 
    deriving (Eq, Functor, Show)

fromLeft :: GridRow e -> Int -> Maybe Int
fromLeft (GridRow row) n = V.findIndex ((> n) . end) $ row

data GridElem e = GridElem {
    end :: Int
  , unwrap :: e
} deriving (Eq, Functor, Show)

type GridPosition = (Int, Int)

-- | Generate a 'Grid' from a list of rows of elements and widths.

grid :: [[(e, Int)]] -> Grid e
grid rs = Grid . V.fromList . Pre.map (GridRow . V.fromList) $ gridElems
  where
    widths = Pre.map (Pre.map snd) rs
    es = Pre.map (Pre.map fst) rs
    f a = Pre.zipWith (+) (0 : f a) a
    ends = Pre.map f widths
    gridElems = Pre.zipWith (Pre.zipWith GridElem) ends es

-- | Pretty print a grid

prettyGrid :: (e -> Int -> String) -> Grid e -> String
prettyGrid eShow grid = unlines . Pre.map Pre.concat $ lShowns
  where
    untyped = V.map elements . rows $ grid
    widths = V.map ((\a -> V.zipWith (-) a (0 `cons` a)) . V.map end) untyped
    unwrappeds = V.map (V.map unwrap) untyped
    showns = V.zipWith (V.zipWith eShow) unwrappeds widths
    lShowns = Pre.map V.toList . V.toList $ showns

-- | Get the grid height

height :: Grid e -> Int
height = V.length . rows

-- | Get the element at a certain position in the grid. The top left block is
-- in the (0,0) position.

(!!?) :: Grid e -> GridPosition -> Maybe e
(!!?) grid (rowPos, elemPos) = do
    row <- rows grid !? rowPos
    element <- elements row !? elemPos
    return $ unwrap element

-- | Return a list of positions satisfying a predicate in a grid, in reading order.

findPositions :: (e -> Bool) -> Grid e -> [GridPosition]
findPositions pred = V.toList . V.concat . V.toList . V.map (\(i,r) -> V.map (i,) . V.findIndices (pred . unwrap) . elements $ r) . indexed . rows


-- | A monad for examining a grid.

newtype GridExamine e a = GridExamine (StateT GridPosition (Reader (Grid e)) a)
    deriving (Functor, Applicative, Monad)

-- | Run a grid examination. A start position must be specified.

gridExamine :: Grid e -> GridPosition -> GridExamine e a -> a
gridExamine grid start (GridExamine s) = runReader (evalStateT s start) grid

-- | Get the entire grid.

getGrid :: GridExamine e (Grid e)
getGrid = GridExamine (lift ask)

-- | Get the current position in the grid.

getPosition :: GridExamine e GridPosition
getPosition = GridExamine get

-- | Set the current position in the grid. 
-- This is unsafe -- an in bounds check is NOT performed.

putPosition :: GridPosition -> GridExamine e ()
putPosition = GridExamine . put

-- | Modify the current position in the grid.
-- This is unsafe -- an in bounds check is NOT performed.

modifyPosition :: (GridPosition -> GridPosition) -> GridExamine e ()
modifyPosition f = GridExamine (St.modify f)

here :: GridExamine e (Maybe (GridElem e))
here = do
    grid <- getGrid
    (rowPos, elemPos) <- getPosition
    return $ do
        row <- rows grid !? rowPos
        element <- elements row !? elemPos
        return element

-- | Get the element at the current position.

hereGet :: GridExamine e (Maybe e)
hereGet = do
    element <- here
    return $ fmap unwrap element

-- | Move to and return the grid element to the left. If there is no element to 
-- the left, returns Nothing.

leftGet :: GridExamine e (Maybe e)
leftGet = do
    (rowPos, elemPos) <- getPosition
    if elemPos > 0 
        then putPosition (rowPos, elemPos - 1) >> (hereGet)
        else return Nothing

-- | Move to and return the grid element to the right. If there is no element to 
-- the right, returns Nothing.

rightGet :: GridExamine e (Maybe e)
rightGet = do
    grid <- getGrid
    (rowPos, elemPos) <- getPosition
    fromMaybe (return Nothing) $ do
        row <- rows grid !? elemPos 
        element <- elements row !? (elemPos + 1)
        return $ putPosition (rowPos, elemPos + 1) >> return (Just $ unwrap element)

-- | Move to and return the grid element above the column a certain distance 
-- from the right edge of the current element. If there is no such element, returns 
-- Nothing.

rUpGet :: Int -> GridExamine e (Maybe e)
rUpGet n = do
    grid <- getGrid
    mCurrentElement <- here
    (rowPos, _) <- getPosition
    fromMaybe (return Nothing) $ do
        currentElement <- mCurrentElement
        newRow <- rows grid !? (rowPos - 1)
        i <- fromLeft newRow (end currentElement - 1 - n)
        return $ putPosition (rowPos - 1, i) >> hereGet

-- | Move to and return the grid element below the column a certain distance 
-- from the right edge of the current element. If there is no such element, returns 
-- Nothing.

rDownGet :: Int -> GridExamine e (Maybe e)
rDownGet n = do
    grid <- getGrid
    mCurrentElement <- here
    (rowPos, _) <- getPosition
    fromMaybe (return Nothing) $ do
        currentElement <- mCurrentElement
        newRow <- rows grid !? (rowPos + 1)
        i <- fromLeft newRow (end currentElement - 1 - n)
        return $ putPosition (rowPos + 1, i) >> hereGet

-- | Move to and return the grid element above the column a certain distance 
-- from the left edge of the current element. If there is no such element, returns 
-- Nothing.

lUpGet :: Int -> GridExamine e (Maybe e)
lUpGet n = do
    grid <- getGrid
    (rowPos, elemPos) <- getPosition
    fromMaybe (return Nothing) $ do
        row <- rows grid !? rowPos
        let start = fromMaybe 0 $ end <$> (elements row !? (elemPos - 1))
        newRow <- rows grid !? (rowPos - 1)
        i <- fromLeft newRow (start + n)
        return $ putPosition (rowPos - 1, i) >> hereGet

-- | Move to and return the grid element below the column a certain distance 
-- from the left edge of the current element. If there is no such element, returns 
-- Nothing.

lDownGet :: Int -> GridExamine e (Maybe e)
lDownGet n = do
    grid <- getGrid
    (rowPos, elemPos) <- getPosition
    fromMaybe (return Nothing) $ do
        row <- rows grid !? rowPos
        let start = fromMaybe 0 $ end <$> (elements row !? (elemPos - 1))
        newRow <- rows grid !? (rowPos + 1)
        i <- fromLeft newRow (start + n)
        return $ putPosition (rowPos + 1, i) >> hereGet

-- | Branch an examination, i.e. perform it and then return to the original position.

branch :: GridExamine e a -> GridExamine e a
branch ge = do
    pos <- getPosition
    x <- ge
    putPosition pos
    return x

-- | The width of the current element

width :: GridExamine e (Maybe Int)
width = do
    grid <- getGrid
    (rowPos, elemPos) <- getPosition
    mh <- here
    return $ do
        h <- mh
        row <- rows grid !? rowPos
        let start = fromMaybe 0 $ end <$> (elements row !? (elemPos - 1))
        return (end h - start)
