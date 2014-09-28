{-|
Module      : Control.Arrow.Needle.Parse
Description : Parsing needle diagrams
Copyright   : (c) 2014 Josh Kirklin
License     : MIT
Maintainer  : jjvk2@cam.ac.uk

This module's main export is 'parseNeedle', which parses a needle diagram into a `NeedleArrow`.
-}

module Control.Arrow.Needle.Parse (
  -- * Parsing needles
    NeedleArrow (..)
  , parseNeedle
  -- * Errors
  , NeedleError (..)
  , presentNeedleError
  ) where

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import Data.Maybe
import Data.Either
import Data.Monoid

import Text.Parsec as P
import Text.Parsec.Extra (natural)
import Data.Char

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State
import Control.Arrow

import Control.Arrow.Needle.Internal.UnevenGrid as G

--------------------------------
-- Types
--------------------------------

-- | The datatype representing a generic needle arrow.

data NeedleArrow = Input Int Int
                 | Through NeedleArrow T.Text
                 | Join [NeedleArrow]
    deriving (Show, Read, Eq)

-- | The grid element for the first round of parsing.

data NeedleElem = None
                | Track
                | In Int Int
                | Out
                | LabelIn T.Text
                | LabelOut T.Text
                | ExtArrow T.Text
                | Switch Direction
                | TunnelEntrance
                | TunnelExit
    deriving (Show, Read, Eq)

-- | Errors in parsing.

data NeedleError = ParseError String
                 | ConstructionError String

instance Show NeedleError where
    show = presentNeedleError

-- | Present the error.

presentNeedleError :: NeedleError -> String
presentNeedleError (ParseError s) = "Needle parse error:\n"++s
presentNeedleError (ConstructionError s) = "Needle construction error:\n"++s

data Direction = Up | Down
    deriving (Show, Read, Eq)

type NeedleGrid = Grid NeedleElem

--------------------------------
-- String -> NeedleArrow
--------------------------------

-- | Parse a string to a needle

parseNeedle :: String -> Either NeedleError NeedleArrow
parseNeedle = parseNeedleGrid >=> gridArrow

--------------------------------
-- NeedleGrid -> NeedleArrow
--------------------------------

gridArrow :: NeedleGrid -> Either NeedleError NeedleArrow
gridArrow grid = do
        os <- mapM (arrowToPosition grid) $ outputPositions grid
        maybe (Left $ ConstructionError "No outputs") return $ arrowJoin os

outputPositions :: NeedleGrid -> [GridPosition]
outputPositions = findPositions (== Out)

findLabelOutPosition :: T.Text -> GridExamine NeedleElem (Maybe GridPosition)
findLabelOutPosition t = do
    grid <- getGrid
    return $ listToMaybe (findPositions (== (LabelOut t)) grid)

arrowJoin :: [NeedleArrow] -> Maybe NeedleArrow
arrowJoin [] = Nothing
arrowJoin [a] = Just a
arrowJoin as = Just $ Join as

arrowToPosition :: NeedleGrid -> GridPosition -> Either NeedleError NeedleArrow
arrowToPosition grid pos = gridExamine grid pos go
  where
    err = return . Left . ConstructionError
    success = return . Right
    
    tryPath path = branch $ do
        mp <- path
        case mp of
            Nothing -> err "Nothing on this path"
            Just _ -> go

    go = do
        mh <- hereGet
        case mh of
            Nothing -> err "Position not in grid"
            Just h -> case h of
                None -> err "Arrow from nothing"
                Track -> do
                    w <- fromJust <$> width
                    ups <- forM [0 .. (w - 1)] $ \n -> tryPath $ do
                        e <- lUpGet n
                        return $ mfilter (== (Switch Down)) e
                    downs <- forM [0 .. (w - 1)] $ \n -> tryPath $ do
                        e <- lDownGet n
                        return $ mfilter (== (Switch Up)) e
                    left <- tryPath leftGet
                    let paths = rights $ left : ups ++ downs
                        mJoint = arrowJoin paths
                    case mJoint of
                        Nothing -> do
                            (n, _) <- G.getPosition
                            err $ "Track from nowhere on line " ++ show (n + 1)
                        Just joint -> success joint
                In n m -> success $ Input n m
                Out -> do
                    ml <- leftGet
                    case ml of
                        Just l -> go
                        Nothing -> err "An output has no arrow going into it"
                LabelIn t -> do
                    mlo <- findLabelOutPosition t
                    case mlo of
                        Just lo -> putPosition lo >> go
                        Nothing -> err $ "Found label-in '" ++ T.unpack t ++ "' with no label-out"
                LabelOut t -> do
                    ml <- leftGet
                    case ml of
                        Just l -> go
                        Nothing -> err $ "Label-out '" ++ T.unpack t ++ "' has no arrow going into it"
                ExtArrow t -> do
                    left <- tryPath leftGet
                    up <- tryPath $ do
                        e <- lUpGet 0
                        return $ mfilter (== (Switch Down)) e
                    down <- tryPath $ do
                        e <- lDownGet 0
                        return $ mfilter (== (Switch Up)) e
                    let paths = rights $ [left,up,down]
                        mJoint = arrowJoin paths
                    case mJoint of
                        Nothing -> do
                            (n, _) <- G.getPosition
                            err $ "External arrow '" ++ T.unpack t ++ "' on line " ++ show (n + 1) ++ " has no arrow going into it"
                        Just joint -> success $ Through joint t
                Switch d -> do
                    left <- tryPath leftGet
                    continuing <- tryPath $ do
                        e <- case d of
                            Down -> lUpGet 0
                            Up -> lDownGet 0
                        return $ mfilter (== h) e
                    let paths = rights $ [left, continuing]
                        mJoint = arrowJoin paths
                    case mJoint of
                        Nothing -> do
                            (n, _) <- G.getPosition
                            err $ "Line switch from nowhere on line " ++ (show n)
                        Just joint -> success joint
                TunnelExit -> do
                    let tunnel n = if n == 0 
                            then go
                            else do
                                ml <- leftGet
                                case ml of
                                    Nothing -> do
                                        (n,_) <- G.getPosition
                                        err $ "Tunnel from nowhere on line " ++ (show n)
                                    Just TunnelExit -> tunnel (n+1)
                                    Just TunnelEntrance -> tunnel (n-1)
                                    Just _ -> tunnel n
                    tunnel 1
                TunnelEntrance -> do
                    ml <- leftGet
                    case ml of
                        Nothing -> do
                            (n,_) <- G.getPosition
                            err $ "Tunnel entrance has no arrow going into it on line " ++ (show n)
                        Just _ -> go

--------------------------------
-- String -> NeedleGrid
--------------------------------

-- | Pretty print a needle grid

prettyNeedleGrid :: NeedleGrid -> String
prettyNeedleGrid = prettyGrid prettyElem
  where
    prettyElem None           n = replicate n ' '
    prettyElem Track          n = replicate n '='
    prettyElem (In _ _)       n = replicate (n-1) ' ' ++ "}"
    prettyElem Out            n = ">" ++ replicate (n-1) ' '
    prettyElem (LabelIn t)    n = replicate (n - 1 - length s) ' ' ++ s ++ ":"
      where 
        s = T.unpack t
    prettyElem (LabelOut t)   n = ":" ++ s ++ replicate (n - 1 - length s) ' ' 
      where 
        s = T.unpack t
    prettyElem (ExtArrow t)   n = "{" ++ s ++ replicate (n - 2 - length s) ' ' ++ "}"
      where
        s = T.unpack t
    prettyElem (Switch Up)    n = replicate n '/'
    prettyElem (Switch Down)  n = replicate n '\\'
    prettyElem TunnelEntrance n = replicate n ')'
    prettyElem TunnelExit     n = replicate n '('

-- | Parse a needle grid

parseNeedleGrid :: String -> Either NeedleError NeedleGrid
parseNeedleGrid s = case result of
    Left pe -> Left . ParseError $
            "line " ++ (show . sourceLine . errorPos $ pe) ++ ":\n" ++
            ls !! ((sourceLine . errorPos $ pe) - 1) ++ "\n" ++
            replicate ((sourceColumn . errorPos $ pe) - 1) ' ' ++ "^"
    Right x -> Right (grid x)
  where
    result = zipWithM parseLine ls [1..]
    ls = lines s
    parseLine l n = runParser (do
        p <- P.getPosition
        setPosition $ setSourceLine p n
        es <- many (withWidth . choice . map try $ elemParsers n)
        optional $ try (string "-- " >> many anyChar)
        eof
        return es) 0 "needle expression" l
    withWidth p = do
        c1 <- sourceColumn <$> P.getPosition
        x <- p
        c2 <- sourceColumn <$> P.getPosition
        return (x, c2 - c1)

    elemParsers n = [
        do
            many1 space
            return None
      , do
            many1 (char '=')
            return Track
      , do
            void (char '}')
            m <- getState
            modifyState (+1)
            return $ In n m
      , do
            void (char '>')
            return Out
      , do
            l <- many1 letter
            spaces
            void (char ':')
            return $ LabelIn (T.pack l)
      , do
            void (char ':')
            spaces
            l <- many1 letter
            return $ LabelOut (T.pack l)
      , do
            void (char '{')
            f <- anyChar
            l <- manyTill anyChar (char '}')
            return $ ExtArrow (T.pack $ f : l)
      , do
            void (char '/')
            return $ Switch Up
      , do
            void (char '\\')
            return $ Switch Down
      , do
            void (char ')')
            return TunnelEntrance
      , do
            void (char '(')
            return TunnelExit
      ]
    
