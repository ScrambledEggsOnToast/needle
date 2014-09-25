module Control.Arrow.Needle.Parse where

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import Data.Maybe

import Text.Parsec
import Text.Parsec.Extra

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State

newtype NExtArrow = NExtArrow T.Text deriving (Show, Eq)
newtype NType = NType (Maybe T.Text) deriving (Show, Eq)
newtype NLabel = NLabel T.Text deriving (Show, Eq)

data SwitchDir = SUp | SDown | SBoth deriving (Show, Eq)

data NTrackElem = NTrackIn Int NType
                | NTrackOut
                | NTrackExtArrow NExtArrow
                | NTrackLabelIn NLabel
                | NTrackLabelOut NLabel
                | NTrackSwitch SwitchDir
                | NTrackTunnelEntrance
                | NTrackTunnelExit
    deriving (Show, Eq)

type Columned a = (a, (Column, Column))

newtype NTrack = NTrack [Columned NTrackElem] deriving (Show, Eq)

newtype NLine = NLine [NTrack] deriving (Show, Eq)

type NLines = [NLine]

(!) :: [a] -> Int -> Maybe a
(!) [] _ = Nothing
(!) (a:as) 0 = Just a
(!) (_:as) n = as ! (n-1)

trRanges :: NLine -> Maybe [(Column,Column)]
trRanges (NLine ts) = mapM (\(NTrack es) -> (,) <$> (listToMaybe . map (fst . snd) $ es) <*>  (listToMaybe . reverse . map (snd . snd) $ es)) ts

nLookup :: Line -> Column -> NLines -> Maybe (Either (Columned NTrackElem) (Columned NTrackElem, Columned NTrackElem))
nLookup r c ls = do
    l@(NLine ts) <- ls ! r
    trRs <- trRanges l
    trN <- listToMaybe . map fst . filter ((>c) . snd . snd) . filter ((<=c) . fst . snd) $ zip [0..] trRs
    (NTrack es) <- ts ! trN
    let me = listToMaybe . filter ((>c) . snd . snd) . filter ((<=c) . fst . snd) $ es
    case me of
        Just e -> return $ Left e
        Nothing -> do
            e1 <- listToMaybe . reverse . filter ((<c) . fst . snd) $ es
            e2 <- listToMaybe . filter ((>=c) . snd . snd) $ es
            return $ Right (e1, e2)

type NLineParser = Parsec String Int

parseLines :: String -> Either ParseError [NLine]
parseLines = runParser (do
    ls <- many aLine
    eof
    return ls) 0 ""

aLine :: NLineParser NLine
aLine = do
    ms <- manyTill (spaces >> aTrack) (eol <?> "eol")
    return $ NLine (ms)

aTrack :: NLineParser NTrack
aTrack = try (do
    firstElem <- columned $ aTrackIn 
                        <|> aTrackSwitcher 
                        <|> aTrackExtArrow 
                        <|> aTrackTunnelExit 
                        <|> aTrackLabelIn

    let go bs = (rails >>) $ (do
            b <- columned aTrackSwitcher
            let bs' = bs ++ [b]
            go bs' <|> return bs') <|> (do
            b <- columned aTrackExtArrow
            let bs' = bs ++ [b]
            go bs') <|> (do
            b <- columned $ aTrackLabelOut
                        <|> aTrackOut 
                        <|> aTrackTunnelEntrance
            let bs' = bs ++ [b]
            return bs')
    otherElems <- go []
    return . NTrack $ firstElem : otherElems) <|> (do
    s <- columned aTrackSwitcher
    return . NTrack $ [s])

columned :: NLineParser a -> NLineParser (Columned a)
columned p = do
    startColumn <- sourceColumn <$> getPosition
    a <- p
    endColumn <- sourceColumn <$> getPosition
    return (a, (startColumn, endColumn))

aTrackIn :: NLineParser NTrackElem
aTrackIn = (do
    t <- aType
    spaces
    void $ char '}'
    modifyState (+1)
    n <- getState
    return $ NTrackIn n t) <?> "arrow input"

aTrackOut :: NLineParser NTrackElem
aTrackOut = (do
    void $ char '>'
    return $ NTrackOut) <?> "arrow output"

aTrackExtArrow :: NLineParser NTrackElem
aTrackExtArrow = (do
    a <- between (char '{' >> spaces) (spaces >> char '}') . many $ do
        c <- lookAhead (spaces >> anyChar)
        if c == '}'
            then fail "close"
            else anyChar
    return $ NTrackExtArrow (NExtArrow $ T.pack a)) <?> "external arrow"

aTrackSwitcher :: NLineParser NTrackElem
aTrackSwitcher = (do 
    void $ char '\\'
    return $ NTrackSwitch SDown) <|> (do 
    void $ char '/'
    return $ NTrackSwitch SUp) <|> (do 
    void $ char 'X'
    return $ NTrackSwitch SBoth) <?> "track switcher"

aTrackTunnelEntrance :: NLineParser NTrackElem
aTrackTunnelEntrance = (do
    void $ char ')'
    return $ NTrackTunnelEntrance) <?> "tunnel entrance"

aTrackTunnelExit :: NLineParser NTrackElem
aTrackTunnelExit = (do
    void $ char '('
    return $ NTrackTunnelExit) <?> "tunnel exit"

aTrackLabelIn :: NLineParser NTrackElem
aTrackLabelIn = (do
    t <- aLabel
    spaces
    void $ char ':'
    return $ NTrackLabelIn t) <?> "label in"

aTrackLabelOut :: NLineParser NTrackElem
aTrackLabelOut = (do
    void $ char ':'
    spaces
    t <- aLabel
    return $ NTrackLabelOut t) <?> "label out"

aLabel :: NLineParser NLabel
aLabel = do
    f <- lower
    r <- many letter
    return . NLabel . T.pack $ f : r

aType :: NLineParser NType
aType = NType <$> (do
    mf <- optionMaybe upper
    case mf of
        Just f -> do
            r <- many letter
            return $ Just $ T.pack (f:r)
        Nothing -> return Nothing
    ) <?> "type"

rails :: NLineParser ()
rails = void (many (char '=')) <?> "rails" 
