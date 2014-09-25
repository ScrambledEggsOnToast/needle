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

data SwitchDir = SUp | SDown deriving (Show, Eq)

data NTrackElem = NTrackIn Int NType
                | NTrackOut
                | NTrackExtArrow NExtArrow
                | NTrackLabelIn NLabel
                | NTrackLabelOut NLabel
                | NTrackSwitch SwitchDir
                | NTrackTunnelEntrance
                | NTrackTunnelExit
                | NTrackSplit
    deriving (Show, Eq)

type Columned a = (a, (Column, Column))

newtype NTrack = NTrack [Columned NTrackElem] deriving (Show, Eq)

newtype NLine = NLine [NTrack] deriving (Show, Eq)

type NLines = [NLine]

data NGArrow = NGIn Int NType | NGJoin [NGArrow] | NGThrough NExtArrow NGArrow deriving Show

parseNeedle :: String -> Either ParseError NGArrow
parseNeedle = fmap linesToNGArrow . parseLines

linesToNGArrow :: NLines -> NGArrow
linesToNGArrow ls = case ops of
    [] -> error "no outputs"
    [op] -> (uncurry $ ngArrowUpToPosition ls) op
    _ -> NGJoin . map (uncurry $ ngArrowUpToPosition ls) $ ops
  where
    trackEnds = zip [0..] ls >>= (\(r, NLine ts) -> [(r, last t) | NTrack t@(e:_) <- ts])
    ops = map (\(r,(_,(c,_))) -> (r,c-1)) . filter ((== NTrackOut) . fst . snd) $ trackEnds

ngArrowUpToPosition :: NLines -> Line -> Column -> NGArrow
ngArrowUpToPosition ls r c = process lbs
  where
    process lbs' = case lbs' of
        [(_, (NTrackIn n t, _))] -> NGIn n t
        [(r', (NTrackExtArrow a, (c',_)))] -> NGThrough a (ngArrowUpToPosition ls r' (c'-1))
        lbs'' -> NGJoin . map (process . return) $ lbs''
    lbs = lookBack ls r c

(!) :: [a] -> Int -> Maybe a
(!) [] _ = Nothing
(!) (a:as) 0 = Just a
(!) (_:as) n = as ! (n-1)

trRanges :: NLine -> Maybe [(Column,Column)]
trRanges (NLine ts) = mapM (\(NTrack es) -> (,) <$> (listToMaybe . map (fst . snd) $ es) <*>  (listToMaybe . reverse . map (snd . snd) $ es)) ts

nLookup :: NLines -> Line -> Column -> Maybe (Either (Columned NTrackElem) (Columned NTrackElem, Columned NTrackElem))
nLookup ls r c = do
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

nLookupBefore :: NLines -> Line -> Column -> Maybe (Columned NTrackElem)
nLookupBefore ls r c = do
    lu <- nLookup ls r c
    return $ case lu of
            Left e -> e
            Right (e,_) -> e

lookBack :: NLines -> Line -> Column -> [(Line, (NTrackElem, (Column, Column)))]
lookBack ls r c = do
    ce@(e, (c1,c2)) <- maybeToList $ nLookupBefore ls r c

    let rE = return (r, (e,(c1,c2)))

    case e of
            NTrackIn _ _ -> rE
            NTrackExtArrow _ -> rE
            NTrackLabelIn label -> do
                let (r',(_,(c',_))) = case (findLabelOut ls label) of
                        Just x -> x
                        Nothing -> error $ "no label out found for: "++ show label
                lookBack ls r' (c' - 1)
            NTrackSwitch dir -> do
                let d = case dir of
                        SUp -> 1
                        SDown -> -1
                let go (r', ce') = case (nLookup ls (r'+d) c1) of
                        Just (Left n@(NTrackSwitch dir, _)) -> go (r'+d, n)
                        Just (Left (NTrackSplit,_)) -> do
                            ce'' <- maybeToList $ nLookupBefore ls (r'+d) (c1-1)
                            return (r'+d, ce'')
                        _ -> []
                let l = lookBack ls r (c - 1)
                case dir of
                    SUp -> l ++ go (r,ce)
                    SDown -> go (r,ce) ++ l
            NTrackSplit -> lookBack ls r (c-1)
            NTrackTunnelExit -> do
                let go 0 c = lookBack ls r (c-1)
                    go n' c' = case (nLookupBefore ls r (c'-1)) of
                        Just (NTrackTunnelEntrance,_) -> go (n'-1) (c'-1)
                        Just (NTrackTunnelExit,_) -> go (n'+1) (c'-1)
                        _ -> go n' (c'-1)
                go 1 c1
            _ -> error ("This shouldn't be here: " ++ show e)

findLabelOut :: NLines -> NLabel -> Maybe (Line, Columned NTrackElem)
findLabelOut ls label = do
    let go _ [] = Nothing
        go r ((NLine ts):ls') = do
            let es = concatMap (\(NTrack x) -> x) ts
            case (filter ((== NTrackLabelOut label) . fst) es) of
                (e:_) -> Just (r, e)
                _ -> go (r+1) ls'
    go 0 ls

type NLineParser = Parsec String Int

parseLines :: String -> Either ParseError [NLine]
parseLines = runParser (do
    ls <- aLine `sepBy` many1 eol
    eof
    return ls) 0 ""

aLine :: NLineParser NLine
aLine = do
    ms <- many (spaces >> aTrack) 
    return $ NLine (ms)

aTrack :: NLineParser NTrack
aTrack = try (do
    firstElem <- columned $ aTrackIn 
                        <|> aTrackSwitcher 
                        <|> aTrackExtArrow 
                        <|> aTrackTunnelExit 
                        <|> aTrackLabelIn

    let go bs = (rails >>) $ (do
            b <- columned $ aTrackSwitcher
                        <|> aTrackSplit
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
    return $ NTrackSwitch SUp) <?> "track switcher"

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

aTrackSplit :: NLineParser NTrackElem
aTrackSplit = (do
    void $ char 'o'
    return NTrackSplit) <?> "track split"

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
