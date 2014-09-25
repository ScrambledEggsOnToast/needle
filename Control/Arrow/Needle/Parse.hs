{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Control.Arrow.Needle.Parse where

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import Data.Maybe
import Data.Monoid

import Text.Parsec
import Text.Parsec.Extra
import Data.Char

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State
import Control.Arrow

import Language.Haskell.TH
import Language.Haskell.TH.Quote

needle :: QuasiQuoter
needle = QuasiQuoter 
    { quoteExp = \str -> case (parseNeedle str) of
        Left e -> error . show $ e
        Right n -> buildArrow n
    }

buildArrow :: (NGArrow, Int) -> Q Exp
buildArrow (a, nIns) = do
    ins <- mapM (newName . T.unpack . inNm) [1..nIns]
    let f (NGIn n) = return 
            $ AppE (VarE $ mkName "Control.Arrow.arr") 
            $ LamE [TupP $ map VarP ins] (VarE (ins !! (n-1)))
        f (NGJoin a1 a2) = do
            e1 <- f a1
            e2 <- f a2
            return $ InfixE (Just e1) (VarE $ mkName "Control.Arrow.&&&") (Just e2)
        f (NGThrough (NExtArrow t) a) = do
            let ea = VarE . mkName . T.unpack $ t
            b <- f a
            return $ InfixE (Just b) (VarE $ mkName "Control.Arrow.>>>") (Just ea)
    f a

newtype NExtArrow = NExtArrow T.Text deriving (Show, Eq)
newtype NLabel = NLabel T.Text deriving (Show, Eq)

data SwitchDir = SUp | SDown deriving (Show, Eq)

data NTrackElem = NTrackIn Int
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

functionify :: (NGArrow, Int) -> T.Text
functionify (a, nIns) = "(" <> inStr <> f a <> ")"
  where
    inStr = "\\(" <> T.intercalate ", " (map inNm [1..nIns]) <> ") ->"
    f (NGIn n) = inNm n
    f (NGJoin a1 a2) = "(" <> f a1 <> ", " <> f a2 <> ")"
    f (NGThrough (NExtArrow t) a') = t <> " (" <> f a' <> ")"

inNm :: Int -> T.Text
inNm n = "__" <> T.pack (show n)

numIns :: NGArrow -> Int
numIns (NGIn n) = n
numIns (NGJoin a1 a2) = max (numIns a1) (numIns a2)
numIns (NGThrough _ a) = numIns a

data NGArrow = NGIn Int | NGJoin NGArrow NGArrow | NGThrough NExtArrow NGArrow deriving Show

parseNeedle :: String -> Either ParseError (NGArrow, Int)
parseNeedle = fmap (first linesToNGArrow) . parseLines

joinList :: [NGArrow] -> NGArrow
joinList [] = error "no joinable things"
joinList [a] = a
joinList (a:as) = NGJoin a (joinList as)

linesToNGArrow :: NLines -> NGArrow
linesToNGArrow ls = joinList . map (uncurry $ ngArrowUpToPosition ls) $ ops
  where
    trackEnds = zip [0..] ls >>= (\(r, NLine ts) -> [(r, last t) | NTrack t@(e:_) <- ts])
    ops = map (\(r,(_,(c,_))) -> (r,c-1)) . filter ((== NTrackOut) . fst . snd) $ trackEnds

ngArrowUpToPosition :: NLines -> Line -> Column -> NGArrow
ngArrowUpToPosition ls r c = process lbs
  where
    process lbs' = case lbs' of
        [(_, (NTrackIn n, _))] -> NGIn n
        [(r', (NTrackExtArrow a, (c',_)))] -> NGThrough a (ngArrowUpToPosition ls r' (c'-1))
        lbs'' -> joinList . map (process . return) $ lbs''
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
            NTrackIn _ -> rE
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
                let l = lookBack ls r (c1 - 1)
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

parseLines :: String -> Either ParseError (NLines, Int)
parseLines = runParser (do
    ls <- many aLine
    eof
    n <- getState
    return (ls, n)) 0 "" . (++ "\n")

sameLineSpaces :: NLineParser ()
sameLineSpaces = skipMany . satisfy $ \c -> not (c `elem` "\r\n") && isSpace c

aLine :: NLineParser NLine
aLine = do
    sameLineSpaces
    ms <- manyTill (sameLineSpaces >> aTrack) (sameLineSpaces >> eol)
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
    void $ char '}'
    modifyState (+1)
    n <- getState
    return $ NTrackIn n) <?> "arrow input"

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

rails :: NLineParser ()
rails = void (many (char '=')) <?> "rails" 
