{-# LANGUAGE OverloadedStrings #-}
module Saline.Test.Prometheus
  ( Counter(..), Gauge(..), Histo(..), Sample(..), Summary, Samples, parsePrometheus
  ) where

import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Functor (($>), void)
import Data.Text (Text)

import qualified Data.Map as Map

data SampleT = CounterT | GaugeT | HistogramT | SummaryT
  deriving stock (Eq, Show)

type K = Text
type V = Text

data Counter = CounterSimple Double | CounterMany (Map.Map [(K, V)] Double)
  deriving stock (Eq, Show)

data Gauge   = Gauge
  { gaugeKVs :: [(K, V)]
  , gaugeval :: Double
  } deriving stock (Eq, Show)

data Histo   = Histo
  { hBuckets :: Map.Map Text Double
  , hSum     :: Double
  , hCount   :: Word
  } deriving stock (Eq, Show)
type Summary = Histo

data Sample = SampleC Counter | SampleG Gauge | SampleH Histo | SampleS Summary
  deriving stock (Eq, Show)

parseType :: Parser SampleT
parseType = choice [ string "counter"   $> CounterT
                   , string "gauge"     $> GaugeT
                   , string "histogram" $> HistogramT
                   , string "summary"   $> SummaryT
                   ]

parseNameTypeLine :: Parser (Text, SampleT)
parseNameTypeLine = do
  string "# TYPE" >> skipSpace
  name <- takeTill isSpace
  skipSpace
  t <- parseType
  endOfLine
  return (name, t)

keyval :: Parser (K, V)
keyval = do
  k <- takeTill (=='=')
  void $ string "=\""
  v <- takeTill (=='"')
  void $ string "\""
  return (k, v)

keyvals :: Parser [(K, V)]
keyvals = sepBy1 keyval (char ',')

parseSample :: SampleT -> Text -> Parser Sample
parseSample CounterT name =
  choice [ string name *> skipSpace *> fmap (SampleC . CounterSimple) double <* endOfLine
         , SampleC . CounterMany . Map.fromList <$> many1 counter
         ]

  where counter = do
          void $ string name
          void $ char '{'
          kvs <- keyvals
          void $ char '}'
          skipSpace
          c <- double
          endOfLine
          return (kvs, c)

parseSample GaugeT name = do
  choice [ string name *> char ' ' *> fmap (SampleG . Gauge []) double <* endOfLine
         , string name *> fmap SampleG kvgauge
         ]

  where kvgauge = do
          void $ char '{'
          kvs <- keyvals
          void $ char '}'
          skipSpace
          g <- double
          endOfLine
          return $ Gauge kvs g

parseSample HistogramT name = do
  buckets <- many1 parseBucket
  sumv <- parseSumLine
  countv <- parseCountLine
  return $ SampleH $
    Histo (Map.fromList buckets) sumv countv

  where parseBucket = do
          void $ string name
          void $ string "_bucket{"
          skipWhile (/='=')
          void $ char '='
          skipWhile (/=',')
          void $ string "le"
          void $ string "=\""
          up <- takeWhile1 (/='"')
          void $ string "\"}"
          skipSpace
          v <- double
          endOfLine
          return (up, v)
        parseSumLine = do
          void $ string name
          void $ string "_sum{"
          skipWhile (/='=')
          void $ char '='
          skipWhile (/='}')
          skipSpace
          v <- double
          endOfLine
          return v
        parseCountLine = do
          void $ string name
          void $ string "_count{"
          skipWhile (/='=')
          void $ char '='
          skipWhile (/='}')
          skipSpace
          v <- decimal
          endOfLine
          return v

parseSample SummaryT name = do
  quantiles <- many1 parseQuantile
  sumv <- parseSumLine
  countv <- parseCountLine
  return $ SampleS $
    Histo (Map.fromList quantiles) sumv countv

  where parseQuantile = do
          void $ string name
          void $ string "{quantile=\""
          q <- takeWhile1 (/='"')
          void $ string "\"} "
          v <- double
          endOfLine
          return (q, v)
        parseSumLine = do
          void $ string name
          void $ string "_sum "
          v <- double
          endOfLine
          return v
        parseCountLine = do
          void $ string name
          void $ string "_count "
          v <- decimal
          endOfLine
          return v

type Samples = Map.Map Text Sample

parseSamples :: Parser Samples
parseSamples = fmap Map.fromList . many1 $ do
  void skipHelp
  (n, t) <- parseNameTypeLine
  (n,) <$> parseSample t n

  where skipHelp = string "# HELP" >> skipWhile (not . isEndOfLine) >> endOfLine

parsePrometheus :: Text -> Either String Samples
parsePrometheus = parseOnly (parseSamples <* endOfInput)
