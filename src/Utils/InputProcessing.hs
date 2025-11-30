module Utils.InputProcessing
  ( readInputT,
    readInputLinesT,
    breakLineT,
    parseIntT,
    readIntsT,
    readIntsByT,
    readInputLines,
    breakLine,
    parseInt,
    readInts,
    readIntsBy,
  )
where

import Data.List.Split (split, splitOn)
import Data.String (lines)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Numeric (readDec)

-- | Read whole file as @Text@.
readInputT :: FilePath -> IO Text
readInputT = TIO.readFile

-- | Read lines as @Text@ from file.
readInputLinesT :: FilePath -> IO [Text]
readInputLinesT = fmap T.lines . readInputT

-- | Break line seperated by @delimiter@ in two.
breakLineT :: Char -> Text -> (Text, Text)
breakLineT delimiter t = case T.break (== delimiter) t of
  pair@(left, right)
    | left == T.empty || right == T.empty ->
        error $
          "Expected \""
            ++ T.unpack t
            ++ "\" to be splitable by delimiter '"
            ++ [delimiter]
            ++ "' in splitLineT"
    | otherwise -> pair

-- | Parse a @Text@ to an @Int@.
parseIntT :: Text -> Int
parseIntT txt =
  case T.uncons txt of
    Nothing -> error "Tried to parse empty string to Int in parseIntT"
    Just ('-', txt') -> negate $ parseIntT' txt'
    _ -> parseIntT' txt
  where
    parseIntT' txt' =
      case TR.decimal txt' of
        Right (n, rest) | T.null rest -> n
        _ -> error $ "Invalid integer: " ++ T.unpack txt'

-- | Convert words in a @Text@ to @Int@s.
readIntsT :: Text -> [Int]
readIntsT = map parseIntT . T.words

-- | Seperate chunks in @Text@ by @delimiter@ and convert
-- these to @Int@s.
readIntsByT :: Char -> Text -> [Int]
readIntsByT delimiter = map parseIntT . T.split (== delimiter)

-- | Read lines from file.
readInputLines :: FilePath -> IO [String]
readInputLines = fmap lines . readFile

-- | Break line seperated by @delimiter@ in two.
breakLine :: Char -> String -> (String, String)
breakLine delimiter s = case break (== delimiter) s of
  pair@(left, right)
    | null left || null right ->
        error $
          "Expected \""
            ++ s
            ++ "\" to be splitable by delimiter '"
            ++ [delimiter]
            ++ "' in splitLine"
    | otherwise -> pair

parseIntSafe :: String -> Either String Int
parseIntSafe [] = Left "Empty string"
parseIntSafe ('-' : rest) = negate <$> parseIntSafe rest
parseIntSafe s = case readDec s of
  [(n, "")] -> Right n
  _ -> Left $ "Invalid integer: " ++ s

-- Parse a @String@ to an @Int@.
parseInt :: String -> Int
parseInt = either error id . parseIntSafe

-- | Convert words in @String@ to @Int@s.
readInts :: String -> [Int]
readInts = map parseInt . words

-- | Seperate @s@ by @delimiter@, then convert the resulting
-- strings to @Int@s.
readIntsBy :: String -> String -> [Int]
readIntsBy delimiter = map parseInt . splitOn delimiter
