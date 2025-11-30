module Utils.Regex
  ( checkIfMatchT,
    getFirstMatchT,
    getAllMatchesT,
    checkIfMatch,
    getFirstMatch,
    getAllMatches,
  )
where

import Data.Text (Text)
import Text.Regex.TDFA hiding (getAllMatches)

type MyRegexT = Text

type MyRegex = String

checkIfMatchT :: Text -> MyRegexT -> Bool
checkIfMatchT = (=~)

getFirstMatchT :: Text -> MyRegexT -> Text
getFirstMatchT = (=~)

getAllMatchesT :: Text -> MyRegexT -> [Text]
getAllMatchesT txt regex = getAllTextMatches $ txt =~ regex

checkIfMatch :: String -> MyRegex -> Bool
checkIfMatch = (=~)

getFirstMatch :: String -> MyRegex -> String
getFirstMatch = (=~)

getAllMatches :: String -> MyRegex -> [String]
getAllMatches str regex = getAllTextMatches $ str =~ regex
