module System.Console.Ask.YesNo where
import Data.Char (toLower)
import Data.Maybe (fromJust, isJust)
import System.Console.Ask.FreeText

confirm :: Bool -> String -> IO Bool
confirm def = fmap (fromJust . parse) . askWith canParse (Just def_str)
  where
    def_str
      | def       = "yes"
      | otherwise = "no"
    yes = ["yes", "y"]
    no = ["no", "n"]
    parse s =
      case map toLower s of
        ""                 -> Just def
        s' | s' `elem` yes -> Just True
           | s' `elem` no  -> Just False
           | otherwise     -> Nothing
    canParse = isJust . parse
