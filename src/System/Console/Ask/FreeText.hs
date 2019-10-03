-- | Free text input with or without defaults and validation.
module System.Console.Ask.FreeText (ask, askWith) where
import Data.Char (isSpace)
import System.IO (hFlush, stdout)

-- | Like 'askWith', but only checks that the answer is non-empty.
ask :: Maybe String -> String -> IO String
ask = askWith (not . all isSpace)

-- | Ask the given question with the given default answer.
--   The question is re-asked until the answer satisfies the given predicate.
--
--   If the answer is the empty string (i.e. the user just hits return)
--   and the empty string does not satisfy the given predicate,
--   the default value is returned if provided. If no default value is provided,
--   the question is re-asked.
askWith :: (String -> Bool) -> Maybe String -> String -> IO String
askWith valid mdef q = do
  case mdef of
    Just def -> putStr (q ++ " [" ++ def ++ "] ")
    _        -> putStr (q ++ " ")
  hFlush stdout
  ln <- getLine
  case ln of
    s | valid s   -> return s
      | null s    -> maybe (ask mdef q) pure mdef
      | otherwise -> ask mdef q
