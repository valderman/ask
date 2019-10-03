Sometimes when writing a quick and dirty console program, you may want to
ask the user for a bit more structured input than just a plain `getLine`,
but reaching for [brick](https://hackage.haskell.org/package/brick) or
[ncurses](https://hackage.haskell.org/package/ncurses) is just overkill.

This package is a collection of convenience functions for such occasions.

Example usage:
```haskell
import Data.Char (toUpper, toLower)
import System.Console.Ask

main = do
  name <- ask (Just "John Doe") "What is your name?"
  name_confirmed <- confirm False ("Is your name *really* " ++ name ++ "?")
  if not name_confirmed
    then putStrLn "OK, let's start over then." >> main
    else greet name

greet name = do
  preferred_name <- chooseOne "Which do you prefer?"
    [ ("Uppercase", map toUpper name)
    , ("Lowercase", map toLower name)
    , ("Neither", name)
    ]
  putStrLn $ "Hello " ++ preferred_name ++ ", nice to meet you!"
```
