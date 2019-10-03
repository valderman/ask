-- | Single-choice menu.
module System.Console.Ask.Menu
  ( MenuStyle, MenuLayout (..)
  , defaultMenuStyle, showAlt, menuLayout, showSelected, linePrefix
  , chooseOneWith, chooseOne, chooseOneH, chooseOneV
  ) where
import Control.Monad
import Control.Exception
import Data.List
import System.Console.ANSI
import System.IO

data MenuKey = OK | Up | Down
  deriving (Show, Read, Eq)

data MenuLayout = Vertical | Horizontal | SameLine
  deriving (Show, Read, Eq)

data MenuStyle = MenuStyle
  { -- | Rendering of a single menu alternative.
    showAlt :: Bool -> String -> String
    -- | General layout of the menu.
  , menuLayout :: MenuLayout
    -- | Should the selected answer be shown next to the question post-selection?
  , showSelected :: Bool
    -- | Prefix (for instance, indentation) for each line of the menu.
  , linePrefix :: String
  }

verticalMenu :: MenuStyle -> Bool
verticalMenu style = menuLayout style == Vertical

sameLineMenu :: MenuStyle -> Bool
sameLineMenu style = menuLayout style == SameLine

defaultMenuStyle :: MenuStyle
defaultMenuStyle = MenuStyle
  { showAlt = \selected alt ->
      if selected
        then ("[" ++ alt ++ "]")
        else (" " ++ alt ++ " ")
  , linePrefix = " "
  , menuLayout = SameLine
  , showSelected = True
  }

resetTerm :: MenuStyle -> [String] -> IO ()
resetTerm style alts = do
  if verticalMenu style
    then cursorUpLine (length alts-1)
    else putStr "\r"

formatSameLineQuestion :: String -> String
formatSameLineQuestion q = q ++ " "

showAlts :: MenuStyle -> String -> Int -> [String] -> String
showAlts style q sel alts =
    prefix ++ intercalate separator (zipWith (drawAlt sel) alts [0..])
  where
    prefix
      | menuLayout style == SameLine = formatSameLineQuestion q
      | otherwise                    = linePrefix style
    separator
      | verticalMenu style = '\n' : linePrefix style
      | otherwise          = ""
    drawAlt sel alt i = showAlt style (i == sel) alt

renderAlts :: MenuStyle -> String -> Int -> [String] -> IO ()
renderAlts style q sel alts = do
  putStr (showAlts style q sel alts)
  hFlush stdout
  resetTerm style alts

clearAlts :: MenuStyle -> String -> [String] -> IO ()
clearAlts style q alts = do
    putStr eraser
    hFlush stdout
    resetTerm style alts
  where
    eraser = map mkSpace (showAlts style q 0 alts)
    mkSpace '\n' = '\n'
    mkSpace _    = ' '

getMenuKey :: Bool -> IO MenuKey
getMenuKey vertical = do
    c <- getChar
    result <- case c of
      '\n'   -> return OK
      ' '    -> return OK
      '\ESC' -> handleSpecial
      _      -> getMenuKey vertical
    return result
  where
    handleSpecial = do
      getChar
      c <- getChar
      case c of
        'A' | vertical     -> return Up
        'B' | vertical     -> return Down
        'C' | not vertical -> return Down
        'D' | not vertical -> return Up
        _                  -> getMenuKey vertical

displayAnswerIfAllowed :: MenuStyle -> String -> String -> IO ()
displayAnswerIfAllowed style q result = do
  when (not (sameLineMenu style)) $ cursorUpLine 1
  putStr (formatSameLineQuestion q)
  if (showSelected style)
    then putStrLn result
    else putStrLn ""

-- | Like 'chooseOneWith', but uses the default menu style.
chooseOne :: String -> [(String, a)] -> IO a
chooseOne = chooseOneWith defaultMenuStyle

-- | Like 'chooseOne', but uses a horizontal menu layout.
chooseOneH :: String -> [(String, a)] -> IO a
chooseOneH = chooseOneWith defaultMenuStyle { menuLayout = Horizontal }

-- | Like 'chooseOne', but uses a vertical menu layout.
chooseOneV :: String -> [(String, a)] -> IO a
chooseOneV = chooseOneWith defaultMenuStyle { menuLayout = Vertical }

-- | Prints the given question, then requires the user to select one of the
--   given options, using the arrow keys to choose an option and return/space
--   to confirm.
chooseOneWith :: MenuStyle -> String -> [(String, a)] -> IO a
chooseOneWith style q alts = do
    when (not (sameLineMenu style)) $ putStrLn q
    withUnbufferedTerm $ do
      result <- choose 0
      clearAlts style q as
      displayAnswerIfAllowed style q (as !! result)
      return (snd (alts !! result))
  where
    as = map fst alts
    withUnbufferedTerm m = do
      bm <- hGetBuffering stdin
      em <- hGetEcho stdout
      hideCursor
      hSetBuffering stdin NoBuffering
      hSetEcho stdout False
      m `finally` do
        hSetBuffering stdin bm
        hSetEcho stdout em
        showCursor
    num_alts = length as
    choose sel = do
      renderAlts style q sel as
      c <- getMenuKey (verticalMenu style)
      case c of
        OK   -> return sel
        Up   -> choose (max 0 (sel-1))
        Down -> choose (min (num_alts-1) (sel+1))
