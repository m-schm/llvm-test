module Main where
import Relude
import Parse (toplevel)
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text.IO as T
import Check

main :: IO ()
main = do
  p:_ <- getArgs
  src <- T.readFile p
  case parse toplevel p src of
    Left peb -> putStrLn $ errorBundlePretty peb
    Right ast -> case check ast of
      Left err -> putStrLn $ errorPretty err
      Right ast' -> traverse_ print ast'
