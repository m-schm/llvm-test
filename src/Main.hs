module Main where
import Relude
import Parse
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  p:_ <- getArgs
  src <- T.readFile p
  either (putStrLn . errorBundlePretty) (traverse_ print) $ parse toplevel p src
