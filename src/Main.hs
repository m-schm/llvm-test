module Main where
import Relude
import qualified Parse
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text.IO as T
import qualified Check
import qualified Returns

main :: IO ()
main = do
  p:_ <- getArgs
  src <- T.readFile p
  case parse Parse.toplevel p src of
    Left peb -> putStrLn $ errorBundlePretty peb
    Right ast -> case Check.check ast of
      Left err -> putStrLn $ Check.errorPretty err
      Right ast' -> case Returns.toplevel ast' of
        Left n -> T.putStrLn $ "Not all code paths return: " <> n
        Right ast'' -> traverse_ print ast''
