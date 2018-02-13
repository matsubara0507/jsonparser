module Main where

import           JSON

import           Data.Either
import           System.Environment (getArgs)
import           Text.Megaparsec    (parse, parseErrorPretty)

main :: IO ()
main = do
  [filename] <- getArgs
  jsonstring <- readFile filename
  case (parse jsonParser "" jsonstring) of
    Left err   -> print err
    Right json -> print json
