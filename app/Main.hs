module Main where

import           JSON
import           System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  jsonstring <- readFile filename
  case parseJson jsonstring of
    Left err   -> print err
    Right json -> print json
