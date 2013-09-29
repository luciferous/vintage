module Main where

import GHC.Conc (getNumProcessors, setNumCapabilities)
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

import Syntax (thrift)
import Generator (generate)

usage :: String
usage = "Usage: hsift <file>"

compile :: FilePath -> IO ()
compile file = do
    result <- parseFromFile thrift file
    case result of
        Left err -> putStrLn $ show err
        Right ptype -> putStrLn $ prettyPrint $ generate ptype

main :: IO ()
main = do setNumCapabilities =<< getNumProcessors
          args <- getArgs
          case args of
              []      -> putStrLn usage
              file:_  -> compile file
