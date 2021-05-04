module Main where

import Prelude hiding (Functor)
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment (getArgs, getProgName)
import Control.Monad.State.Strict hiding (Functor)
import SDParser
import SDNamespace

import Paths_tikzsd (version)
import Data.Version (showVersion)

main :: IO ()
main = do args <- getArgs
          case args of
               ["--help"]    -> usage
               ["--version"] -> putStrLn ("tikzsd " ++ showVersion version)
               _             -> launch args

launch :: [String] -> IO ()
launch args = do contents <- get_total_cont args
                 let parse_result = parse sd_parser "" contents
                 case parse_result of
                      Left pe -> hPutStrLn stderr $ show pe
                      Right sdcs -> sequence_ $ evalState (mapM handle_sdc sdcs) empty_sdns

get_total_cont :: [String] -> IO (String)
get_total_cont args = do contents <- mapM readFile args
                         return $ concat $ contents

usage :: IO ()
usage = do
    self <- getProgName
    putStr $ unlines $ 
        concat ["Usage: ", self, " [Option | source-files]"] :
        "":
        "Available options" :
        "   --help           Print this help message." : 
        "   --version        Print the version number." :
        "":
        "The most common usage is " :
        "   tikzsd source-file":
        "or":
        "   tikzsd source-file-1 source-file-2 ... source-file-n":
        "-------------------------------------------------------":
        []
