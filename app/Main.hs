module Main where

import Prelude hiding (Functor)
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Control.Monad.State.Strict hiding (Functor)
import SDParser
import SDNamespace

main :: IO ()
main = do contents <- get_total_cont
          let parse_result = parse sd_parser "" contents
          case parse_result of
               Left pe -> hPutStrLn stderr $ show pe
               Right sdcs -> sequence_ $ evalState (mapM handle_sdc sdcs) empty_sdns

get_total_cont :: IO (String)
get_total_cont = do args <- getArgs
                    contents <- mapM readFile args
                    return $ concat $ contents
