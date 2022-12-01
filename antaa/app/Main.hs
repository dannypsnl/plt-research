module Main (main) where

import Antaa.Core
import Antaa.Parser
import Antaa.Term
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  [filepath] <- getArgs
  content <- readFile filepath
  case parse pSrc filepath content of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right (RSrcPos pos tm) -> do
      (tm', vty) <- infer (emptyCtx pos) tm
      putStrLn "type checked:\n"
      putStrLn $ showTm0 tm'
      putStrLn "\ntype is:\n"
      putStrLn $ showVal (emptyCtx pos) vty
    _ -> error "bad"
