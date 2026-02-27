module Main where

import System.Environment (getArgs)
import AcceptanceSuite (parseArgs, runMBTTAcceptance)

main :: IO ()
main = do
  cfg <- parseArgs <$> getArgs
  runMBTTAcceptance cfg
