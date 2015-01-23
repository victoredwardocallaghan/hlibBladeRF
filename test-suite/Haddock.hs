{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  test-suite/Haddock.hs
-}

module Main (main) where

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.Regex (matchRegex, mkRegex)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "cabal" ["haddock"] ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
