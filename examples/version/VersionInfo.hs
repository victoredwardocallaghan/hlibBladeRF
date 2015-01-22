module VersionInfo where

import LibBladeRF.Utils

main :: IO ()
main  = do
  version <- bladerfVersion
  putStrLn $ " libbladeRF version: " ++ version
