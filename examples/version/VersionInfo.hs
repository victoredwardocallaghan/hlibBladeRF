{-# LANGUAGE ForeignFunctionInterface #-}

module VersionInfo where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF

bladerfVersion :: IO String
bladerfVersion = do
  p <- malloc :: IO (Ptr C'bladerf_version)
  c'bladerf_version p
  brfv <- peek p
  desc <- peekCString $ c'bladerf_version'describe brfv
  free p
  return desc

main :: IO ()
main  = do
  version <- bladerfVersion
  putStrLn $ " libbladeRF version: " ++ version
