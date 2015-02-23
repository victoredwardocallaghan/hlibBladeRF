{-# LANGUAGE ForeignFunctionInterface #-}

module BladeRFInfo where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Monad.IO.Class

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Types

main  = withBladeRF $ \dev -> do
  info <- bladeRFGetDevInfo dev
  putStrLn $ show info
  foo <- bladeRFDeviceSpeed dev
  putStrLn $ " Device Speed: " ++ (show foo)
  serial <- bladeRFGetSerial dev
  putStrLn $ " Device Serial: " ++ serial
  size <- bladeRFGetFPGASize dev
  putStrLn $ " FPGA Size = " ++ (show size)
