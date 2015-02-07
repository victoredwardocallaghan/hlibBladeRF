{-# LANGUAGE ForeignFunctionInterface #-}

module BladeRFInfo where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Monad.IO.Class

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Misc
import LibBladeRF.Types

main  = withBladeRF $ do
  info <- bladeRFGetDevInfo
  printBladeRF $ show info
  foo <- bladeRFDeviceSpeed
  printBladeRF $ " Device Speed: " ++ (show foo)
  serial <- bladeRFGetSerial
  printBladeRF $ " Device Serial: " ++ serial
  size <- bladeRFGetFPGASize
  printBladeRF $ " FPGA Size = " ++ (show size)
