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
  printBladeRF $ " Backend : " ++ backend info
  printBladeRF $ " Serial #: " ++ serial info
  printBladeRF $ " USB bus: " ++ (show (usbBus info))
  printBladeRF $ " USB address: " ++ (show (usbAddr info))
  printBladeRF $ " Instance: " ++ (show (inst info))
  foo <- bladeRFDeviceSpeed
  printBladeRF $ " Device Speed: " ++ (show foo)
