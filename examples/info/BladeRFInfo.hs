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

main  = withBladeRF $ do
  (backend, serial, usb_bus, usb_addr, inst) <- bladeRFGetDevInfo
  printBladeRF $ " Backend : " ++ backend
  printBladeRF $ " Serial #: " ++ serial
  printBladeRF $ " USB bus: " ++ (show usb_bus)
  printBladeRF $ " USB address: " ++ (show usb_addr)
  printBladeRF $ " Instance: " ++ (show inst)
  foo <- bladeRFDeviceSpeed
  printBladeRF $ " Device Speed: " ++ (show foo)
