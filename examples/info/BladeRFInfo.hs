{-# LANGUAGE ForeignFunctionInterface #-}

module BladeRFInfo where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import LibBladeRF.LibBladeRF


main :: IO ()
main  = withBladeRF $ \dev -> do
  (backend, serial, usb_bus, usb_addr, inst) <- bladerfGetDevInfo dev
  putStrLn $ " Backend : " ++ backend
  putStrLn $ " Serial #: " ++ serial
  putStrLn $ " USB bus: " ++ (show usb_bus)
  putStrLn $ " USB address: " ++ (show usb_addr)
  putStrLn $ " Instance: " ++ (show inst)
