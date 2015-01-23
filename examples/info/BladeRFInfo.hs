{-# LANGUAGE ForeignFunctionInterface #-}

module BladeRFInfo where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Monad.IO.Class

import LibBladeRF.LibBladeRF

-- main :: IO ()
main  = withBladeRF $ do
  (backend, serial, usb_bus, usb_addr, inst) <- bladerfGetDevInfo
  liftIO . putStrLn $ " Backend : " ++ backend
  liftIO . putStrLn $ " Serial #: " ++ serial
  liftIO . putStrLn $ " USB bus: " ++ (show usb_bus)
  liftIO . putStrLn $ " USB address: " ++ (show usb_addr)
  liftIO . putStrLn $ " Instance: " ++ (show inst)
