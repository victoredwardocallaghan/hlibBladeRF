{-# LANGUAGE ForeignFunctionInterface #-}

module BladeRFInfo where

import Foreign
import Foreign.C.Types

-- struct bladerf *dev;
-- struct bladerf_devinfo info;

main :: IO ()
main  = do
  status = bladerf_get_devinfo dev &info
  putStrLn " Serial #: " + info.serial
  putStrLn " USB bus: " + info.usb_bus);
  putStrLn " USB address: " + info.usb_addr
  putStrLn " Instance: " + info.instance
