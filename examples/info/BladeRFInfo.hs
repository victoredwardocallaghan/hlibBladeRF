module BladeRFInfo where

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
