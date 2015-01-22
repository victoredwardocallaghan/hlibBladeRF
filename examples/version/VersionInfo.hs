module VersionInfo where

import LibBladeRF.Utils
import LibBladeRF.LibBladeRF

main :: IO ()
main  = withBladeRF $ \dev -> do
  fwVersion <- bladeRFFwVersion dev
  libVersion <- bladeRFLibVersion
  fpgaVersion <- bladeRFFPGAVersion dev
  putStrLn $ " libbladeRF version: " ++ libVersion
  putStrLn $ " Firmware version: " ++ fwVersion
  putStrLn $ " FPGA version: " ++ fpgaVersion
