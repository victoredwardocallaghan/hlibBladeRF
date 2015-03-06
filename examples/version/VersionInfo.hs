module VersionInfo where

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Types

main  = withBladeRF $ \dev -> do
  libVersion <- bladeRFLibVersion
  fwVersion <- bladeRFFwVersion dev
  fpgaVersion <- bladeRFFPGAVersion dev
  putStrLn $ " libbladeRF version: " ++ (show libVersion)
  putStrLn $ " Firmware version: " ++ (show fwVersion)
  putStrLn $ " FPGA version: " ++ (show fpgaVersion)
