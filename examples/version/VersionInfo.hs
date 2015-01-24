module VersionInfo where

import Control.Monad.IO.Class

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Misc

main  = withBladeRF $ do
  libVersion <- liftIO $ bladeRFLibVersion
  fwVersion <- bladeRFFwVersion
  fpgaVersion <- bladeRFFPGAVersion
  printBladeRF $ " libbladeRF version: " ++ libVersion
  printBladeRF $ " Firmware version: " ++ fwVersion
  printBladeRF $ " FPGA version: " ++ fpgaVersion
