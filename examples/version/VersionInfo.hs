module VersionInfo where

import Control.Monad.IO.Class

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Misc
import LibBladeRF.Types

main  = withBladeRF $ do
  libVersion <- liftIO $ bladeRFLibVersion
  fwVersion <- bladeRFFwVersion
  fpgaVersion <- bladeRFFPGAVersion
  printBladeRF $ " libbladeRF version: " ++ (show libVersion)
  printBladeRF $ " Firmware version: " ++ (show fwVersion)
  printBladeRF $ " FPGA version: " ++ (show fpgaVersion)
