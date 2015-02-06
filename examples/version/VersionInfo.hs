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
  let version = show (major libVersion) ++ "." ++
                show (minor libVersion) ++ "." ++
                show (patch libVersion) ++ " (" ++
                     (descr libVersion) ++ ")"
  printBladeRF $ " bladeRF library version " ++ version
  printBladeRF $ " Firmware version: " ++ fwVersion
  printBladeRF $ " FPGA version: " ++ fpgaVersion
