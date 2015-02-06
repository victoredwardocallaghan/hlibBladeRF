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
  let lversion = show (major libVersion) ++ "." ++
                 show (minor libVersion) ++ "." ++
                 show (patch libVersion) ++ " (" ++
                      (descr libVersion) ++ ")"
  printBladeRF $ " bladeRF library version " ++ lversion
  printBladeRF $ " Firmware version: " ++ (show fwVersion)
  let fversion = show (major fwVersion) ++ "." ++
                 show (minor fwVersion) ++ "." ++
                 show (patch fwVersion) ++ " (" ++
                      (descr fwVersion) ++ ")"
  printBladeRF $ " bladeRF firmware version " ++ fversion
  printBladeRF $ " FPGA version: " ++ (show fpgaVersion)
