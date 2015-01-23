module VersionInfo where

import LibBladeRF.Utils
import LibBladeRF.LibBladeRF

import Control.Monad.IO.Class

main :: IO ()
main  = withBladeRF $ do
--  libVersion <- bladeRFLibVersion
  fwVersion <- bladeRFFwVersion
--  fpgaVersion <- bladeRFFPGAVersion dev
--  putStrLn $ " libbladeRF version: " ++ libVersion
  liftIO (putStrLn $ " Firmware version: " ++ fwVersion)
--  putStrLn $ " FPGA version: " ++ fpgaVersion
