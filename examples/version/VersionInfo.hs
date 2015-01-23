module VersionInfo where

import LibBladeRF.Utils
import LibBladeRF.LibBladeRF

import Control.Monad.IO.Class

--main :: IO ()
main  = withBladeRF $ do
  libVersion <- liftIO $ bladeRFLibVersion
  fwVersion <- bladeRFFwVersion
  fpgaVersion <- bladeRFFPGAVersion
  liftIO . putStrLn $ " libbladeRF version: " ++ libVersion
  liftIO . putStrLn $ " Firmware version: " ++ fwVersion
  liftIO . putStrLn $ " FPGA version: " ++ fpgaVersion
