{-# LANGUAGE ForeignFunctionInterface #-}

module LibBladeRF.Utils where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF

bladeRFLibVersion :: IO String
bladeRFLibVersion = do
  p <- malloc :: IO (Ptr C'bladerf_version)
  c'bladerf_version p
  brfv <- peek p
  desc <- peekCString $ c'bladerf_version'describe brfv
  free p
  return desc


bladeRFFwVersion :: Ptr C'bladerf -> IO String
bladeRFFwVersion dev = do
  p <- malloc :: IO (Ptr C'bladerf_version)
  c'bladerf_fw_version dev p
  brfv <- peek p
  desc <- peekCString $ c'bladerf_version'describe brfv
  free p
  return desc


bladeRFFPGAVersion :: Ptr C'bladerf -> IO String
bladeRFFPGAVersion dev = do
  status <- c'bladerf_is_fpga_configured dev
  if status > 0 then do
    p <- malloc :: IO (Ptr C'bladerf_version)
    c'bladerf_fpga_version dev p
    brfv <- peek p
    desc <- peekCString $ c'bladerf_version'describe brfv
    free p
    return desc
  else
    return "Unknown (FPGA not loaded)"
