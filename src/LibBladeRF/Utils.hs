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
