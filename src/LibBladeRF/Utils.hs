{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates misc libbladeRF library functions.
-}

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
