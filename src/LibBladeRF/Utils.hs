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

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF

bladeRFLibVersion :: IO String
bladeRFLibVersion = do
  p <- malloc :: IO (Ptr C'bladerf_version)
  c'bladerf_version p
  brfv <- peek p
  desc <- peekCString $ c'bladerf_version'describe brfv
  free p
  return desc


bladeRFFwVersion :: BladeRF String
bladeRFFwVersion = do
  p <- liftIO (malloc :: IO (Ptr C'bladerf_version))
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_fw_version dev p
  brfv <- liftIO $ peek p
  desc <- liftIO $ peekCString $ c'bladerf_version'describe brfv
  liftIO $ free p
  return desc


bladeRFFPGAVersion :: BladeRF String
bladeRFFPGAVersion  = do
  dev <- BladeRF $ lift get
  status <- liftIO $ c'bladerf_is_fpga_configured dev
  if status > 0 then do
    p <- liftIO (malloc :: IO (Ptr C'bladerf_version))
    liftIO $ c'bladerf_fpga_version dev p
    brfv <- liftIO $ peek p
    desc <- liftIO $ peekCString $ c'bladerf_version'describe brfv
    liftIO $ free p
    return desc
  else
    return "Unknown (FPGA not loaded)"
