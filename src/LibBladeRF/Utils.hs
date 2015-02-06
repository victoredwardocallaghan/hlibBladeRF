{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates misc libbladeRF library functions.
-}

module LibBladeRF.Utils ( bladeRFLibVersion
                        , bladeRFFwVersion
                        , bladeRFFPGAVersion
                        , bladeRFDeviceSpeed
                        , bladeRFGetDevInfo
                        ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types

--
-- | Get libbladeRF version information
bladeRFLibVersion :: IO BladeRFVersion
bladeRFLibVersion = do
  p <- malloc :: IO (Ptr C'bladerf_version)
  c'bladerf_version p
  brfv <- peek p
  desc <- peekCString $ c'bladerf_version'describe brfv
  let ver = BladeRFVersion { major = c'bladerf_version'major brfv
                           , minor = c'bladerf_version'minor brfv
                           , patch = c'bladerf_version'patch brfv
                           , descr = desc
                           }
  free p
  return ver

--
-- | Query firmware version
bladeRFFwVersion :: BladeRF BladeRFVersion
bladeRFFwVersion = do
  p <- liftIO (malloc :: IO (Ptr C'bladerf_version))
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_fw_version dev p
  brfv <- liftIO $ peek p
  desc <- liftIO $ peekCString $ c'bladerf_version'describe brfv
  let ver = BladeRFVersion { major = c'bladerf_version'major brfv
                           , minor = c'bladerf_version'minor brfv
                           , patch = c'bladerf_version'patch brfv
                           , descr = desc
                           }
  liftIO $ free p
  return ver

--
-- | Query FPGA version
bladeRFFPGAVersion :: BladeRF BladeRFVersion
bladeRFFPGAVersion  = do
  dev <- BladeRF $ lift get
  status <- liftIO $ c'bladerf_is_fpga_configured dev
  if status > 0 then do
    p <- liftIO (malloc :: IO (Ptr C'bladerf_version))
    liftIO $ c'bladerf_fpga_version dev p
    brfv <- liftIO $ peek p
    desc <- liftIO $ peekCString $ c'bladerf_version'describe brfv
    let ver = BladeRFVersion { major = c'bladerf_version'major brfv
                             , minor = c'bladerf_version'minor brfv
                             , patch = c'bladerf_version'patch brfv
                             , descr = desc
                             }
    liftIO $ free p
    return ver
  else
    return    BladeRFVersion { major = 0
                             , minor = 0
                             , patch = 0
                             , descr = "Unknown (FPGA not loaded)"
                             }

--
-- | Obtain the bus speed at which the device is operating
bladeRFDeviceSpeed :: BladeRF Word32
bladeRFDeviceSpeed  = do
  dev <- BladeRF $ lift get
  speed <- liftIO $ c'bladerf_device_speed dev
  return $ fromIntegral speed

--
-- | Fill out a provided bladerf_devinfo structure, given an open device handle.
bladeRFGetDevInfo :: BladeRF (String, String, Word8, Word8, CUInt)
bladeRFGetDevInfo = do
  p <- liftIO (malloc :: IO (Ptr C'bladerf_devinfo))
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_get_devinfo dev p
-- XXX ^ handle status return error with Maybe monad???
  brfv <- liftIO $ peek p
  -- XXX decode backend to string??
  let backend = show . c'bladerf_devinfo'backend $ brfv
  let serial = map castCCharToChar . c'bladerf_devinfo'serial $ brfv
  let usb_bus = c'bladerf_devinfo'usb_bus brfv
  let usb_addr = c'bladerf_devinfo'usb_addr brfv
  let inst = c'bladerf_devinfo'instance brfv
  liftIO $ free p
  return (backend, serial, usb_bus, usb_addr, inst)
