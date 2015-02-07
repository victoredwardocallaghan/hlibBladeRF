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
                        , bladeRFGetSerial
                        , bladeRFGetFPGASize
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
bladeRFGetDevInfo :: BladeRF BladeRFDeviceInfo
bladeRFGetDevInfo = do
  p <- liftIO (malloc :: IO (Ptr C'bladerf_devinfo))
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_get_devinfo dev p
-- XXX ^ handle status return error with Maybe monad???
  brfv <- liftIO $ peek p
  let info = BladeRFDeviceInfo { backend = toEnum . fromEnum . c'bladerf_devinfo'backend $ brfv
                               , serial  = map castCCharToChar . c'bladerf_devinfo'serial $ brfv
                               , usbBus  = c'bladerf_devinfo'usb_bus brfv
                               , usbAddr = c'bladerf_devinfo'usb_addr brfv
                               , inst    = c'bladerf_devinfo'instance brfv
                               }
  liftIO $ free p
  return info

--
-- | Query a device's serial number
bladeRFGetSerial :: BladeRF String
bladeRFGetSerial  = do
  cstring <- liftIO (mallocBytes 34) -- device serial is 33 bytes long + null terminating byte.
  dev <- BladeRF $ lift get
  -- API bug bladerf_get_serial() should be allocating the buffer itself, not the call site!
  liftIO $ c'bladerf_get_serial dev cstring
  serial <- liftIO $ peekCString cstring
  liftIO $ free cstring
  return serial

--
-- | Query a device's FPGA size
bladeRFGetFPGASize :: BladeRF BladeRFFPGASize
bladeRFGetFPGASize  = do
  dev <- BladeRF $ lift get
  p <- liftIO (malloc :: IO (Ptr C'bladerf_fpga_size))
  liftIO $ c'bladerf_get_fpga_size dev p
  sz <- liftIO $ peek p
  liftIO $ free p
  return $ (toEnum . fromEnum) sz
