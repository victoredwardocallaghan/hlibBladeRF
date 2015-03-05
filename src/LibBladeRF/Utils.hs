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
                        , bladeRFLoadFPGA
                        , bladeRFGetDevInfo
                        , bladeRFGetSerial
                        , bladeRFGetFPGASize
                        , bladeRFEnableModule
                        , bladeRFSetLoopback
                        , bladeRFGetLoopback
                        ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


-- | Get libbladeRF version information.
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

-- | Query firmware version.
bladeRFFwVersion :: DeviceHandle      -- ^ Device handle
                 -> IO BladeRFVersion -- ^ Returned firmware version
bladeRFFwVersion dev = do
  p <- malloc :: IO (Ptr C'bladerf_version)
  c'bladerf_fw_version (unDeviceHandle dev) p
  brfv <- peek p
  desc <- peekCString $ c'bladerf_version'describe brfv
  let ver = BladeRFVersion { major = c'bladerf_version'major brfv
                           , minor = c'bladerf_version'minor brfv
                           , patch = c'bladerf_version'patch brfv
                           , descr = desc
                           }
  free p
  return ver

-- | Query FPGA version.
bladeRFFPGAVersion :: DeviceHandle      -- ^ Device handle
                   -> IO BladeRFVersion -- ^ Returned firmware version
bladeRFFPGAVersion dev = do
  status <- c'bladerf_is_fpga_configured (unDeviceHandle dev)
  if status > 0 then do
    p <- malloc :: IO (Ptr C'bladerf_version)
    c'bladerf_fpga_version (unDeviceHandle dev) p
    brfv <- peek p
    desc <- peekCString $ c'bladerf_version'describe brfv
    let ver = BladeRFVersion { major = c'bladerf_version'major brfv
                             , minor = c'bladerf_version'minor brfv
                             , patch = c'bladerf_version'patch brfv
                             , descr = desc
                             }
    free p
    return ver
  else
    return    BladeRFVersion { major = 0
                             , minor = 0
                             , patch = 0
                             , descr = "Unknown (FPGA not loaded)"
                             }


-- | Load device's FPGA.
--
-- Note that this FPGA configuration will be reset at the next
-- power cycle. Pass Full path to FPGA bitstream.
bladeRFLoadFPGA :: DeviceHandle -- ^ Device handle
                -> String       -- ^ Full path to FPGA bitstream
                -> IO ()
bladeRFLoadFPGA dev s = do
  p <- newCString s
  _ <- c'bladerf_load_fpga (unDeviceHandle dev) p
  return ()

-- | Obtain the bus speed at which the device is operating
bladeRFDeviceSpeed :: DeviceHandle    -- ^ Device handle
                   -> IO BladeRFSpeed -- ^ Device speed
bladeRFDeviceSpeed dev = do
  speed <- c'bladerf_device_speed (unDeviceHandle dev)
  return $ (toEnum . fromEnum) speed

-- | Fill out a provided 'LibBladeRF.Types.BladeRFDevInfo' structure, given an open device handle.
bladeRFGetDevInfo :: DeviceHandle         -- ^ Device handle
                  -> IO BladeRFDeviceInfo -- ^ Device information populated by this function
bladeRFGetDevInfo dev = do
  p <- malloc :: IO (Ptr C'bladerf_devinfo)
  c'bladerf_get_devinfo (unDeviceHandle dev) p
-- XXX ^ handle status return error with Maybe monad???
  brfv <- peek p
  let info = BladeRFDeviceInfo { backend = toEnum . fromEnum . c'bladerf_devinfo'backend $ brfv
                               , serial  = map castCCharToChar . c'bladerf_devinfo'serial $ brfv
                               , usbBus  = c'bladerf_devinfo'usb_bus brfv
                               , usbAddr = c'bladerf_devinfo'usb_addr brfv
                               , inst    = c'bladerf_devinfo'instance brfv
                               }
  free p
  return info

-- | Query a device's serial number.
bladeRFGetSerial :: DeviceHandle -- ^ Device handle
                 -> IO String    -- ^ Returned serial number.
bladeRFGetSerial dev = do
  cstring <- mallocBytes 34 -- device serial is 33 bytes long + null terminating byte.
  -- API bug bladerf_get_serial() should be allocating the buffer itself, not the call site!
  -- See upstream report: https://github.com/Nuand/bladeRF/issues/382
  c'bladerf_get_serial (unDeviceHandle dev) cstring
  serial <- peekCString cstring
  free cstring
  return serial

-- | Query a device's FPGA size.
bladeRFGetFPGASize :: DeviceHandle       -- ^ Device handle
                   -> IO BladeRFFPGASize -- ^ Returned on-board FPGA's size.
bladeRFGetFPGASize dev = do
  p <- malloc :: IO (Ptr C'bladerf_fpga_size)
  c'bladerf_get_fpga_size (unDeviceHandle dev) p
  sz <- peek p
  free p
  return $ (toEnum . fromEnum) sz

-- | Enable or disable the specified RX/TX module.
--
-- When a synchronous stream is associated with the specified module, this
-- will shut down the underlying asynchronous stream when `enable` = 'False'.
bladeRFEnableModule :: DeviceHandle  -- ^ Device handle
                    -> BladeRFModule -- ^ Device module
                    -> Bool          -- ^ 'True' to enable, 'False' to disable
                    -> IO ()
bladeRFEnableModule dev m t = do
  c'bladerf_enable_module (unDeviceHandle dev) ((fromIntegral . fromEnum) m) t
  return () -- XXX ignore ret

-- | Apply specified loopback mode.
--
-- Loopback modes should only be enabled or disabled while the RX and TX
-- modules are both disabled (and therefore, when no samples are being
-- actively streamed). Otherwise, unexpected behavior may occur.
bladeRFSetLoopback :: DeviceHandle    -- ^ Device handle
                   -> BladeRFLoopback -- ^ Loopback mode. Note that 'BLADERF_LB_NONE'
                                      --   disables the use of loopback functionality.
                   -> IO ()
bladeRFSetLoopback dev l = do
  c'bladerf_set_loopback (unDeviceHandle dev) ((fromIntegral . fromEnum) l)
  return () -- XXX ignore ret

-- | Get current loopback mode.
bladeRFGetLoopback :: DeviceHandle       -- ^ Device handle
                   -> IO BladeRFLoopback -- ^ Current loopback mode
bladeRFGetLoopback dev = do
  l <- alloca $ \lp -> do
    c'bladerf_get_loopback (unDeviceHandle dev) lp
    peek lp
  return $ (toEnum . fromEnum) l