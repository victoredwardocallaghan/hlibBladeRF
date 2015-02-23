{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates the foundational libbladeRF functions
  commonly used into a Monadic style. This Monadic style avoids
  passing around references of indirection to the device type and
  so on.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module LibBladeRF.LibBladeRF ( withBladeRF
                             , DeviceHandle(..)
                             ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception
import Data.Typeable (Typeable, cast)

import Bindings.LibBladeRF


--
-- | Error codes returned by internal libbladeRF functions.
data BladeRFError = BLADERF_ERR_UNEXPECTED
                  | BLADERF_ERR_RANGE
                  | BLADERF_ERR_INVAL
                  | BLADERF_ERR_MEM
                  | BLADERF_ERR_IO
                  | BLADERF_ERR_TIMEOUT
                  | BLADERF_ERR_NODEV
                  | BLADERF_ERR_UNSUPPORTED
                  | BLADERF_ERR_MISALIGNED
                  | BLADERF_ERR_CHECKSUM
                  | BLADERF_ERR_NO_FILE
                  | BLADERF_ERR_UPDATE_FPGA
                  | BLADERF_ERR_UPDATE_FW
                  | BLADERF_ERR_TIME_PAST
                   deriving (Typeable)

instance Show BladeRFError where
  show BLADERF_ERR_UNEXPECTED  = "An unexpected failure occurred"
  show BLADERF_ERR_RANGE       = "Provided parameter is out of range"
  show BLADERF_ERR_INVAL       = "Invalid operation/parameter"
  show BLADERF_ERR_MEM         = "Memory allocation error"
  show BLADERF_ERR_IO          = "File/Device I/O error"
  show BLADERF_ERR_TIMEOUT     = "Operation timed out"
  show BLADERF_ERR_NODEV       = "No device(s) available"
  show BLADERF_ERR_UNSUPPORTED = "Operation not supported"
  show BLADERF_ERR_MISALIGNED  = "Misaligned flash access"
  show BLADERF_ERR_CHECKSUM    = "Invalid checksum"
  show BLADERF_ERR_NO_FILE     = "File not found"
  show BLADERF_ERR_UPDATE_FPGA = "An FPGA update is required"
  show BLADERF_ERR_UPDATE_FW   = "A firmware update is requied"
  show BLADERF_ERR_TIME_PAST   = "Requested timestamp is in the past"

instance Exception BladeRFError

newtype DeviceHandle = DeviceHandle { unDeviceHandle :: Ptr C'bladerf }

-- | essential wrapper
withBladeRF :: (DeviceHandle -> IO a) -> IO ()
withBladeRF body = do
  dev <- openBladeRF
  body dev
  closeBladeRF dev

-- | Handy helper to wrap around Either results
openBladeRF :: IO DeviceHandle
openBladeRF = do
  r <- openBladeRF'
  case r of
    Left e -> throwIO e
    Right dev -> return dev

--
-- Open specified device using a device identifier string.
-- See bladerf_open_with_devinfo() if a device identifier string
-- is not readily available.
openBladeRF' :: IO (Either BladeRFError DeviceHandle)
openBladeRF'  = alloca $ \ptr -> do
  ret <- c'bladerf_open ptr nullPtr
  if ret /= 0 then
    return (Left BLADERF_ERR_NODEV) -- is this the right error in every case?
  else do
    pdev <- peek ptr
    return (Right (DeviceHandle pdev))

-- | Close device. Deallocates the memory allocated by openBladeRF when called.
closeBladeRF :: DeviceHandle -> IO ()
closeBladeRF d = c'bladerf_close $ unDeviceHandle d
