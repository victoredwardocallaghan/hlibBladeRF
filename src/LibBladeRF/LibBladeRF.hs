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
{-# OPTIONS_HADDOCK prune #-}

module LibBladeRF.LibBladeRF ( withBladeRF
                             , DeviceHandle(..)
                             , BladeRFError(..)
                             , bladeRFErrorValue
                             , bladeRFErrorTy
                             ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception
import Data.Typeable (Typeable, cast)
import Data.Maybe
import Data.Tuple

import Bindings.LibBladeRF


-- | Error codes returned by internal libbladeRF functions.
data BladeRFError = BLADERF_ERR_UNEXPECTED  -- ^ An unexpected failure occurred
                  | BLADERF_ERR_RANGE       -- ^ Provided parameter is out of range
                  | BLADERF_ERR_INVAL       -- ^ Invalid operation/parameter
                  | BLADERF_ERR_MEM         -- ^ Memory allocation error
                  | BLADERF_ERR_IO          -- ^ File/Device I/O error
                  | BLADERF_ERR_TIMEOUT     -- ^ Operation timed out
                  | BLADERF_ERR_NODEV       -- ^ No device(s) available
                  | BLADERF_ERR_UNSUPPORTED -- ^ Operation not supported
                  | BLADERF_ERR_MISALIGNED  -- ^ Misaligned flash access
                  | BLADERF_ERR_CHECKSUM    -- ^ Invalid checksum
                  | BLADERF_ERR_NO_FILE     -- ^ File not found
                  | BLADERF_ERR_UPDATE_FPGA -- ^ An FPGA update is required
                  | BLADERF_ERR_UPDATE_FW   -- ^ A firmware update is requied
                  | BLADERF_ERR_TIME_PAST   -- ^ Requested timestamp is in the past
                   deriving (Eq, Typeable)

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

-- | Returned C Error codes
--
-- bladeRF library routines return negative values to indicate errors.
-- Values >= 0 are used to indicate success.
instance Enum BladeRFError where
  fromEnum = fromJust . flip lookup errors
  toEnum   = fromJust . flip lookup (map swap errors)

errors = [ (BLADERF_ERR_UNEXPECTED, c'BLADERF_ERR_UNEXPECTED)
         , (BLADERF_ERR_RANGE, c'BLADERF_ERR_RANGE)
         , (BLADERF_ERR_INVAL, c'BLADERF_ERR_INVAL)
         , (BLADERF_ERR_MEM, c'BLADERF_ERR_MEM)
         , (BLADERF_ERR_IO, c'BLADERF_ERR_IO)
         , (BLADERF_ERR_TIMEOUT, c'BLADERF_ERR_TIMEOUT)
         , (BLADERF_ERR_NODEV, c'BLADERF_ERR_NODEV)
         , (BLADERF_ERR_UNSUPPORTED, c'BLADERF_ERR_UNSUPPORTED)
         , (BLADERF_ERR_MISALIGNED, c'BLADERF_ERR_MISALIGNED)
         , (BLADERF_ERR_CHECKSUM, c'BLADERF_ERR_CHECKSUM)
         , (BLADERF_ERR_NO_FILE, c'BLADERF_ERR_NO_FILE)
         , (BLADERF_ERR_UPDATE_FPGA, c'BLADERF_ERR_UPDATE_FPGA)
         , (BLADERF_ERR_UPDATE_FW, c'BLADERF_ERR_UPDATE_FW)
         , (BLADERF_ERR_TIME_PAST, c'BLADERF_ERR_TIME_PAST)
         ]

-- | (For internal use) Obtain a 'BladeRFError' type of a C value from the Error codes list.
bladeRFErrorValue :: CInt -> Either BladeRFError Int
bladeRFErrorValue c | c >= 0 = (Right . fromIntegral) c         -- Success (on ret == 0)
                    | c <  0 = (Left . toEnum . fromIntegral) c -- C ret code to typed error

-- | (For internal use) Obtain a 'BladeRFError' type of a C value from the Error codes list.
bladeRFErrorTy :: CInt -> Either BladeRFError ()
bladeRFErrorTy c | c >= 0 = return ()                        -- Success (on ret == 0)
                 | c <  0 = (Left . toEnum . fromIntegral) c -- C ret code to typed error

-- | DeviceHandle wrapper around C device descriptor pointer
newtype DeviceHandle = DeviceHandle { unDeviceHandle :: Ptr C'bladerf }

-- | Essential wrapper
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

-- Open specified device using a device identifier string.
-- See bladerf_open_with_devinfo() if a device identifier string
-- is not readily available.
openBladeRF' :: IO (Either BladeRFError DeviceHandle)
openBladeRF'  = alloca $ \ptr -> do
  ret <- c'bladerf_open ptr nullPtr
  if ret /= 0 then
    return $ (Left . toEnum . fromIntegral) ret
  else do
    pdev <- peek ptr
    return (Right (DeviceHandle pdev))

-- | Close device. Deallocates the memory allocated by openBladeRF when called.
closeBladeRF :: DeviceHandle -> IO ()
closeBladeRF d = c'bladerf_close $ unDeviceHandle d
