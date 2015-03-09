{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates frequency adjustments libbladeRF library functions.
-}

module LibBladeRF.Frequency ( bladeRFDACWrite
                            , bladeRFGetFrequency
                            , bladeRFSetFrequency
                            , bladeRFGetCorrection
                            , bladeRFSetCorrection
                            ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


-- | Write value to VCTCXO DAC.
bladeRFDACWrite :: DeviceHandle -- ^ Device handle
                -> Word16       -- ^ Data to write to DAC register
                -> IO (BladeRFReturnType ())
bladeRFDACWrite dev v = do
  ret <- c'bladerf_dac_write (unDeviceHandle dev) v
  return $ bladeRFErrorTy ret

-- | Get module's current frequency in Hz.
bladeRFGetFrequency :: DeviceHandle  -- ^ Device handle
                    -> BladeRFModule -- ^ Module to configure
                    -> IO Int        -- ^ Returned frequency
bladeRFGetFrequency dev m = do
  f <- alloca $ \pf -> do
    c'bladerf_get_frequency (unDeviceHandle dev) ((fromIntegral . fromEnum) m) pf
    peek pf
  return $ fromIntegral f

-- | Set module's frequency in Hz.
bladeRFSetFrequency :: DeviceHandle  -- ^ Device handle
                    -> BladeRFModule -- ^ Module to configure
                    -> Int           -- ^ Desired frequency
                    -> IO (BladeRFReturnType ())
bladeRFSetFrequency dev m f = do
  ret <- c'bladerf_set_frequency (unDeviceHandle dev) ((fromIntegral . fromEnum) m) (fromIntegral f)
  return $ bladeRFErrorTy ret

-- | Obtain the current value of the specified configuration parameter.
bladeRFGetCorrection :: DeviceHandle       -- ^ Device handle
                     -> BladeRFModule      -- ^ Module to retrieve correction information from
                     -> BladeRFCorrection  -- ^ Correction type
                     -> IO Word16          -- ^ Current value
bladeRFGetCorrection dev m c = alloca $ \pc -> do
  c'bladerf_get_correction (unDeviceHandle dev) ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) pc
  peek pc

-- | Set the value of the specified configuration parameter.
bladeRFSetCorrection :: DeviceHandle      -- ^ Device handle
                     -> BladeRFModule     -- ^ Module to apply correction to
                     -> BladeRFCorrection -- ^ Correction type
                     -> Word16            -- ^ Value to apply
                     -> IO (BladeRFReturnType ())
bladeRFSetCorrection dev m c v = do
  ret <- c'bladerf_set_correction (unDeviceHandle dev) ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) v
  return $ bladeRFErrorTy ret
