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
                -> IO ()
bladeRFDACWrite dev v = do
  c'bladerf_dac_write (unDeviceHandle dev) v
  return () -- ignores ret

-- | Get module's current frequency in Hz.
bladeRFGetFrequency :: DeviceHandle  -- ^ Device handle
                    -> BladeRFModule -- ^ Module to configure
                    -> IO Int        -- ^ Returned frequency
bladeRFGetFrequency dev m = do
  pf <- malloc :: IO (Ptr CUInt)
  c'bladerf_get_frequency (unDeviceHandle dev) ((fromIntegral . fromEnum) m) pf
  f <- peek pf
  free pf
  return $ fromIntegral f

-- | Set module's frequency in Hz.
bladeRFSetFrequency :: DeviceHandle  -- ^ Device handle
                    -> BladeRFModule -- ^ Module to configure
                    -> Int           -- ^ Desired frequency
                    -> IO ()
bladeRFSetFrequency dev m f = do
  c'bladerf_set_frequency (unDeviceHandle dev) ((fromIntegral . fromEnum) m) (fromIntegral f)
  return () -- ignores ret

-- | Obtain the current value of the specified configuration parameter.
bladeRFGetCorrection :: DeviceHandle       -- ^ Device handle
                     -> BladeRFModule      -- ^ Module to retrieve correction information from
                     -> BladeRFCorrection  -- ^ Correction type
                     -> IO Word16          -- ^ Current value
bladeRFGetCorrection dev m c = do
  pc <- malloc :: IO (Ptr Word16)
  c'bladerf_get_correction (unDeviceHandle dev) ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) pc
  c <- peek pc
  free pc
  return c

-- | Set the value of the specified configuration parameter.
bladeRFSetCorrection :: DeviceHandle      -- ^ Device handle
                     -> BladeRFModule     -- ^ Module to apply correction to
                     -> BladeRFCorrection -- ^ Correction type
                     -> Word16            -- ^ Value to apply
                     -> IO ()
bladeRFSetCorrection dev m c v = do
  c'bladerf_set_correction (unDeviceHandle dev) ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) v
  return () -- ignores ret
