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

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


--
-- | Write value to VCTCXO DAC
bladeRFDACWrite :: DeviceHandle
                -> Word16 -- ^ Data to write to DAC register
                -> IO ()
bladeRFDACWrite dev v = do
  c'bladerf_dac_write (unDeviceHandle dev) v
  return () -- ignores ret

--
-- | ..
bladeRFGetFrequency :: DeviceHandle -> BladeRFModule -> IO Int
bladeRFGetFrequency dev m = do
  pf <- malloc :: IO (Ptr CUInt)
  c'bladerf_get_frequency (unDeviceHandle dev) ((fromIntegral . fromEnum) m) pf
  f <- peek pf
  free pf
  return $ fromIntegral f

--
-- | ..
bladeRFSetFrequency :: DeviceHandle -> BladeRFModule -> Int -> IO ()
bladeRFSetFrequency dev m f = do
  c'bladerf_set_frequency (unDeviceHandle dev) ((fromIntegral . fromEnum) m) (fromIntegral f)
  return () -- ignores ret

--
-- | ..
bladeRFGetCorrection :: DeviceHandle -> BladeRFModule -> BladeRFCorrection -> IO Word16
bladeRFGetCorrection dev m c = do
  pc <- malloc :: IO (Ptr Word16)
  c'bladerf_get_correction (unDeviceHandle dev) ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) pc
  c <- peek pc
  free pc
  return c

--
-- | ..
bladeRFSetCorrection :: DeviceHandle -> BladeRFModule -> BladeRFCorrection -> Word16 -> IO ()
bladeRFSetCorrection dev m c v = do
  c'bladerf_set_correction (unDeviceHandle dev) ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) v
  return () -- ignores ret
