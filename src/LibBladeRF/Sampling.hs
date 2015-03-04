{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates sampling rate libbladeRF library functions.
-}

module LibBladeRF.Sampling ( bladeRFSetSampleRate
                           , bladeRFSetRationalSampleRate
                           , bladeRFSetBandwidth
                           ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


-- | Configure the device's sample rate, in Hz.  Note this requires the sample
--   rate is an integer value of Hz.  Use bladeRFSetRationalSampleRate
--   for more arbitrary values.
bladeRFSetSampleRate :: DeviceHandle  -- ^ Device handle
                     -> BladeRFModule -- ^ Module to change
                     -> Int           -- ^ Sample rate
                     -> IO Int        -- ^ Actual sample rate achieved.
bladeRFSetSampleRate dev m r = do
  par <- malloc :: IO (Ptr CUInt)
  c'bladerf_set_sample_rate (unDeviceHandle dev) ((fromIntegral . fromEnum) m) (fromIntegral r) par
  actual <- peek par
  free par
  return $ fromIntegral actual

-- | Configure the device's sample rate as a rational fraction of Hz.
--   Sample rates are in the form of integer + num/denom.
bladeRFSetRationalSampleRate :: DeviceHandle           -- ^ Device handle
                             -> BladeRFModule          -- ^ Module to change
                             -> BladeRFRationalRate    -- ^ Rational sample rate
                             -> IO BladeRFRationalRate -- ^ Actual rational sample rate achieved.
bladeRFSetRationalSampleRate dev m r = do
  pr <- malloc :: IO (Ptr C'bladerf_rational_rate)
  par <- malloc :: IO (Ptr C'bladerf_rational_rate)
  let rate = C'bladerf_rational_rate { c'bladerf_rational_rate'integer = integer r
                                     , c'bladerf_rational_rate'num     = num r
                                     , c'bladerf_rational_rate'den     = den r
                                     }
  poke pr rate
  c'bladerf_set_rational_sample_rate (unDeviceHandle dev) ((fromIntegral . fromEnum) m) pr par
  ar <- peek par
  let actual = BladeRFRationalRate { integer = c'bladerf_rational_rate'integer ar
                                   , num     = c'bladerf_rational_rate'num ar
                                   , den     = c'bladerf_rational_rate'den ar
                                   }
  free pr
  free par
  return actual

-- | Set the bandwidth of the LMS LPF to specified value in Hz
bladeRFSetBandwidth :: DeviceHandle  -- ^ Device handle
                    -> BladeRFModule -- ^ Module for bandwidth request
                    -> Int           -- ^ Desired bandwidth
                    -> IO Int        -- ^ Actual bandwidth that the device was able to achieve.
bladeRFSetBandwidth dev m b = do
  ab <- malloc :: IO (Ptr CUInt)
  c'bladerf_set_bandwidth (unDeviceHandle dev) ((fromIntegral . fromEnum) m) (fromIntegral b) ab
  actual <- peek ab
  free ab
  return $ fromIntegral actual
