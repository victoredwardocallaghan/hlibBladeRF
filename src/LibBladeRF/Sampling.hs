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

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


--
-- | Configure the device's sample rate, in Hz.  Note this requires the sample
--   rate is an integer value of Hz.  Use bladeRFSetRationalSampleRate
--   for more arbitrary values.
bladeRFSetSampleRate :: BladeRFModule -> Int -> BladeRF Int
bladeRFSetSampleRate m r = do
  dev <- BladeRF $ lift get
  par <- liftIO (malloc :: IO (Ptr CUInt))
  liftIO $ c'bladerf_set_sample_rate dev ((fromIntegral . fromEnum) m) (fromIntegral r) par
  actual <- liftIO $ peek par
  liftIO $ free par
  return $ fromIntegral actual

--
-- | Configure the device's sample rate as a rational fraction of Hz.
--   Sample rates are in the form of integer + num/denom.
bladeRFSetRationalSampleRate :: BladeRFModule -> BladeRFRationalRate -> BladeRF BladeRFRationalRate
bladeRFSetRationalSampleRate m r = do
  dev <- BladeRF $ lift get
  pr <- liftIO (malloc :: IO (Ptr C'bladerf_rational_rate))
  par <- liftIO (malloc :: IO (Ptr C'bladerf_rational_rate))
  let rate = C'bladerf_rational_rate { c'bladerf_rational_rate'integer = integer r
                                     , c'bladerf_rational_rate'num     = num r
                                     , c'bladerf_rational_rate'den     = den r
                                     }
  liftIO $ poke pr rate
  liftIO $ c'bladerf_set_rational_sample_rate dev ((fromIntegral . fromEnum) m) pr par
  ar <- liftIO $ peek par
  let actual = BladeRFRationalRate { integer = c'bladerf_rational_rate'integer ar
                                   , num     = c'bladerf_rational_rate'num ar
                                   , den     = c'bladerf_rational_rate'den ar
                                   }
  liftIO $ free pr
  liftIO $ free par
  return actual

--
-- | Set the bandwidth of the LMS LPF to specified value in Hz
bladeRFSetBandwidth :: BladeRFModule -> Int -> BladeRF Int
bladeRFSetBandwidth m b = do
  dev <- BladeRF $ lift get
  ab <- liftIO (malloc :: IO (Ptr CUInt))
  liftIO $ c'bladerf_set_bandwidth dev ((fromIntegral . fromEnum) m) (fromIntegral b) ab
  actual <- liftIO $ peek ab
  liftIO $ free ab
  return $ fromIntegral actual
