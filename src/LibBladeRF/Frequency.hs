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
bladeRFDACWrite :: Word16 -- ^ Data to write to DAC register
                -> BladeRF ()
bladeRFDACWrite v = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_dac_write dev v
  return () -- ignores ret

--
-- | ..
bladeRFGetFrequency :: BladeRFModule -> BladeRF Int
bladeRFGetFrequency m = do
  dev <- BladeRF $ lift get
  pf <- liftIO (malloc :: IO (Ptr CUInt))
  liftIO $ c'bladerf_get_frequency dev ((fromIntegral . fromEnum) m) pf
  f <- liftIO $ peek pf
  liftIO $ free pf
  return $ fromIntegral f

--
-- | ..
bladeRFSetFrequency :: BladeRFModule -> Int -> BladeRF ()
bladeRFSetFrequency m f = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_frequency dev ((fromIntegral . fromEnum) m) (fromIntegral f)
  return () -- ignores ret

--
-- | ..
bladeRFGetCorrection :: BladeRFModule -> BladeRFCorrection -> BladeRF Word16
bladeRFGetCorrection m c = do
  dev <- BladeRF $ lift get
  pc <- liftIO (malloc :: IO (Ptr Word16))
  liftIO $ c'bladerf_get_correction dev ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) pc
  c <- liftIO $ peek pc
  liftIO $ free pc
  return c

--
-- | ..
bladeRFSetCorrection :: BladeRFModule -> BladeRFCorrection -> Word16 -> BladeRF ()
bladeRFSetCorrection m c v = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_correction dev ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) c) v
  return () -- ignores ret
