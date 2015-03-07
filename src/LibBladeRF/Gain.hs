{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module deals with Gain control.
-}

module LibBladeRF.Gain ( -- * set,get TxVGA2
                         bladeRFSetTXVGA2
                       , bladeRFGetTXVGA2
                       -- * set,get TxVGA1
                       , bladeRFSetTXVGA1
                       , bladeRFGetTXVGA1
                       -- * set,get RxVGA2
                       , bladeRFSetRXVGA2
                       , bladeRFGetRXVGA2
                       -- * set,get RxVGA1
                       , bladeRFSetRXVGA1
                       , bladeRFGetRXVGA1
                       -- * set,gain LNA Gain
                       , bladeRFSetLNAGain
                       , bladeRFGetLNAGain
                       -- * Optimal gain control
                       , bladeRFSetGain
                       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


-- | Set the PA gain in dB.
bladeRFSetTXVGA2 :: DeviceHandle -- ^ Device handle
                 -> Int          -- ^ Desired gain
                 -> IO (Either BladeRFError ())
bladeRFSetTXVGA2 dev g = do
  ret <- c'bladerf_set_txvga2 (unDeviceHandle dev) (fromIntegral g)
  return $ bladeRFErrorTy ret

-- | Get the PA gain in dB.
bladeRFGetTXVGA2 :: DeviceHandle -- ^ Device handle
                 -> IO Int       -- ^ Returned gain
bladeRFGetTXVGA2 dev = do
  g <- alloca $ \p -> do
    c'bladerf_get_txvga2 (unDeviceHandle dev) p
    peek p
  return $ fromIntegral g

-- | Set the post-LPF gain in dB.
bladeRFSetTXVGA1 :: DeviceHandle         -- ^ Device handle
                 -> BladeRFVGAGainBounds -- ^ Desired gain
                 -> IO (Either BladeRFError ())
bladeRFSetTXVGA1 dev g = do
  ret <- c'bladerf_set_txvga1 (unDeviceHandle dev) ((fromIntegral . fromEnum) g)
  return $ bladeRFErrorTy ret

-- | Get the post-LPF gain in dB.
bladeRFGetTXVGA1 :: DeviceHandle -- ^ Device handle
                 -> IO Int       -- ^ Returned gain
bladeRFGetTXVGA1 dev = do
  g <- alloca $ \p -> do
    c'bladerf_get_txvga1 (unDeviceHandle dev) p
    peek p
  return $ fromIntegral g

-- | Set the post-LPF VGA gain.
bladeRFSetRXVGA2 :: DeviceHandle         -- ^ Device handle
                 -> BladeRFVGAGainBounds -- ^ Desired gain
                 -> IO (Either BladeRFError ())
bladeRFSetRXVGA2 dev g = do
  ret <- c'bladerf_set_rxvga2 (unDeviceHandle dev) ((fromIntegral . fromEnum) g)
  return $ bladeRFErrorTy ret

-- | Get the post-LPF VGA gain.
bladeRFGetRXVGA2 :: DeviceHandle -- ^ Device handle
                 -> IO Int       -- ^ Returned set gain level
bladeRFGetRXVGA2 dev = do
  g <- alloca $ \p -> do
    c'bladerf_get_rxvga2 (unDeviceHandle dev) p
    peek p
  return $ fromIntegral g

-- | Set the pre-LPF VGA gain.
bladeRFSetRXVGA1 :: DeviceHandle         -- ^ Device handle
                 -> BladeRFVGAGainBounds -- ^ Desired gain
                 -> IO (Either BladeRFError ())
bladeRFSetRXVGA1 dev g = do
  ret <- c'bladerf_set_rxvga1 (unDeviceHandle dev) ((fromIntegral . fromEnum) g)
  return $ bladeRFErrorTy ret

-- | Get the pre-LPF VGA gain.
bladeRFGetRXVGA1 :: DeviceHandle -- ^ Device handle
                 -> IO Int       -- ^ Returned set gain level
bladeRFGetRXVGA1 dev = do
  g <- alloca $ \p -> do
    c'bladerf_get_rxvga1 (unDeviceHandle dev) p
    peek p
  return $ fromIntegral g

-- | Set LNA Gain.
bladeRFSetLNAGain :: DeviceHandle   -- ^ Device handle
                  -> BladeRFLNAGain -- ^ Desired gain level
                  -> IO (Either BladeRFError ())
bladeRFSetLNAGain dev g = do
  ret <- c'bladerf_set_lna_gain (unDeviceHandle dev) ((fromIntegral . fromEnum) g)
  return $ bladeRFErrorTy ret

-- | Get LNA Gain.
bladeRFGetLNAGain :: DeviceHandle      -- ^ Device handle
                  -> IO BladeRFLNAGain -- ^ Returned set gain level
bladeRFGetLNAGain dev = do
  g <- alloca $ \p -> do
    c'bladerf_get_lna_gain (unDeviceHandle dev) p
    peek p
  return $ (toEnum . fromIntegral) g

-- | Set a combined pre and post LPF RX gain.
--
-- This action computes the optimal LNA, RXVGA1, and RVGA2 gains for a
-- requested amount of RX gain, and computes the optimal TXVGA1 and TXVGA2
-- gains for a requested amount of TX gain
bladeRFSetGain :: DeviceHandle  -- ^ Device handle
               -> BladeRFModule -- ^ Module
               -> Int           -- ^ Desired gain
               -> IO (Either BladeRFError ())
bladeRFSetGain dev m g = do
  ret <- c'bladerf_set_gain (unDeviceHandle dev) ((fromIntegral . fromEnum) m) (fromIntegral g)
  return $ bladeRFErrorTy ret
