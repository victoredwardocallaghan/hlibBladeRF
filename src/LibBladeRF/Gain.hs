{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module deals with Gain control
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
-- XXX symb not found!!!
--                       , bladeRFSetTXGain
                       , bladeRFSetGain
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
-- | Set the PA gain in dB
bladeRFSetTXVGA2 :: Int -> BladeRF ()
bladeRFSetTXVGA2 g = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_txvga2 dev (fromIntegral g)
  return () -- ignores ret

--
-- | Get the PA gain in dB
bladeRFGetTXVGA2 :: BladeRF Int
bladeRFGetTXVGA2  = do
  dev <- BladeRF $ lift get
  p <- liftIO (malloc :: IO (Ptr CInt))
  liftIO $ c'bladerf_get_txvga2 dev p
  g <- liftIO $ peek p
  liftIO $ free p
  return $ fromIntegral g

--
-- | Set the post-LPF gain in dB
bladeRFSetTXVGA1 :: Int -> BladeRF ()
bladeRFSetTXVGA1 g = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_txvga1 dev (fromIntegral g)
  return () -- ignores ret

--
-- | Get the post-LPF gain in dB
bladeRFGetTXVGA1 :: BladeRF Int
bladeRFGetTXVGA1  = do
  dev <- BladeRF $ lift get
  p <- liftIO (malloc :: IO (Ptr CInt))
  liftIO $ c'bladerf_get_txvga1 dev p
  g <- liftIO $ peek p
  liftIO $ free p
  return $ fromIntegral g

--
-- | Set the post-LPF VGA gain
bladeRFSetRXVGA2 :: Int -> BladeRF ()
bladeRFSetRXVGA2 g = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_rxvga2 dev (fromIntegral g)
  return () -- ignores ret

--
-- | Get the post-LPF VGA gain
bladeRFGetRXVGA2 :: BladeRF Int
bladeRFGetRXVGA2  = do
  dev <- BladeRF $ lift get
  p <- liftIO (malloc :: IO (Ptr CInt))
  liftIO $ c'bladerf_get_rxvga2 dev p
  g <- liftIO $ peek p
  liftIO $ free p
  return $ fromIntegral g

--
-- | Set the pre-LPF VGA gain
bladeRFSetRXVGA1 :: Int -> BladeRF ()
bladeRFSetRXVGA1 g = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_rxvga1 dev (fromIntegral g)
  return () -- ignores ret

--
-- | Get the pre-LPF VGA gain
bladeRFGetRXVGA1 :: BladeRF Int
bladeRFGetRXVGA1  = do
  dev <- BladeRF $ lift get
  p <- liftIO (malloc :: IO (Ptr CInt))
  liftIO $ c'bladerf_get_rxvga1 dev p
  g <- liftIO $ peek p
  liftIO $ free p
  return $ fromIntegral g

--
-- | Set LNA Gain
bladeRFSetLNAGain :: BladeRFLNAGain -> BladeRF ()
bladeRFSetLNAGain g = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_lna_gain dev ((fromIntegral . fromEnum) g)
  return () -- ignores ret

--
-- | Get LNA Gain
bladeRFGetLNAGain :: BladeRF BladeRFLNAGain
bladeRFGetLNAGain  = do
  dev <- BladeRF $ lift get
  p <- liftIO (malloc :: IO (Ptr C'bladerf_lna_gain))
  liftIO $ c'bladerf_get_lna_gain dev p
  g <- liftIO $ peek p
  liftIO $ free p
  return $ (toEnum . fromIntegral) g

--
-- | Set a combined VGA TX gain
--   This function computes the optimal TXVGA1 and TXVGA2 gains for a requested
--   amount of gain
-- XXX symb not found!!!
--bladeRFSetTXGain :: Int -> BladeRF ()
--bladeRFSetTXGain g = do
--  dev <- BladeRF $ lift get
--  liftIO $ c'bladerf_set_tx_gain dev (fromIntegral g)
--  return () -- ignores ret

-- | Set a combined pre and post LPF RX gain
--   This function computes the optimal LNA, RXVGA1, and RVGA2 gains for a
--   requested amount of RX gain, and computes the optimal TXVGA1 and TXVGA2 gains
--   for a requested amount of TX gain
bladeRFSetGain :: BladeRFModule -> Int -> BladeRF ()
bladeRFSetGain m g = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_set_gain dev ((fromIntegral . fromEnum) m) (fromIntegral g)
  return () -- ignores ret
