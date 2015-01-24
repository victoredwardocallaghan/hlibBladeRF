{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates flash libbladeRF library functions.
  WARNING !!! Untested !!!
-}

module LibBladeRF.Flash ( bladeRFEraseFlash
                        , bladeRFReadFlash
                        , bladeRFWriteFlash
                        ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF


--
-- | Erase regions of the bladeRF's SPI flash
bladeRFEraseFlash :: Word32 -> Word32 -> BladeRF CInt
bladeRFEraseFlash b c = do
  dev <- BladeRF $ lift get
  ret <- liftIO $ c'bladerf_erase_flash dev b c
  return ret

--
-- | Read data from the bladeRF's SPI flash
bladeRFReadFlash :: Word32 -> Word32 -> BladeRF (CInt, Word8)
bladeRFReadFlash p c = do
  dev <- BladeRF $ lift get
  bptr <- liftIO (malloc :: IO (Ptr Word8))
  ret <- liftIO $ c'bladerf_read_flash dev bptr p c
  buffer <- liftIO $ peek bptr
  liftIO $ free bptr
  return (ret, buffer)

--
-- | Write data from the bladeRF's SPI flash
-- FIXME - how to pass in a buffer of data???
--bladeRFWriteFlash :: Word32 -> Word32 -> BladeRF (CInt, Word8)
--bladeRFWriteFlash p c = do
--  dev <- BladeRF $ lift get
--  bptr <- liftIO (malloc :: IO (Ptr Word8))
--  ret <- liftIO $ c'bladerf_read_flash dev bptr p c
--  buffer <- liftIO $ peek bptr
--  liftIO $ free bptr
--  return (ret, buffer)
