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
--                        , bladeRFWriteFlash
                        ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF


-- | Erase regions of the bladeRF's SPI flash
--
-- This function operates in units of 64KiB erase blocks
bladeRFEraseFlash :: DeviceHandle  -- ^ Device handle
                  -> Word32        -- ^ Erase block to start erasing at
                  -> Word32        -- ^ Number of blocks to erase.
                  -> IO CInt
bladeRFEraseFlash dev b c = c'bladerf_erase_flash (unDeviceHandle dev) b c

-- | Read data from the bladeRF's SPI flash
--
-- This function operates in units of 256-byte pages.
bladeRFReadFlash :: DeviceHandle -- ^ Device handle
                 -> Word32       -- ^ Page to begin reading from
                 -> Word32       -- ^ Number of pages to read
                 -> IO (CInt, Word8)
bladeRFReadFlash dev p c = do
  bptr <- malloc :: IO (Ptr Word8)
  ret <- c'bladerf_read_flash (unDeviceHandle dev) bptr p c
  buffer <- peek bptr
  free bptr
  return (ret, buffer)

-- | Write data from the bladeRF's SPI flash
-- FIXME - how to pass in a buffer of data???
--bladeRFWriteFlash :: DeviceHandle -> Word32 -> Word32 -> IO (CInt, Word8)
--bladeRFWriteFlash dev p c = do
--  bptr <- malloc :: IO (Ptr Word8)
--  ret <- c'bladerf_write_flash (unDeviceHandle dev) bptr p c
--  buffer <- peek bptr
--  free bptr
--  return (ret, buffer)
