{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module GPIO configuration handling
-}

module LibBladeRF.Gpio ( bladeRFConfigGPIORead
                       , bladeRFConfigGPIOWrite
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
-- | Read a configuration GPIO register
bladeRFConfigGPIORead :: BladeRF Word32
bladeRFConfigGPIORead  = do
  dev <- BladeRF $ lift get
  pv <- liftIO (malloc :: IO (Ptr Word32))
  liftIO $ c'bladerf_config_gpio_read dev pv
  v <- liftIO $ peek pv
  liftIO $ free pv
  return v

--
-- | Write a configuration GPIO register. Callers should be sure to perform a
--   read-modify-write sequence to avoid accidentally clearing other
--   GPIO bits that may be set by the library internally.
bladeRFConfigGPIOWrite :: Word32 -> BladeRF ()
bladeRFConfigGPIOWrite v = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_config_gpio_write dev v
  return () -- ignores ret
