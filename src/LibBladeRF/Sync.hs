{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates Synchronous data transmission and reception
 
  This group of functions presents synchronous, blocking calls (with optional
  timeouts) for transmitting and receiving samples.
 
  The synchronous interface is built atop the asynchronous interface, and is
  generally less complex and easier to work with.  It alleviates the need to
  explicitly spawn threads (it is done under the hood) and manually manage
  sample buffers.
 
  Under the hood, this interface spawns worker threads to handle an
  asynchronous stream and perform thread-safe buffer management.
-}

module LibBladeRF.Sync ( bladeRFSyncConfig
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
-- | (Re)Configure a device for synchronous transmission or reception
bladeRFSyncConfig :: BladeRFModule -- ^ Module to use with synchronous interface
                  -> BladeRFFormat -- ^ Format to use in synchronous data transfers
                  -> Int           -- ^ The number of buffers to use in the underlying data stream.
                  -> Int           -- ^ The size of the underlying stream buffers, in samples. This value must be a multiple of 1024.
                  -> Int           -- ^ The number of active USB transfers that may be in-flight at any given time.
                  -> Int           -- ^ Timeout (milliseconds) for transfers in the underlying data stream.
                  -> BladeRF ()

bladeRFSyncConfig m f nb sz tr to = do
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_sync_config dev ((fromIntegral . fromEnum) m) ((fromIntegral . fromEnum) f) (fromIntegral nb) (fromIntegral sz) (fromIntegral tr) (fromIntegral to)
  return () -- ignores ret


--
-- | Transmit IQ samples.
-- bladeRFSyncTx :: ?

--
-- | Receive IQ samples.
-- bladeRFSyncRX :: ?
