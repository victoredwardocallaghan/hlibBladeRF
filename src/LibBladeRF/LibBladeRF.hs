{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates the foundational libbladeRF functions
  commonly used into a Monadic style. This Monadic style avoids
  passing around references of indirection to the device type and
  so on.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LibBladeRF.LibBladeRF ( withBladeRF
                             , BladeRF(..)
                             ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap)
import Control.Monad.Trans
-- import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Bindings.LibBladeRF


--
-- | Error codes returned by internal libbladeRF functions.
data BladeRFError = BLADERF_ERR_UNEXPECTED
                  | BLADERF_ERR_RANGE
                  | BLADERF_ERR_INVAL
                  | BLADERF_ERR_MEM
                  | BLADERF_ERR_IO
                  | BLADERF_ERR_TIMEOUT
                  | BLADERF_ERR_NODEV
                  | BLADERF_ERR_UNSUPPORTED
                  | BLADERF_ERR_MISALIGNED
                  | BLADERF_ERR_CHECKSUM
                  | BLADERF_ERR_NO_FILE
                  | BLADERF_ERR_UPDATE_FPGA
                  | BLADERF_ERR_UPDATE_FW
                  | BLADERF_ERR_TIME_PAST

instance Show BladeRFError where
  show BLADERF_ERR_UNEXPECTED  = "An unexpected failure occurred"
  show BLADERF_ERR_RANGE       = "Provided parameter is out of range"
  show BLADERF_ERR_INVAL       = "Invalid operation/parameter"
  show BLADERF_ERR_MEM         = "Memory allocation error"
  show BLADERF_ERR_IO          = "File/Device I/O error"
  show BLADERF_ERR_TIMEOUT     = "Operation timed out"
  show BLADERF_ERR_NODEV       = "No device(s) available"
  show BLADERF_ERR_UNSUPPORTED = "Operation not supported"
  show BLADERF_ERR_MISALIGNED  = "Misaligned flash access"
  show BLADERF_ERR_CHECKSUM    = "Invalid checksum"
  show BLADERF_ERR_NO_FILE     = "File not found"
  show BLADERF_ERR_UPDATE_FPGA = "An FPGA update is required"
  show BLADERF_ERR_UPDATE_FW   = "A firmware update is requied"
  show BLADERF_ERR_TIME_PAST   = "Requested timestamp is in the past"


newtype BladeRF a = BladeRF { unBladeRF :: ExceptT BladeRFError (StateT (Ptr C'bladerf) IO) a }
-- newtype BladeRF a = BladeRF { unBladeRF :: ExceptT BladeRFError IO a }
  deriving (Monad, MonadIO)

instance Functor BladeRF where
  {-# INLINE fmap #-}
  fmap f m = BladeRF (f <$> unBladeRF m)

instance Applicative BladeRF where
  {-# INLINE pure #-}
  pure  = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

runBladeRF m = evalStateT (runExceptT . unBladeRF $ m) nullPtr
-- XXX change nullPtr to invocate openBladeRF and move over to ReaderT instead of StateT

-- XXX dont rethrow but show the error message with a instance of strings..
bracket open close body = do
    BladeRF $ catchE (unBladeRF open)
     throwE
    BladeRF $ catchE (unBladeRF body)
     (\e -> do unBladeRF close ; throwE e)
    close

--
-- Open specified device using a device identifier string.
-- See bladerf_open_with_devinfo() if a device identifier string
-- is not readily available.
openBladeRF p = do
  dev <- liftIO (malloc :: IO (Ptr (Ptr C'bladerf)))
  ret <- liftIO $ c'bladerf_open dev p
  -- make into switch or something better?? for more detailed error report
  if ret /= 0 then do
    liftIO $ free dev
    BladeRF $ throwE BLADERF_ERR_NODEV
  else do
    pdev <- liftIO $ peek dev
    BladeRF $ lift (put pdev)

--
-- | Close device. Deallocates the memory allocated by openBladeRF when called.
closeBladeRF = (BladeRF $ lift get) >>= liftIO . c'bladerf_close
-- closeBladeRF = do dev <- BladeRF $ lift get ; liftIO $ c'bladerf_close dev


-- nullPtr is passed to openBladeRF as the "device string" whatever that is??
-- passing nullPtr appears to make a probe occur so just use that.
-- withBladeRF :: IO a -> BladeRF ()
withBladeRF stuff = runBladeRF $ bracket (openBladeRF nullPtr) closeBladeRF stuff
