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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LibBladeRF.LibBladeRF ( withBladeRF
                             , bladerfGetDevInfo
                             , BladeRF(..)
                             ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Monad.Trans
-- import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Bindings.LibBladeRF

data BladeRFError = Success | BoardNotFound

newtype BladeRF a = BladeRF { unBladeRF :: ExceptT BladeRFError (StateT (Ptr C'bladerf) IO) a }
-- newtype BladeRF a = BladeRF { unBladeRF :: ExceptT BladeRFError IO a }
  deriving (Monad, MonadIO)

-- instance Applicative BladeRF where

runBladeRF = runExceptT . unBladeRF

bracket open close body = do
    BladeRF $ catchE (unBladeRF open)
     (\e -> throwE e)
    BladeRF $ catchE (unBladeRF body)
     (\e -> do unBladeRF close ; throwE e)
    close

openBladeRF p = do
  dev <- liftIO (malloc :: IO (Ptr (Ptr C'bladerf)))
  ret <- liftIO $ c'bladerf_open dev p
  if ret /= 0 then do
    liftIO $ free dev
    BladeRF $ throwE BoardNotFound
  else do
    pdev <- liftIO $ peek dev
    BladeRF $ lift (put pdev)
    return pdev

closeBladeRF = (BladeRF $ lift get) >>= liftIO . c'bladerf_close
-- closeBladeRF = do dev <- BladeRF $ lift get ; liftIO $ c'bladerf_close dev


-- nullPtr is passed to openBladeRF as the "device string" whatever that is??
-- passing nullPtr appears to make a probe occur so just use that.
-- withBladeRF :: IO a -> BladeRF ()
withBladeRF stuff = runBladeRF $ bracket (openBladeRF nullPtr) (closeBladeRF) (stuff)


-- bladerfGetDevInfo :: Ptr C'bladerf -> IO (String, [Char], Word8, Word8, CUInt)
bladerfGetDevInfo :: BladeRF (String, [Char], Word8, Word8, CUInt)
bladerfGetDevInfo = do
  p <- liftIO (malloc :: IO (Ptr C'bladerf_devinfo))
  dev <- BladeRF $ lift get
  liftIO $ c'bladerf_get_devinfo dev p
-- XXX ^ handle status return error with Maybe monad???
  brfv <- liftIO $ peek p
  -- XXX decode backend to string??
  let backend = show . c'bladerf_devinfo'backend $ brfv
  let serial = map castCCharToChar . c'bladerf_devinfo'serial $ brfv
  let usb_bus = c'bladerf_devinfo'usb_bus $ brfv
  let usb_addr = c'bladerf_devinfo'usb_addr $ brfv
  let inst = c'bladerf_devinfo'instance $ brfv
  liftIO $ free p
  return (backend, serial, usb_bus, usb_addr, inst)
