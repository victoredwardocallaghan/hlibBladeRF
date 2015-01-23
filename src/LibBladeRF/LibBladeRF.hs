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

module LibBladeRF.LibBladeRF ( withBladeRF
                             , bladerfGetDevInfo
                             ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception

import Bindings.LibBladeRF

-- TODO: Provide error handling??
-- This is currently crap!
openBladeRF p = do
  dev <- malloc :: IO (Ptr (Ptr C'bladerf))
  ret <- c'bladerf_open dev p
  if ret /= 0 then do
    free dev
    putStrLn "Error: cant open device"
    return nullPtr
  else do
    pdev <- peek dev
    return pdev

closeBladeRF dev = c'bladerf_close dev

-- nullPtr is passed to openBladeRF as the "device string" whatever that is??
-- passing nullPtr appears to make a probe occur so just use that.
withBladeRF stuff = bracket (openBladeRF nullPtr) closeBladeRF stuff


bladerfGetDevInfo :: Ptr C'bladerf -> IO (String, [Char], Word8, Word8, CUInt)
bladerfGetDevInfo dev = do
  p <- malloc :: IO (Ptr C'bladerf_devinfo)
  c'bladerf_get_devinfo dev p
-- XXX ^ handle status return error with Maybe monad???
  brfv <- peek p
  -- XXX decode backend to string??
  let backend = show . c'bladerf_devinfo'backend $ brfv
  let serial = map castCCharToChar . c'bladerf_devinfo'serial $ brfv
  let usb_bus = c'bladerf_devinfo'usb_bus $ brfv
  let usb_addr = c'bladerf_devinfo'usb_addr $ brfv
  let inst = c'bladerf_devinfo'instance $ brfv
  free p
  return (backend, serial, usb_bus, usb_addr, inst)
