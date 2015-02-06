{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates types libbladeRF library functions.
-}

module LibBladeRF.Types ( BladeRFVersion(..)
                        , BladeRFDeviceInfo(..)
                        ) where


import Foreign
import Foreign.C.Types
import Foreign.C.String

--
-- ..
data BladeRFVersion = BladeRFVersion { major :: Word16
                                     , minor :: Word16
                                     , patch :: Word16
                                     , descr :: String
                                     } deriving (Eq, Show)

--
-- ..
data BladeRFDeviceInfo = BladeRFDeviceInfo { backend :: String
                                           , serial  :: String
                                           , usbBus  :: Word8
                                           , usbAddr :: Word8
                                           , inst    :: CUInt
                                           } deriving (Eq, Show)
