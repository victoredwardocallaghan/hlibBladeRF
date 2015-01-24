{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates supporting monadic functions.
-}

module LibBladeRF.Misc ( printBladeRF
                       ) where

import Control.Monad.IO.Class

import LibBladeRF.LibBladeRF


--
-- | Allow printing from within the BladeRF monadic structure.
printBladeRF :: String -> BladeRF ()
printBladeRF = liftIO . putStrLn
