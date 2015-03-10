{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module deals with GPIO configuration handling.

  The following example illustrates the setup of GPIO's
  for timestamping in the usual, read, modify, write
  sequence:

  @
   gpios <- bladeRFConfigGPIORead dev
   case gpios of
     Left e -> throwIO e
     Right gpios -> do
       putStrLn \"========= GPIO Dump =========\"
       mapM_ putStrLn $ debugBladeRFGPIOFlags gpios
       bladeRFConfigGPIOWrite dev $ GPIO_TIMESTAMP : gpios
  @
-}

{-# LANGUAGE Trustworthy #-}
module LibBladeRF.Gpio ( BladeRFGPIOFlags(..)
                       , debugBladeRFGPIOFlags
                       , bladeRFConfigGPIORead
                       , bladeRFConfigGPIOWrite
                       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Maybe
import Data.Tuple

import Bindings.LibBladeRF
import LibBladeRF.LibBladeRF
import LibBladeRF.Types


-- | BladeRF GPIO Flag Type.
data BladeRFGPIOFlags = GPIO_LMS_RX_ENABLE          -- ^ Enable LMS receive.
                                                    --   N.B. This bit is set/cleared by 'LibBladeRF.Utils.bladeRFEnableModule'
                      | GPIO_LMS_TX_ENABLE          -- ^ Enable LMS transmit.
                                                    --   N.B. This bit is set/cleared by 'LibBladeRF.Utils.bladeRFEnableModule'
                      | GPIO_TX_LB_ENABLE           -- ^ Switch to use TX low band (300MHz - 1.5GHz).
                                                    --   N.B. This is set using 'LibBladeRF.Frequency.bladeRFSetFrequency'.
                      | GPIO_TX_HB_ENABLE           -- ^ Switch to use TX high band (1.5GHz - 3.8GHz).
                                                    --   N.B. This is set using 'LibBladeRF.Frequency.bladeRFSetFrequency'.
                      | GPIO_COUNTER_ENABLE         -- ^ Counter mode enable.
                                                    --
                                                    --   Setting this bit to 1 instructs the FPGA to replace the (I, Q) pair in
                                                    --   sample data with an incrementing, little-endian, 32-bit counter value. A
                                                    --   0 in bit specifies that sample data should be sent (as normally done).
                                                    --
                                                    --   This feature is useful when debugging issues involving dropped samples.
                      | GPIO_RX_LB_ENABLE           -- ^ Switch to use RX low band (300M - 1.5GHz).
                                                    --   N.B. This is set using 'LibBladeRF.Frequency.bladeRFSetFrequency'.
                      | GPIO_RX_HB_ENABLE           -- ^ Switch to use RX high band (1.5GHz - 3.8GHz).
                                                    --   N.B. This is set using 'LibBladeRF.Frequency.bladeRFSetFrequency'.
                      | GPIO_FEATURE_SMALL_DMA_XFER -- ^ This GPIO bit configures the FPGA to use smaller DMA
                                                    --   transfers (256 cycles instead of 512). This is required
                                                    --   when the device is not connected at Super Speed (i.e., when
                                                    --   it is connected at High Speed).
                                                    --
                                                    --   However, the caller need not set this in 'bladeRFConfigGPIOWrite' calls.
                                                    --   The library will set this as needed; callers generally
                                                    --   do not need to be concerned with setting/clearing this bit.
                      | GPIO_TIMESTAMP              -- ^ Enable-bit for timestamp counter in the FPGA.
                      | GPIO_TIMESTAMP_DIV2         -- ^ Timestamp 2x divider control.
                                                    --
                                                    --   By default (value = 0), the sample counter is incremented with I and Q,
                                                    --   yielding two counts per sample.
                                                    --
                                                    --   Set this bit to 1 to enable a 2x timestamp divider, effectively
                                                    --   achieving 1 timestamp count per sample.
                      deriving (Eq)

instance Enum BladeRFGPIOFlags where
  fromEnum = fromJust . flip lookup gpios
  toEnum   = fromJust . flip lookup (map swap gpios)

gpios = [ (GPIO_LMS_RX_ENABLE, c'BLADERF_GPIO_LMS_RX_ENABLE)
        , (GPIO_LMS_TX_ENABLE, c'BLADERF_GPIO_LMS_TX_ENABLE)
        , (GPIO_TX_LB_ENABLE, c'BLADERF_GPIO_TX_LB_ENABLE)
        , (GPIO_TX_HB_ENABLE, c'BLADERF_GPIO_TX_HB_ENABLE)
        , (GPIO_COUNTER_ENABLE, c'BLADERF_GPIO_COUNTER_ENABLE)
        , (GPIO_RX_LB_ENABLE, c'BLADERF_GPIO_RX_LB_ENABLE)
        , (GPIO_RX_HB_ENABLE, c'BLADERF_GPIO_RX_HB_ENABLE)
        , (GPIO_FEATURE_SMALL_DMA_XFER, c'BLADERF_GPIO_FEATURE_SMALL_DMA_XFER)
        , (GPIO_TIMESTAMP, c'BLADERF_GPIO_TIMESTAMP)
        , (GPIO_TIMESTAMP_DIV2, c'BLADERF_GPIO_TIMESTAMP_DIV2)
        ]

-- | Useful helper function to decode GPIO flags into strings
--
--   Example:
--
--   @
--    gpios <- bladeRFConfigGPIORead dev
--    case gpios of
--      Left e -> throwIO e
--      Right g -> mapM_ putStrLn $ debugBladeRFGPIOFlags g
--   @
--
debugBladeRFGPIOFlags :: [BladeRFGPIOFlags] -> [String]
debugBladeRFGPIOFlags  = map fts
  where fts f | f == GPIO_LMS_RX_ENABLE          = " [GPIO flag set] " ++ "Enable LMS receive."
              | f == GPIO_LMS_TX_ENABLE          = " [GPIO flag set] " ++ "Enable LMS transmit."
              | f == GPIO_TX_LB_ENABLE           = " [GPIO flag set] " ++ "Switch to use TX low band (300MHz - 1.5GHz)."
              | f == GPIO_TX_HB_ENABLE           = " [GPIO flag set] " ++ "Switch to use TX high band (1.5GHz - 3.8GHz)."
              | f == GPIO_COUNTER_ENABLE         = " [GPIO flag set] " ++ "Counter mode enable."
              | f == GPIO_RX_LB_ENABLE           = " [GPIO flag set] " ++ "Switch to use RX low band (300M - 1.5GHz)."
              | f == GPIO_RX_HB_ENABLE           = " [GPIO flag set] " ++ "Switch to use RX high band (1.5GHz - 3.8GHz)."
              | f == GPIO_FEATURE_SMALL_DMA_XFER = " [GPIO flag set] " ++ "Configures the FPGA to use smaller DMA transfers."
              | f == GPIO_TIMESTAMP              = " [GPIO flag set] " ++ "Enable-bit for timestamp counter in the FPGA."
              | f == GPIO_TIMESTAMP_DIV2         = " [GPIO flag set] " ++ "Timestamp 2x divider control."

-- | Read a configuration GPIO register.
bladeRFConfigGPIORead :: DeviceHandle                              -- ^ Device handle
                      -> IO (BladeRFReturnType [BladeRFGPIOFlags]) -- ^ Read data
bladeRFConfigGPIORead dev = alloca $ \pv -> do
  ret <- c'bladerf_config_gpio_read (unDeviceHandle dev) pv
  if ret < 0 then (return . Left . toEnum . fromIntegral) ret -- C ret code to typed error
  else do gpior <- peek pv
          (return . Right . wordToFlags . fromIntegral) gpior
  where wordToFlags w = [e | (e, bit) <- gpios, bit .&. w /= 0]

-- | Write a configuration GPIO register.
--
-- Callers should be sure to perform a read-modify-write sequence to avoid
-- accidentally clearing other GPIO bits that may be set by the library internally.
bladeRFConfigGPIOWrite :: DeviceHandle       -- ^ Device handle
                       -> [BladeRFGPIOFlags] -- ^ Data to write to GPIO register
                       -> IO (BladeRFReturnType ())
bladeRFConfigGPIOWrite dev v = do
  ret <- c'bladerf_config_gpio_write (unDeviceHandle dev) value
  return $ bladeRFErrorTy ret
  where
    value = foldr1 (.|.) flags
    flags = map (fromIntegral . fromEnum) v
