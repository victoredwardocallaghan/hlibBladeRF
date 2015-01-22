#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.Si5338 where

#strict_import
import Bindings.LibBladeRF.Types


#ccall bladerf_si5338_read , Ptr (<bladerf>) -> Word8 -> Ptr (Word8) -> CInt
#ccall bladerf_si5338_write , Ptr (<bladerf>) -> Word8 -> Word8 -> CInt

-- #ccall bladerf_si5338_set_tx_freq , Ptr (<bladerf>) -> CUInt -> CInt
-- #ccall bladerf_si5338_set_rx_freq , Ptr (<bladerf>) -> CUInt -> CInt
