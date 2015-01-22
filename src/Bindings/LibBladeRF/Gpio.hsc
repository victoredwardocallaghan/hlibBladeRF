#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.Gpio where

#strict_import
import Bindings.LibBladeRF.Types


#ccall bladerf_config_gpio_read , Ptr (<bladerf>) -> Ptr (Word32) -> CInt
#ccall bladerf_config_gpio_write , Ptr (<bladerf>) -> Word32 -> CInt

#ccall bladerf_expansion_gpio_read , Ptr (<bladerf>) -> Ptr (Word32) -> CInt
#ccall bladerf_expansion_gpio_write , Ptr (<bladerf>) -> Word32 -> CInt

#ccall bladerf_expansion_gpio_dir_read , Ptr (<bladerf>) -> Ptr (Word32) -> CInt
#ccall bladerf_expansion_gpio_dir_write , Ptr (<bladerf>) -> Word32 -> CInt


-- #ccall bladerf_expansion_get_timestamp , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (Word64) -> CInt

-- #ccall bladerf_expansion_dac_write , Ptr (<bladerf>) -> Word16 -> CInt

#ccall bladerf_xb_spi_write , Ptr (<bladerf>) -> Word32 -> CInt

#ccall bladerf_calibrate_dc , Ptr (<bladerf>) -> <bladerf_cal_module> -> CInt
