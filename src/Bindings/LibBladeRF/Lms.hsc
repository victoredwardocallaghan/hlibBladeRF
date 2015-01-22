#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.Lms where

#strict_import
import Bindings.LibBladeRF.Types


#ccall bladerf_lms_read , Ptr (<bladerf>) -> Word8 -> Ptr (Word8) -> CInt
#ccall bladerf_lms_write , Ptr (<bladerf>) -> Word8 -> Word8 -> CInt

#ccall bladerf_lms_set_dc_cals , Ptr (<bladerf>) -> Ptr (<bladerf_lms_dc_cals>) -> CInt
#ccall bladerf_lms_get_dc_cals , Ptr (<bladerf>) -> Ptr (<bladerf_lms_dc_cals>) -> CInt
