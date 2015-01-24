#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.Utils where

#strict_import
import Bindings.LibBladeRF.Types

#ccall bladerf_get_serial , Ptr (<bladerf>) -> CString -> CInt

#ccall bladerf_get_vctcxo_trim , Ptr (<bladerf>) -> Ptr (Word16) -> CInt


#ccall bladerf_get_fpga_size , Ptr (<bladerf>) -> Ptr (<bladerf_fpga_size>) -> CInt
#ccall bladerf_is_fpga_configured , Ptr (<bladerf>) -> IO (CInt)
#ccall bladerf_fpga_version , Ptr (<bladerf>) -> Ptr (<bladerf_version>) -> IO (CInt)


#ccall bladerf_device_speed , Ptr (<bladerf>) -> IO (<bladerf_dev_speed>)

#ccall bladerf_fw_version , Ptr (<bladerf>) -> Ptr (<bladerf_version>) -> IO (CInt)

-- #ccall bladerf_stderr , CInt -> Ptr (CChar)

#ccall bladerf_version , Ptr (<bladerf_version>) -> IO ()

#ccall bladerf_log_set_verbosity , <bladerf_log_level> -> IO ()
