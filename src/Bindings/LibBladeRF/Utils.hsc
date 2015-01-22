#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.Utils where

#strict_import
import Bindings.LibBladeRF.Types


-- #ccall bladerf_stderr , CInt -> Ptr (CChar)

#ccall bladerf_version , Ptr (<bladerf_version>) -> IO ()

#ccall bladerf_log_set_verbosity , <bladerf_log_level> -> IO ()
