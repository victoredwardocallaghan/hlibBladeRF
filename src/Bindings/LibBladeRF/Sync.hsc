#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.Sync where

#strict_import
import Bindings.LibBladeRF.Types


#ccall bladerf_sync_config , Ptr (<bladerf>) -> <bladerf_module> -> <bladerf_format> -> CUInt -> CUInt -> CUInt -> CUInt -> IO (CInt)

#ccall bladerf_sync_tx , Ptr (<bladerf>) -> Ptr a -> CUInt -> Ptr (<bladerf_metadata>) -> CUInt -> IO (CInt)
#ccall bladerf_sync_rx , Ptr (<bladerf>) -> Ptr a -> CUInt -> Ptr (<bladerf_metadata>) -> CUInt -> IO (CInt)
