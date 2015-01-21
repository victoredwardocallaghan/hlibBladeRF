#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.LibBladeRF where

#strict_import
import Bindings.LibBladeRF.Types


#ccall bladerf_get_device_list , Ptr (Ptr <bladerf_devinfo>) -> IO (CInt)

#ccall bladerf_free_device_list , Ptr (<bladerf_devinfo>) -> IO ()


#ccall bladerf_open_with_devinfo , Ptr (Ptr <bladerf>) -> Ptr (<bladerf_devinfo>) -> IO (CInt)


#ccall bladerf_open , Ptr (Ptr <bladerf>) -> Ptr (CChar) -> IO (CInt)

#ccall bladerf_close , Ptr (<bladerf>) -> IO ()

#ccall bladerf_set_usb_reset_on_open , CShort -> IO ()

#ccall bladerf_init_devinfo , Ptr (<bladerf_devinfo>) -> IO ()

#ccall bladerf_get_devinfo , Ptr (<bladerf>) -> Ptr (<bladerf_devinfo>) -> IO (CInt)

#ccall bladerf_get_devinfo_from_str , Ptr (CChar) -> Ptr (<bladerf_devinfo>) -> IO (CInt)

#ccall bladerf_devinfo_matches , Ptr (<bladerf_devinfo>) -> Ptr (<bladerf_devinfo>) -> IO (CShort)

#ccall bladerf_devstr_matches , Ptr (CChar) -> Ptr (<bladerf_devinfo>) -> IO (CShort)

#ccall bladerf_backend_str , <bladerf_backend> -> IO (Ptr (CChar))
