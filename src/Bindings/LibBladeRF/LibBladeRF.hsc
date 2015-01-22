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

-- #ccall bladerf_set_usb_reset_on_open , CShort -> IO ()

#ccall bladerf_init_devinfo , Ptr (<bladerf_devinfo>) -> IO ()

#ccall bladerf_get_devinfo , Ptr (<bladerf>) -> Ptr (<bladerf_devinfo>) -> IO (CInt)

#ccall bladerf_get_devinfo_from_str , Ptr (CChar) -> Ptr (<bladerf_devinfo>) -> IO (CInt)

#ccall bladerf_devinfo_matches , Ptr (<bladerf_devinfo>) -> Ptr (<bladerf_devinfo>) -> IO (CShort)

#ccall bladerf_devstr_matches , Ptr (CChar) -> Ptr (<bladerf_devinfo>) -> IO (CShort)

#ccall bladerf_backend_str , <bladerf_backend> -> IO (Ptr (CChar))



-- #ccall bladerf_enable_module , Ptr (<bladerf>) -> <bladerf_module> -> CShort -> CInt

#ccall bladerf_set_loopback , Ptr (<bladerf>) -> <bladerf_loopback> -> CInt
#ccall bladerf_get_loopback , Ptr (<bladerf>) -> Ptr (<bladerf_loopback>) -> CInt

-- #ccall bladerf_set_sample_rate , Ptr (<bladerf>) -> <bladerf_module> -> CUInt -> Ptr (CUInt) -> CInt

-- #ccall bladerf_set_sample_rate , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (<bladerf_rational_rate>) -> Ptr (<bladerf_rational_rate>) -> CInt

#ccall bladerf_set_sampling , Ptr (<bladerf>) -> <bladerf_sampling> -> CInt
#ccall bladerf_get_sampling , Ptr (<bladerf>) -> Ptr (<bladerf_sampling>) -> CInt

#ccall bladerf_get_sample_rate , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (CUInt) -> CInt
#ccall bladerf_get_rational_sample_rate , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (<bladerf_rational_rate>) -> CInt

#ccall bladerf_set_correction , Ptr (<bladerf>) -><bladerf_module> -> <bladerf_correction> -> Word16 -> CInt
#ccall bladerf_get_correction , Ptr (<bladerf>) -><bladerf_module> -> <bladerf_correction> -> Ptr (Word16) -> CInt


#ccall bladerf_set_txvga2 , Ptr (<bladerf>) -> CInt -> CInt
#ccall bladerf_get_txvga2 , Ptr (<bladerf>) -> Ptr (CInt) -> CInt

#ccall bladerf_set_txvga1 , Ptr (<bladerf>) -> CInt -> CInt
#ccall bladerf_get_txvga1 , Ptr (<bladerf>) -> Ptr (CInt) -> CInt

-- #ccall bladerf_set_tx_gain , Ptr (<bladerf>) -> CInt -> CInt

#ccall bladerf_set_lna_gain , Ptr (<bladerf>) -> <bladerf_lna_gain> -> CInt
#ccall bladerf_get_lna_gain , Ptr (<bladerf>) -> Ptr (<bladerf_lna_gain>) -> CInt

#ccall bladerf_set_rxvga1 , Ptr (<bladerf>) -> CInt -> CInt
#ccall bladerf_get_rxvga1 , Ptr (<bladerf>) -> Ptr (CInt) -> CInt

#ccall bladerf_set_rxvga2 , Ptr (<bladerf>) -> CInt -> CInt
#ccall bladerf_get_rxvga2 , Ptr (<bladerf>) -> Ptr (CInt) -> CInt

#ccall bladerf_set_gain , Ptr (<bladerf>) -> <bladerf_module> -> CInt -> CInt

#ccall bladerf_set_bandwidth , Ptr (<bladerf>) -> <bladerf_module> -> CUInt -> Ptr (CUInt) -> CInt
#ccall bladerf_get_bandwidth , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (CUInt) -> CInt

#ccall bladerf_set_lpf_mode , Ptr (<bladerf>) -> <bladerf_module> -> <bladerf_lpf_mode> -> CInt
#ccall bladerf_get_lpf_mode , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (<bladerf_lpf_mode>) -> CInt

#ccall bladerf_select_band , Ptr (<bladerf>) -> <bladerf_module> -> CUInt -> CInt

#ccall bladerf_set_frequency , Ptr (<bladerf>) -> <bladerf_module> -> CUInt -> CInt
#ccall bladerf_get_frequency , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (CUInt) -> CInt

#ccall bladerf_expansion_attach , Ptr (<bladerf>) -> <bladerf_xb> -> CInt

#ccall bladerf_expansion_get_attached , Ptr (<bladerf>) -> Ptr (<bladerf_xb>) -> CInt

#ccall bladerf_xb200_set_filterbank , Ptr (<bladerf>) -> <bladerf_module> -> <bladerf_xb200_filter> -> CInt
#ccall bladerf_xb200_get_filterbank , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (<bladerf_xb200_filter>) -> CInt

#ccall bladerf_xb200_set_path , Ptr (<bladerf>) -> <bladerf_module> -> <bladerf_xb200_path> -> CInt
#ccall bladerf_xb200_get_path , Ptr (<bladerf>) -> <bladerf_module> -> Ptr (<bladerf_xb200_path>) -> CInt
