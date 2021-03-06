#include <bindings.dsl.h>
#include <libbladeRF.h>

{-# OPTIONS_HADDOCK hide #-}

module Bindings.LibBladeRF.Gain where

#strict_import
import Bindings.LibBladeRF.Types


#ccall bladerf_set_txvga2 , Ptr (<bladerf>) -> CInt -> IO CInt
#ccall bladerf_get_txvga2 , Ptr (<bladerf>) -> Ptr (CInt) -> IO CInt

#ccall bladerf_set_txvga1 , Ptr (<bladerf>) -> CInt -> IO CInt
#ccall bladerf_get_txvga1 , Ptr (<bladerf>) -> Ptr (CInt) -> IO CInt

#ccall bladerf_set_lna_gain , Ptr (<bladerf>) -> <bladerf_lna_gain> -> IO CInt
#ccall bladerf_get_lna_gain , Ptr (<bladerf>) -> Ptr (<bladerf_lna_gain>) -> IO CInt

#ccall bladerf_set_rxvga1 , Ptr (<bladerf>) -> CInt -> IO CInt
#ccall bladerf_get_rxvga1 , Ptr (<bladerf>) -> Ptr (CInt) -> IO CInt

#ccall bladerf_set_rxvga2 , Ptr (<bladerf>) -> CInt -> IO CInt
#ccall bladerf_get_rxvga2 , Ptr (<bladerf>) -> Ptr (CInt) -> IO CInt

#ccall bladerf_set_gain , Ptr (<bladerf>) -> <bladerf_module> -> CInt -> IO CInt
