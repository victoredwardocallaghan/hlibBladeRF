#include <bindings.dsl.h>
#include <libbladeRF.h>

module Bindings.LibBladeRF.LibBladeRF where

#strict_import

import Bindings.LibBladeRF.Types

#ccall bladerf_get_device_list , Ptr (Ptr <bladerf_devinfo>) -> IO <Int>
