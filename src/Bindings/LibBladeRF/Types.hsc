#include <bindings.dsl.h>
#include <libbladeRF.h>

{-# OPTIONS_HADDOCK hide #-}

module Bindings.LibBladeRF.Types where

#strict_import
import Foreign.Storable


#num BLADERF_ERR_UNEXPECTED
#num BLADERF_ERR_RANGE
#num BLADERF_ERR_INVAL
#num BLADERF_ERR_MEM
#num BLADERF_ERR_IO
#num BLADERF_ERR_TIMEOUT
#num BLADERF_ERR_NODEV
#num BLADERF_ERR_UNSUPPORTED
#num BLADERF_ERR_MISALIGNED
#num BLADERF_ERR_CHECKSUM
#num BLADERF_ERR_NO_FILE
#num BLADERF_ERR_UPDATE_FPGA
#num BLADERF_ERR_UPDATE_FW
#num BLADERF_ERR_TIME_PAST

#opaque_t bladerf

-- #integral_t enum
#integral_t bladerf_backend
#num BLADERF_BACKEND_ANY
#num BLADERF_BACKEND_LINUX
#num BLADERF_BACKEND_LIBUSB
#num BLADERF_BACKEND_CYPRESS
#num BLADERF_BACKEND_DUMMY

-- #integral_t enum
#integral_t bladerf_dev_speed
#num BLADERF_DEVICE_SPEED_UNKNOWN
#num BLADERF_DEVICE_SPEED_HIGH
#num BLADERF_DEVICE_SPEED_SUPER


#num BLADERF_SERIAL_LENGTH


#starttype struct bladerf_devinfo
#field backend , <bladerf_backend>
#array_field serial , CChar
#field usb_bus , Word8
#field usb_addr , Word8
#field instance , CUInt
#stoptype


#num BLADERF_RXVGA1_GAIN_MIN
#num BLADERF_RXVGA1_GAIN_MAX
#num BLADERF_RXVGA2_GAIN_MIN
#num BLADERF_RXVGA2_GAIN_MAX
#num BLADERF_TXVGA1_GAIN_MIN
#num BLADERF_TXVGA1_GAIN_MAX
#num BLADERF_TXVGA2_GAIN_MIN
#num BLADERF_TXVGA2_GAIN_MAX
#num BLADERF_SAMPLERATE_MIN
#num BLADERF_SAMPLERATE_REC_MAX
#num BLADERF_BANDWIDTH_MIN
#num BLADERF_BANDWIDTH_MAX
#num BLADERF_FREQUENCY_MIN
#num BLADERF_FREQUENCY_MIN_XB200
#num BLADERF_FREQUENCY_MAX


-- #integral_t enum
#integral_t bladerf_loopback
#num BLADERF_LB_FIRMWARE
#num BLADERF_LB_BB_TXLPF_RXVGA2
#num BLADERF_LB_BB_TXVGA1_RXVGA2
#num BLADERF_LB_BB_TXLPF_RXLPF
#num BLADERF_LB_BB_TXVGA1_RXLPF
#num BLADERF_LB_RF_LNA1
#num BLADERF_LB_RF_LNA2
#num BLADERF_LB_RF_LNA3
#num BLADERF_LB_NONE


#starttype struct bladerf_rational_rate
#field integer , Word64
#field num , Word64
#field den , Word64
#stoptype


-- #integral_t enum
#integral_t bladerf_sampling
#num BLADERF_SAMPLING_UNKNOWN
#num BLADERF_SAMPLING_INTERNAL
#num BLADERF_SAMPLING_EXTERNAL


-- #integral_t enum
#integral_t bladerf_lna_gain
#num BLADERF_LNA_GAIN_UNKNOWN
#num BLADERF_LNA_GAIN_BYPASS
#num BLADERF_LNA_GAIN_MID
#num BLADERF_LNA_GAIN_MAX


#num BLADERF_LNA_GAIN_MID_DB
#num BLADERF_LNA_GAIN_MAX_DB


-- #integral_t enum
#integral_t bladerf_lpf_mode
#num BLADERF_LPF_NORMAL
#num BLADERF_LPF_BYPASSED
#num BLADERF_LPF_DISABLED


-- #integral_t enum
#integral_t bladerf_module
#num BLADERF_MODULE_RX
#num BLADERF_MODULE_TX


-- #integral_t enum
#integral_t bladerf_xb
#num BLADERF_XB_NONE
#num BLADERF_XB_100
#num BLADERF_XB_200


-- #integral_t enum
#integral_t bladerf_xb200_filter
#num BLADERF_XB200_50M
#num BLADERF_XB200_144M
#num BLADERF_XB200_222M
#num BLADERF_XB200_CUSTOM
#num BLADERF_XB200_AUTO_1DB
#num BLADERF_XB200_AUTO_3DB


-- #integral_t enum
#integral_t bladerf_xb200_path
#num BLADERF_XB200_BYPASS
#num BLADERF_XB200_MIX


-- #integral_t enum
#integral_t bladerf_cal_module
#num BLADERF_DC_CAL_LPF_TUNING
#num BLADERF_DC_CAL_TX_LPF
#num BLADERF_DC_CAL_RX_LPF
#num BLADERF_DC_CAL_RXVGA2


-- #integral_t enum
#integral_t bladerf_correction
#num BLADERF_CORR_LMS_DCOFF_I
#num BLADERF_CORR_LMS_DCOFF_Q
#num BLADERF_CORR_FPGA_PHASE
#num BLADERF_CORR_FPGA_GAIN


-- #integral_t enum
#integral_t bladerf_format
#num BLADERF_FORMAT_SC16_Q11
#num BLADERF_FORMAT_SC16_Q11_META


#num BLADERF_META_STATUS_OVERRUN

#num BLADERF_META_STATUS_UNDERRUN

#num BLADERF_META_FLAG_TX_BURST_START

#num BLADERF_META_FLAG_TX_BURST_END

#num BLADERF_META_FLAG_TX_NOW

#num BLADERF_META_FLAG_RX_NOW


#starttype struct bladerf_metadata
#field timestamp , Word64
#field flags , Word32
#field status , Word32
#field actual_count , CInt
#array_field reserved , Word8
#stoptype


-- #num BLADERF_STREAM_SHUTDOWN (NULL)

-- #num BLADERF_STREAM_NO_DATA  ((void*)(-1))


#callback_t bladerf_stream_cb , Ptr() -> Ptr() -> Ptr() -> Ptr() -> CSize -> Ptr() -> IO()


#starttype struct bladerf_version
#field major , Word16
#field minor , Word16
#field patch , Word16
#field describe , CString
#stoptype


-- #integral_t enum
#integral_t bladerf_fpga_size
#num BLADERF_FPGA_UNKNOWN
#num BLADERF_FPGA_40KLE
#num BLADERF_FPGA_115KLE


-- #integral_t enum
#integral_t bladerf_log_level
#num BLADERF_LOG_LEVEL_VERBOSE
#num BLADERF_LOG_LEVEL_DEBUG
#num BLADERF_LOG_LEVEL_INFO
#num BLADERF_LOG_LEVEL_WARNING
#num BLADERF_LOG_LEVEL_ERROR
#num BLADERF_LOG_LEVEL_CRITICAL
#num BLADERF_LOG_LEVEL_SILENT


-- #integral_t enum
#integral_t bladerf_image_type
#num BLADERF_IMAGE_TYPE_INVALID
#num BLADERF_IMAGE_TYPE_RAW
#num BLADERF_IMAGE_TYPE_FIRMWARE
#num BLADERF_IMAGE_TYPE_FPGA_40KLE
#num BLADERF_IMAGE_TYPE_FPGA_115KLE
#num BLADERF_IMAGE_TYPE_CALIBRATION
#num BLADERF_IMAGE_TYPE_RX_DC_CAL
#num BLADERF_IMAGE_TYPE_TX_DC_CAL
#num BLADERF_IMAGE_TYPE_RX_IQ_CAL
#num BLADERF_IMAGE_TYPE_TX_IQ_CAL


#num BLADERF_IMAGE_MAGIC_LEN

#num BLADERF_IMAGE_CHECKSUM_LEN

#num BLADERF_IMAGE_RESERVED_LEN


#starttype struct bladerf_image
#array_field magic , CChar
#array_field checksum , Word8
#field version , <bladerf_version>
#field timestamp , Word64
#array_field serial , CChar
#array_field reserved , CChar
#field type , <bladerf_image_type>
#field address , Word32
#field length , Word32
#field data , Ptr (Word8)
#stoptype


#starttype struct bladerf_lms_dc_cals
#field lpf_tuning , Word16
#field tx_lpf_i , Word16
#field tx_lpf_q , Word16
#field rx_lpf_i , Word16
#field rx_lpf_q , Word16
#field dc_ref , Word16
#field rxvga2a_i , Word16
#field rxvga2a_q , Word16
#field rxvga2b_i , Word16
#field rxvga2b_q , Word16
#stoptype


#num BLADERF_GPIO_LMS_RX_ENABLE
#num BLADERF_GPIO_LMS_TX_ENABLE
#num BLADERF_GPIO_TX_LB_ENABLE
#num BLADERF_GPIO_TX_HB_ENABLE
#num BLADERF_GPIO_COUNTER_ENABLE
#num BLADERF_GPIO_RX_LB_ENABLE
#num BLADERF_GPIO_RX_HB_ENABLE
#num BLADERF_GPIO_FEATURE_SMALL_DMA_XFER
#num BLADERF_GPIO_TIMESTAMP
#num BLADERF_GPIO_TIMESTAMP_DIV2


#num BLADERF_FLASH_TOTAL_SIZE
#num BLADERF_FLASH_PAGE_SIZE
#num BLADERF_FLASH_EB_SIZE
#num BLADERF_FLASH_NUM_PAGES
#num BLADERF_FLASH_NUM_EBS
-- #num BLADERF_FLASH_TO_PAGES
-- #num BLADERF_FLASH_TO_EB
#num BLADERF_FLASH_ADDR_FIRMWARE
-- #num BLADERF_FLASH_PAGE_FIRMWARE
-- #num BLADERF_FLASH_EB_FIRMWARE
#num BLADERF_FLASH_BYTE_LEN_FIRMWARE
-- #num BLADERF_FLASH_PAGE_LEN_FIRMWARE
-- #num BLADERF_FLASH_EB_LEN_FIRMWARE
#num BLADERF_FLASH_ADDR_CAL
-- #num BLADERF_FLASH_PAGE_CAL
-- #num BLADERF_FLASH_EB_CAL
#num BLADERF_FLASH_BYTE_LEN_CAL
-- #num BLADERF_FLASH_PAGE_LEN_CAL
#num BLADERF_FLASH_EB_LEN_CAL


#num BLADERF_FLASH_ADDR_FPGA
-- #num BLADERF_FLASH_PAGE_FPGA
-- #num BLADERF_FLASH_EB_FPGA
#num BLADERF_FLASH_BYTE_LEN_FPGA
-- #num BLADERF_FLASH_EB_LEN_FPGA
