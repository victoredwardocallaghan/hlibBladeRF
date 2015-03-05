{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates types libbladeRF library functions.
-}

{-# LANGUAGE Trustworthy, DeriveGeneric #-}

module LibBladeRF.Types ( BladeRFVersion(..)
                        , BladeRFDeviceInfo(..)
                        , BladeRFFPGASize(..)
                        , BladeRFBackend(..)
                        , BladeRFRationalRate(..)
                        , BladeRFModule(..)
                        , BladeRFFormat(..)
                        , BladeRFLNAGain(..)
                        , BladeRFVGAGainBounds(..)
                        , BladeRFCorrection(..)
                        , BladeRFSpeed(..)
                        , BladeRFMetadata(..)
--                        needs ghc >= 7.8
--                        , bladeRFMetadataToCBladeRFMetadata
--                        , bladeRFMetadataFromCBladeRFMetadata
                        , BladeRFLoopback(..)
                        ) where


import Bindings.LibBladeRF

import Foreign.C.Types
import Data.Word
import Data.Maybe
import Data.Tuple
--import Data.Coerce
--import GHC.Generics

-- | Version structure for FPGA, firmware, libbladeRF, and associated utilities.
data BladeRFVersion = BladeRFVersion { major :: Word16 -- ^ Major version
                                     , minor :: Word16 -- ^ Minor version
                                     , patch :: Word16 -- ^ Patch version
                                     , descr :: String -- ^ Version string with any additional suffix information.
                                     } deriving (Eq)

instance Show BladeRFVersion where
  show a = show (major a) ++ "." ++
           show (minor a) ++ "." ++
           show (patch a) ++ " (" ++
                descr a ++ ")"

-- | Information about a bladeRF attached to the system.
data BladeRFDeviceInfo = BladeRFDeviceInfo { backend :: BladeRFBackend -- ^ Backend to use when connecting to device
                                           , serial  :: String         -- ^ Device serial number string
                                           , usbBus  :: Word8          -- ^ Bus number device is attached to
                                           , usbAddr :: Word8          -- ^ Device address on bus
                                           , inst    :: CUInt          -- ^ Device instance or ID
                                           } deriving (Eq)

instance Show BladeRFDeviceInfo where
  show a = " Backend : " ++ show (backend a) ++ "\n" ++
           " Serial #: " ++ serial a ++ "\n" ++
           " USB bus: " ++ show (usbBus a) ++ "\n" ++
           " USB address: " ++ show (usbAddr a) ++ "\n" ++
           " Instance: " ++ show (inst a)

-- | FPGA device variant (size).
data BladeRFFPGASize = FPGA_UNKNOWN -- ^ Unable to determine FPGA variant
                     | FPGA_40KLE   -- ^ 40 kLE FPGA
                     | FPGA_115KLE  -- ^ 115 kLE FPGA
                     deriving (Eq)

instance Show BladeRFFPGASize where
  show FPGA_UNKNOWN = "Unable to determine FPGA variant"
  show FPGA_40KLE   = "40 kLE FPGA"
  show FPGA_115KLE  = "115 kLE FPGA"

instance Enum BladeRFFPGASize where
  fromEnum = fromJust . flip lookup sizes
  toEnum   = fromJust . flip lookup (map swap sizes)

sizes = [ (FPGA_UNKNOWN, c'BLADERF_FPGA_UNKNOWN)
        , (FPGA_40KLE, c'BLADERF_FPGA_40KLE)
        , (FPGA_115KLE, c'BLADERF_FPGA_115KLE)
        ]

-- | Backend by which the host communicates with the device.
data BladeRFBackend = BACKEND_ANY     -- ^ Don't Care, use any available backend
                    | BACKEND_LINUX   -- ^ Linux kernel driver
                    | BACKEND_LIBUSB  -- ^ libusb
                    | BACKEND_CYPRESS -- ^ CyAPI
                    | BACKEND_DUMMY   -- ^ Dummy used for development purposes
                    deriving (Eq)

instance Show BladeRFBackend where
  show BACKEND_ANY     = "Don't Care, use any available backend"
  show BACKEND_LINUX   = "Linux kernel driver"
  show BACKEND_LIBUSB  = "libusb"
  show BACKEND_CYPRESS = "CyAPI"
  show BACKEND_DUMMY   = "Dummy used for development purposes"

instance Enum BladeRFBackend where
  fromEnum = fromJust . flip lookup backends
  toEnum   = fromJust . flip lookup (map swap backends)

backends = [ (BACKEND_ANY, c'BLADERF_BACKEND_ANY)
           , (BACKEND_LINUX, c'BLADERF_BACKEND_LINUX)
           , (BACKEND_LIBUSB, c'BLADERF_BACKEND_LIBUSB)
           , (BACKEND_CYPRESS, c'BLADERF_BACKEND_CYPRESS)
           , (BACKEND_DUMMY, c'BLADERF_BACKEND_DUMMY)
           ]

-- | Rational sample rate representation.
data BladeRFRationalRate = BladeRFRationalRate { integer :: Word64 -- ^ Integer portion
                                               , num     :: Word64 -- ^ Numerator in fractional portion
                                               , den     :: Word64 -- ^ Denominator in fractional portion. This must be > 0.
                                               } deriving (Eq, Show)


-- | Module selection for those which have both RX and TX constituents.
data BladeRFModule = MODULE_RX -- ^ Receive Module
                   | MODULE_TX -- ^ Transmit Module
                   deriving (Eq)

instance Enum BladeRFModule where
  fromEnum = fromJust . flip lookup modules
  toEnum   = fromJust . flip lookup (map swap modules)

modules = [ (MODULE_RX, c'BLADERF_MODULE_RX)
          , (MODULE_TX, c'BLADERF_MODULE_TX)
          ]


-- | Sample format.
data BladeRFFormat
  {-| Signed, Complex 16-bit Q11. This is the native format of the DAC data.

      Values in the range [-2048, 2048) are used to represent [-1.0, 1.0).
      Note that the lower bound here is inclusive, and the upper bound is
      exclusive. Ensure that provided samples stay within [-2048, 2047].

      Samples consist of interleaved IQ value pairs, with I being the first
      value in the pair. Each value in the pair is a right-aligned,
      little-endian int16_t. The FPGA ensures that these values are
      sign-extended.

      When using this format the minimum required buffer size, in bytes, is:

      > buffer_size_min = [ 2 * num_samples * sizeof(int16_t) ]

      For example, to hold 2048 samples, a buffer must be at least 8192 bytes
      large.
  -}
  = FORMAT_SC16_Q11
  {-| This format is the same as the 'FORMAT_SC16_Q11' format, except the
    first 4 samples (16 bytes) in every block of 1024 samples are replaced
    with metadata, organized as follows, with all fields being little endian
    byte order:

    @
     0x00 [uint32_t:  Reserved]
     0x04 [uint64_t:  64-bit Timestamp]
     0x0c [uint32_t:  BLADERF_META_FLAG_* flags]
    @

    When using the 'LibBladeRF.Sync.bladeRFSyncRx' and
    'LibBladeRF.Sync.bladeRFSyncTx' actions, this detail is transparent to
    caller. These functions take care of packing/unpacking the metadata
    into/from the data, via the 'LibBladeRF.Types.BladeRFMetadata' structure.

    Currently, when using the asynchronous data transfer interface, the user
    is responsible for manually packing/unpacking this metadata into/from
    their sample data.
  -}
  | FORMAT_SC16_Q11_META
   deriving (Eq)


instance Enum BladeRFFormat where
  fromEnum = fromJust . flip lookup formats
  toEnum   = fromJust . flip lookup (map swap formats)

formats = [ (FORMAT_SC16_Q11, c'BLADERF_FORMAT_SC16_Q11)
          , (FORMAT_SC16_Q11_META, c'BLADERF_FORMAT_SC16_Q11_META)
          ]


-- | LNA gain options.
data BladeRFLNAGain = LNA_GAIN_UNKNOWN -- ^ Invalid LNA gain
                    | LNA_GAIN_BYPASS  -- ^ LNA bypassed - 0dB gain
                    | LNA_GAIN_MID     -- ^ LNA Mid Gain (MAX-6dB)
                    | LNA_GAIN_MAX     -- ^ LNA Max Gain
                    deriving (Eq)

instance Enum BladeRFLNAGain where
  fromEnum = fromJust . flip lookup lgains
  toEnum   = fromJust . flip lookup (map swap lgains)

lgains = [ (LNA_GAIN_UNKNOWN, c'BLADERF_LNA_GAIN_UNKNOWN)
         , (LNA_GAIN_BYPASS, c'BLADERF_LNA_GAIN_BYPASS)
         , (LNA_GAIN_MID, c'BLADERF_LNA_GAIN_MID)
         , (LNA_GAIN_MAX, c'BLADERF_LNA_GAIN_MAX)
         ]

-- | Device control and configuration.
--
-- This section provides functions pertaining to accessing, controlling, and
-- configuring various device options and parameters.
data BladeRFVGAGainBounds = RXVGA1_GAIN_MIN -- ^ Minimum RXVGA1 gain, in dB
                          | RXVGA1_GAIN_MAX -- ^ Maximum RXVGA1 gain, in dB
                          | RXVGA2_GAIN_MIN -- ^ Minimum RXVGA2 gain, in dB
                          | RXVGA2_GAIN_MAX -- ^ Maximum RXVGA2 gain, in dB
                          | TXVGA1_GAIN_MIN -- ^ Minimum TXVGA1 gain, in dB
                          | TXVGA1_GAIN_MAX -- ^ Maximum TXVGA1 gain, in dB
                          | TXVGA2_GAIN_MIN -- ^ Minimum TXVGA2 gain, in dB
                          | TXVGA2_GAIN_MAX -- ^ Maximum TXVGA2 gain, in dB
                          deriving (Eq)

instance Enum BladeRFVGAGainBounds where
  fromEnum = fromJust . flip lookup vgains
  toEnum   = fromJust . flip lookup (map swap vgains)

vgains = [ (RXVGA1_GAIN_MIN, c'BLADERF_RXVGA1_GAIN_MIN)
         , (RXVGA1_GAIN_MAX, c'BLADERF_RXVGA1_GAIN_MAX)
         , (RXVGA2_GAIN_MIN, c'BLADERF_RXVGA2_GAIN_MIN)
         , (RXVGA2_GAIN_MAX, c'BLADERF_RXVGA2_GAIN_MAX)
         , (TXVGA1_GAIN_MIN, c'BLADERF_TXVGA1_GAIN_MIN)
         , (TXVGA1_GAIN_MAX, c'BLADERF_TXVGA1_GAIN_MAX)
         , (TXVGA2_GAIN_MIN, c'BLADERF_TXVGA2_GAIN_MIN)
         , (TXVGA2_GAIN_MAX, c'BLADERF_TXVGA2_GAIN_MAX)
         ]

-- | Correction parameter selection.
--
-- These values specify the correction parameter to modify or query when
-- calling 'LibBladeRF.Frequency.bladeRFSetCorrection' or
-- 'LibBladeRF.Frequency.bladeRFGetCorrection'. Note that the
-- meaning of the `value` parameter to these functions depends upon the
-- correction parameter.
data BladeRFCorrection = CORR_LMS_DCOFF_I -- ^ Adjusts the in-phase DC offset via controls provided by the LMS6002D
                                          --   front end. Valid values are [-2048, 2048], which are scaled to the
                                          --   available control bits in the LMS device.
                       | CORR_LMS_DCOFF_Q -- ^ Adjusts the quadrature DC offset via controls provided the LMS6002D
                                          --   front end. Valid values are [-2048, 2048], which are scaled to the
                                          --   available control bits.
                       | CORR_FPGA_PHASE  -- ^ Adjusts FPGA-based phase correction of [-10, 10] degrees, via a provided
                                          --   count value of [-4096, 4096].
                       | CORR_FPGA_GAIN   -- ^ Adjusts FPGA-based gain correction of [0.0, 2.0], via provided
                                          --   values in the range of [-4096, 4096], where a value of 0 corresponds to
                                          --   a gain of 1.0.
                       deriving (Eq)

instance Enum BladeRFCorrection where
  fromEnum = fromJust . flip lookup corrections
  toEnum   = fromJust . flip lookup (map swap corrections)

corrections = [ (CORR_LMS_DCOFF_I, c'BLADERF_CORR_LMS_DCOFF_I)
              , (CORR_LMS_DCOFF_Q, c'BLADERF_CORR_LMS_DCOFF_Q)
              , (CORR_FPGA_PHASE, c'BLADERF_CORR_FPGA_PHASE)
              , (CORR_FPGA_GAIN, c'BLADERF_CORR_FPGA_GAIN)
              ]


-- | This enum describes the USB Speed at which the bladeRF is connected.
--
-- Speeds not listed here are not supported.
data BladeRFSpeed = DEVICE_SPEED_UNKNOWN -- ^ Unknown
                  | DEVICE_SPEED_HIGH    -- ^ USB2.0
                  | DEVICE_SPEED_SUPER   -- ^ USB3.0
                   deriving (Eq)

instance Show BladeRFSpeed where
  show DEVICE_SPEED_UNKNOWN = "unknown speed"
  show DEVICE_SPEED_HIGH    = "high speed"
  show DEVICE_SPEED_SUPER   = "super speed"

instance Enum BladeRFSpeed where
  fromEnum = fromJust . flip lookup speeds
  toEnum   = fromJust . flip lookup (map swap speeds)

speeds = [ (DEVICE_SPEED_UNKNOWN, c'BLADERF_DEVICE_SPEED_UNKNOWN)
         , (DEVICE_SPEED_HIGH, c'BLADERF_DEVICE_SPEED_HIGH)
         , (DEVICE_SPEED_SUPER, c'BLADERF_DEVICE_SPEED_SUPER)
         ]


-- | Sample metadata.
--
-- This structure is used in conjunction with the 'FORMAT_SC16_Q11_META'
-- format to TX scheduled bursts or retrieve timestamp information about
-- received samples.
data BladeRFMetadata = BladeRFMetadata { timestamp :: Word64 -- ^ Free-running FPGA counter that monotonically increases
                                                             --   at the sample rate of the associated module.
                                       , flags     :: Word32 -- ^ Input bit field to control the behavior of the call
                                                             --   that the metadata structure is passed to.
                                       , status    :: Word32 -- ^ Output bit field to denoting the status of
                                                             --   transmissions/receptions.
                                       , count     :: Int    -- ^ This output parameter is updated to reflect the actual
                                                             --   number of contiguous samples that have been populated
                                                             --   in an RX buffer during a 'LibBladeRF.Sync.bladeRFSyncRx' call.
                                       }


-- | Isomorpishms
--bladeRFMetadataToCBladeRFMetadata :: BladeRFMetadata -> C'bladerf_metadata
--bladeRFMetadataToCBladeRFMetadata  = to . coerce . from
--
--bladeRFMetadataFromCBladeRFMetadata :: C'bladerf_metadata -> BladeRFMetadata
--bladeRFMetadataFromCBladeRFMetadata  = to . coerce . from


-- | Loopback options.
data BladeRFLoopback = LB_FIRMWARE         -- ^ Firmware loopback inside of the FX3
                     | LB_BB_TXLPF_RXVGA2  -- ^ Baseband loopback. TXLPF output is connected to the RXVGA2 input.
                     | LB_BB_TXVGA1_RXVGA2 -- ^ Baseband loopback. TXVGA1 output is connected to the RXVGA2 input.
                     | LB_BB_TXLPF_RXLPF   -- ^ Baseband loopback. TXLPF output is connected to the RXLPF input.
                     | LB_BB_TXVGA1_RXLPF  -- ^ Baseband loopback. TXVGA1 output is connected to RXLPF input.
                     | LB_RF_LNA1          -- ^ RF loopback. The TXMIX output, through the AUX PA, is connected to the
                                           --   output of LNA1.
                     | LB_RF_LNA2          -- ^ RF loopback. The TXMIX output, through the AUX PA, is connected to the
                                           --   output of LNA2.
                     | LB_RF_LNA3          -- ^ RF loopback. The TXMIX output, through the AUX PA, is connected to the
                                           --   output of LNA3.
                     | LB_NONE             -- ^ Disables loopback and returns to normal operation.
                     deriving (Eq)

instance Enum BladeRFLoopback where
  fromEnum = fromJust . flip lookup loopbacks
  toEnum   = fromJust . flip lookup (map swap loopbacks)

loopbacks = [ (LB_FIRMWARE, c'BLADERF_LB_FIRMWARE)
            , (LB_BB_TXLPF_RXVGA2, c'BLADERF_LB_BB_TXLPF_RXVGA2)
            , (LB_BB_TXVGA1_RXVGA2, c'BLADERF_LB_BB_TXVGA1_RXVGA2)
            , (LB_BB_TXLPF_RXLPF, c'BLADERF_LB_BB_TXLPF_RXLPF)
            , (LB_BB_TXVGA1_RXLPF, c'BLADERF_LB_BB_TXVGA1_RXLPF)
            , (LB_RF_LNA1, c'BLADERF_LB_RF_LNA1)
            , (LB_RF_LNA2, c'BLADERF_LB_RF_LNA2)
            , (LB_RF_LNA3, c'BLADERF_LB_RF_LNA3)
            , (LB_NONE, c'BLADERF_LB_NONE)
            ]
