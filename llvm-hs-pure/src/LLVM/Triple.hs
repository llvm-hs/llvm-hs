{-# LANGUAGE RecordWildCards #-}

-- | A 'Triple' represents a target triple, which is a target host description.
-- | Target triples consistent of a few components: architecture, vendor,
-- | operating system, and environment.
-- | <https://llvm.org/docs/LangRef.html#target-triple>

module LLVM.Triple (
   Triple (..), Architecture (..), Vendor (..), OS (..), unknownTriple, parseTriple, tripleToString
 ) where

import LLVM.Prelude
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as ByteString hiding (map, foldr)
import Data.ByteString.Short hiding (pack, foldr)

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Architecture
  = UnknownArch
  | Arm            -- ARM (little endian): arm armv.* xscale
  | Armeb          -- ARM (big endian): armeb
  | Aarch64        -- AArch64 (little endian): aarch64
  | Aarch64_be     -- AArch64 (big endian): aarch64_be
  | Aarch64_32     -- AArch64 (little endian) ILP32: aarch64_32
  | Arc            -- ARC: Synopsys ARC
  | Avr            -- AVR: Atmel AVR microcontroller
  | Bpfel          -- eBPF or extended BPF or 64-bit BPF (little endian)
  | Bpfeb          -- eBPF or extended BPF or 64-bit BPF (big endian)
  | Csky           -- CSKY: csky
  | Hexagon        -- Hexagon: hexagon
  | Mips           -- MIPS: mips mipsallegrex mipsr6
  | Mipsel         -- MIPSEL: mipsel mipsallegrexe mipsr6el
  | Mips64         -- MIPS64: mips64 mips64r6 mipsn32 mipsn32r6
  | Mips64el       -- MIPS64EL: mips64el mips64r6el mipsn32el mipsn32r6el
  | Msp430         -- MSP430: msp430
  | Ppc            -- PPC: powerpc
  | Ppcle          -- PPCLE: powerpc (little endian)
  | Ppc64          -- PPC64: powerpc64 ppu
  | Ppc64le        -- PPC64LE: powerpc64le
  | R600           -- R600: AMD GPUs HD2XXX - HD6XXX
  | Amdgcn         -- AMDGCN: AMD GCN GPUs
  | Riscv32        -- RISC-V (32-bit): riscv32
  | Riscv64        -- RISC-V (64-bit): riscv64
  | Sparc          -- Sparc: sparc
  | Sparcv9        -- Sparcv9: Sparcv9
  | Sparcel        -- Sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
  | Systemz        -- SystemZ: s390x
  | Tce            -- TCE (http://tce.cs.tut.fi/): tce
  | Tcele          -- TCE little endian (http://tce.cs.tut.fi/): tcele
  | Thumb          -- Thumb (little endian): thumb thumbv.*
  | Thumbeb        -- Thumb (big endian): thumbeb
  | X86            -- X86: i[3-9]86
  | X86_64         -- X86-64: amd64 x86_64
  | Xcore          -- XCore: xcore
  | Nvptx          -- NVPTX: 32-bit
  | Nvptx64        -- NVPTX: 64-bit
  | Le32           -- le32: generic little-endian 32-bit CPU (PNaCl)
  | Le64           -- le64: generic little-endian 64-bit CPU (PNaCl)
  | Amdil          -- AMDIL
  | Amdil64        -- AMDIL with 64-bit pointers
  | Hsail          -- AMD HSAIL
  | Hsail64        -- AMD HSAIL with 64-bit pointers
  | Spir           -- SPIR: standard portable IR for OpenCL 32-bit version
  | Spir64         -- SPIR: standard portable IR for OpenCL 64-bit version
  | Kalimba        -- Kalimba: generic kalimba
  | Shave          -- SHAVE: Movidius vector VLIW processors
  | Lanai          -- Lanai: Lanai 32-bit
  | Wasm32         -- WebAssembly with 32-bit pointers
  | Wasm64         -- WebAssembly with 64-bit pointers
  | Renderscript32 -- 32-bit RenderScript
  | Renderscript64 -- 64-bit RenderScript
  | Ve             -- NEC SX-Aurora Vector Engine
  deriving (Eq, Ord, Show)

-- NOTE: SubArchitecture is not currently used.
data SubArchitecture
  = NoSubArch
  | ARMSubArch_v8_7a
  | ARMSubArch_v8_6a
  | ARMSubArch_v8_5a
  | ARMSubArch_v8_4a
  | ARMSubArch_v8_3a
  | ARMSubArch_v8_2a
  | ARMSubArch_v8_1a
  | ARMSubArch_v8
  | ARMSubArch_v8r
  | ARMSubArch_v8m_baseline
  | ARMSubArch_v8m_mainline
  | ARMSubArch_v8_1m_mainline
  | ARMSubArch_v7
  | ARMSubArch_v7em
  | ARMSubArch_v7m
  | ARMSubArch_v7s
  | ARMSubArch_v7k
  | ARMSubArch_v7ve
  | ARMSubArch_v6
  | ARMSubArch_v6m
  | ARMSubArch_v6k
  | ARMSubArch_v6t2
  | ARMSubArch_v5
  | ARMSubArch_v5te
  | ARMSubArch_v4t
  | AArch64SubArch_arm64e
  | KalimbaSubArch_v3
  | KalimbaSubArch_v4
  | KalimbaSubArch_v5
  | MipsSubArch_r6
  | PPCSubArch_spe
  deriving (Eq, Ord, Show)

data Vendor
  = UnknownVendor
  | Apple
  | PC
  | SCEI
  | Freescale
  | IBM
  | ImaginationTechnologies
  | MipsTechnologies
  | NVIDIA
  | CSR
  | Myriad
  | AMD
  | Mesa
  | SUSE
  | OpenEmbedded
  deriving (Eq, Ord, Show)

data OS
  = UnknownOS
  | Ananas
  | CloudABI
  | Darwin
  | DragonFly
  | FreeBSD
  | Fuchsia
  | IOS
  | KFreeBSD
  | Linux
  | Lv2        -- PS3
  | MacOSX
  | NetBSD
  | OpenBSD
  | Solaris
  | Win32
  | ZOS
  | Haiku
  | Minix
  | RTEMS
  | NaCl       -- Native Client
  | AIX
  | CUDA       -- NVIDIA CUDA
  | NVCL       -- NVIDIA OpenCL
  | AMDHSA     -- AMD HSA Runtime
  | PS4
  | ELFIAMCU
  | TvOS       -- Apple tvOS
  | WatchOS    -- Apple watchOS
  | Mesa3D
  | Contiki
  | AMDPAL     -- AMD PAL Runtime
  | HermitCore -- HermitCore Unikernel/Multikernel
  | Hurd       -- GNU/Hurd
  | WASI       -- Experimental WebAssembly OS
  | Emscripten
  deriving (Eq, Ord, Show)

data Environment
  = UnknownEnvironment
  | GNU
  | GNUABIN32
  | GNUABI64
  | GNUEABI
  | GNUEABIHF
  | GNUX32
  | GNUILP32
  | CODE16
  | EABI
  | EABIHF
  | Android
  | Musl
  | MuslEABI
  | MuslEABIHF
  | MSVC
  | Itanium
  | Cygnus
  | CoreCLR
  | Simulator -- Simulator variants of other systems e.g. Apple's iOS
  | MacABI -- Mac Catalyst variant of Apple's iOS deployment target.
  deriving (Eq, Ord, Show)

data ObjectFormat
  = UnknownObjectFormat
  | COFF
  | ELF
  | GOFF
  | MachO
  | Wasm
  | XCOFF
  deriving (Eq, Ord, Show)

data Triple = Triple {
    architecture :: Architecture,
    subarchitecture :: SubArchitecture,
    os :: OS,
    vendor :: Vendor,
    environment :: Environment,
    objectFormat :: ObjectFormat
  } deriving (Eq, Ord, Show)

unknownTriple :: Triple
unknownTriple = Triple {
    architecture = UnknownArch,
    subarchitecture = NoSubArch,
    vendor = UnknownVendor,
    os = UnknownOS,
    environment = UnknownEnvironment,
    objectFormat = UnknownObjectFormat
  }

invertBijection :: (Ord k, Ord v) => Map k v -> Map v k
invertBijection = Map.foldrWithKey (flip Map.insert) Map.empty

architectureFromStringMap :: Map String Architecture
architectureFromStringMap = Map.fromList [
  ("unknown", UnknownArch),
  ("arm", Arm),
  ("armeb", Armeb),
  ("aarch64", Aarch64),
  ("aarch64_be", Aarch64_be),
  ("aarch64_32", Aarch64_32),
  ("arc", Arc),
  ("avr", Avr),
  ("bpfel", Bpfel),
  ("bpfeb", Bpfeb),
  ("csky", Csky),
  ("hexagon", Hexagon),
  ("mips", Mips),
  ("mipsel", Mipsel),
  ("mips64", Mips64),
  ("mips64el", Mips64el),
  ("msp430", Msp430),
  ("ppc", Ppc),
  ("ppcle", Ppcle),
  ("ppc64", Ppc64),
  ("ppc64le", Ppc64le),
  ("r600", R600),
  ("amdgcn", Amdgcn),
  ("riscv32", Riscv32),
  ("riscv64", Riscv64),
  ("sparc", Sparc),
  ("sparcv9", Sparcv9),
  ("sparcel", Sparcel),
  ("systemz", Systemz),
  ("tce", Tce),
  ("tcele", Tcele),
  ("thumb", Thumb),
  ("thumbeb", Thumbeb),
  ("x86", X86),
  ("x86_64", X86_64),
  ("xcore", Xcore),
  ("nvptx", Nvptx),
  ("nvptx64", Nvptx64),
  ("le32", Le32),
  ("le64", Le64),
  ("amdil", Amdil),
  ("amdil64", Amdil64),
  ("hsail", Hsail),
  ("hsail64", Hsail64),
  ("spir", Spir),
  ("spir64", Spir64),
  ("kalimba", Kalimba),
  ("shave", Shave),
  ("lanai", Lanai),
  ("wasm32", Wasm32),
  ("wasm64", Wasm64),
  ("renderscript32", Renderscript32),
  ("renderscript64", Renderscript64),
  ("ve", Ve)
  ]

architectureToStringMap :: Map Architecture String
architectureToStringMap = invertBijection architectureFromStringMap

vendorFromStringMap :: Map String Vendor
vendorFromStringMap = Map.fromList [
  ("apple"                  , Apple                  ),
  ("pc"                     , PC                     ),
  ("scei"                   , SCEI                   ),
  ("freescale"              , Freescale              ),
  ("ibm"                    , IBM                    ),
  ("imaginationtechnologies", ImaginationTechnologies),
  ("mipstechnologies"       , MipsTechnologies       ),
  ("nvidia"                 , NVIDIA                 ),
  ("csr"                    , CSR                    ),
  ("myriad"                 , Myriad                 ),
  ("amd"                    , AMD                    ),
  ("mesa"                   , Mesa                   ),
  ("suse"                   , SUSE                   ),
  ("openembedded"           , OpenEmbedded           )
  ]

vendorToStringMap :: Map Vendor String
vendorToStringMap = invertBijection vendorFromStringMap

osFromStringMap :: Map String OS
osFromStringMap = Map.fromList [
  ("ananas"    , Ananas    ),
  ("cloudabi"  , CloudABI  ),
  ("darwin"    , Darwin    ),
  ("dragonfly" , DragonFly ),
  ("freebsd"   , FreeBSD   ),
  ("fuchsia"   , Fuchsia   ),
  ("ios"       , IOS       ),
  ("kfreebsd"  , KFreeBSD  ),
  ("linux"     , Linux     ),
  ("lv2"       , Lv2       ),
  ("macosx"    , MacOSX    ),
  ("netbsd"    , NetBSD    ),
  ("openbsd"   , OpenBSD   ),
  ("solaris"   , Solaris   ),
  ("win32"     , Win32     ),
  ("zos"       , ZOS       ),
  ("haiku"     , Haiku     ),
  ("minix"     , Minix     ),
  ("rtems"     , RTEMS     ),
  ("nacl"      , NaCl      ),
  ("aix"       , AIX       ),
  ("cuda"      , CUDA      ),
  ("nvcl"      , NVCL      ),
  ("amdhsa"    , AMDHSA    ),
  ("ps4"       , PS4       ),
  ("elfiamcu"  , ELFIAMCU  ),
  ("tvos"      , TvOS      ),
  ("watchos"   , WatchOS   ),
  ("mesa3d"    , Mesa3D    ),
  ("contiki"   , Contiki   ),
  ("amdpal"    , AMDPAL    ),
  ("hermitcore", HermitCore),
  ("hurd"      , Hurd      ),
  ("wasi"      , WASI      ),
  ("emscripten", Emscripten)
  ]

osToStringMap :: Map OS String
osToStringMap = invertBijection osFromStringMap

tripleToString :: Triple -> ShortByteString
tripleToString Triple {..} =
  toShort $ ByteString.intercalate (pack "-") [
    pack (architectureToStringMap ! architecture),
    pack (vendorToStringMap ! vendor),
    pack (osToStringMap ! os)
  ]

parseTriple :: ShortByteString -> Except String Triple
parseTriple triple = do
  let
    tripleStr = fromShort triple
    parseSpec :: Parser (Triple -> Triple)
    parseSpec = choice [
      do
        arch <- choice [string (pack s) $> a | (s, a) <- Map.toList architectureFromStringMap]
        pure $ \t -> t { architecture = arch },
      do
        vendor <- choice [string (pack s) $> v | (s, v) <- Map.toList vendorFromStringMap]
        pure $ \t -> t { vendor = vendor },
      do
        os <- choice [string (pack s) $> o | (s, o) <- Map.toList osFromStringMap]
        pure $ \t -> t { os = os }
     ]
   in
    case parseOnly (parseSpec `sepBy` char '-') tripleStr of
      Left _ -> throwE $ "ill-formed triple: " ++ show tripleStr
      Right fs -> pure $ foldr ($) unknownTriple fs

