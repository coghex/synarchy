{-# LANGUAGE Strict, UnicodeSyntax, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
-- | Wire-level types for the tagged, checksummed save envelope (issue
--   #759, save-overhaul B1): stable component identifiers, a manifest
--   of per-component descriptors, the structured errors a malformed or
--   incompatible envelope can fail with, and the documented allocation
--   limits enforced before any claimed length is trusted.
--
--   Deliberately independent of "World.Save.Types" — this module knows
--   nothing about 'SaveData'/'SaveMetadata'; it only describes the
--   generic tagged-component container those types are carried inside.
--   "World.Save.Envelope" is the layer that ties the two together, and
--   "World.Save.Envelope.Codec" is the pure encode/decode logic built
--   on top of these types.
module World.Save.Envelope.Types
    ( ComponentId(..)
    , ComponentDescriptor(..)
    , EnvelopeManifest(..)
    , EnvelopeLimits(..)
    , defaultEnvelopeLimits
    , EnvelopeError(..)
    , envelopeMagic
    , componentIdLength
    , fnv1a64
    , encodeW32
    , decodeW32
    , encodeW64
    , decodeW64
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | A stable, writer-chosen component identifier (e.g. @"metadata"@,
--   @"session"@). 'Serialize' rides on 'Text''s own orphan instance
--   (UPrelude) so component ids can appear inside a cereal-encoded
--   'EnvelopeManifest'.
newtype ComponentId = ComponentId Text
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable, Serialize)

-- | One component's manifest entry. 'cdOffset'/'cdLength' locate its
--   payload within the envelope's payload section (the bytes
--   immediately following the manifest checksum); 'cdChecksum' is
--   'fnv1a64' of exactly those payload bytes. 'cdRequired' is the
--   WRITER's own declaration of whether a reader must understand this
--   component — see "World.Save.Envelope.Codec".decodeEnvelope for how
--   that interacts with a READER's own hard requirements (requirement
--   6: a component the writer marked optional can't override a
--   reader's own need for it).
data ComponentDescriptor = ComponentDescriptor
    { cdId       ∷ !ComponentId
    , cdVersion  ∷ !Word32
    , cdRequired ∷ !Bool
    , cdOffset   ∷ !Word64
    , cdLength   ∷ !Word64
    , cdChecksum ∷ !Word64
    } deriving (Show, Eq, Generic, Serialize)

-- | The full set of components an envelope declares, in the order the
--   writer chose to lay them out on disk (canonically, component-id
--   ascending — see 'World.Save.Envelope.Codec.encodeEnvelope').
newtype EnvelopeManifest = EnvelopeManifest
    { emComponents ∷ [ComponentDescriptor]
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (Serialize)

-- | Documented allocation limits, enforced against every CLAIMED
--   length before it is trusted for further parsing (requirement 8).
--   Generous enough for any real world save; a file that claims more
--   is corrupt or hostile, never legitimate.
data EnvelopeLimits = EnvelopeLimits
    { elMaxManifestBytes     ∷ !Int
        -- ^ A manifest is a handful of small descriptors; 1 MiB is
        --   thousands of components, far beyond any real envelope.
    , elMaxComponents        ∷ !Int
    , elMaxComponentIdLength ∷ !Int
    , elMaxPayloadBytes      ∷ !Word64
        -- ^ Per-component cap.
    , elMaxTotalPayloadBytes ∷ !Word64
        -- ^ Sum of every declared component length.
    } deriving (Show, Eq)

-- | The default limits every production encode/decode call uses.
--   Tests pass a deliberately tighter 'EnvelopeLimits' to exercise
--   each rejection path without needing to hand-craft oversized bytes.
defaultEnvelopeLimits ∷ EnvelopeLimits
defaultEnvelopeLimits = EnvelopeLimits
    { elMaxManifestBytes     = 1 * 1024 * 1024
    , elMaxComponents        = 64
    , elMaxComponentIdLength = 64
    , elMaxPayloadBytes      = 2 * 1024 * 1024 * 1024
    , elMaxTotalPayloadBytes = 4 * 1024 * 1024 * 1024
    }

-- | Every way envelope decoding can fail, naming the failing PHASE by
--   its own constructor and, where applicable, the offending component
--   (requirement 7). Never thrown as an exception — decoding untrusted
--   bytes is a routine, expected-to-sometimes-fail operation, always
--   reported as a plain 'Either', never a crash.
data EnvelopeError
    = TruncatedHeader
    | BadMagic
    | UnsupportedEnvelopeVersion !Word32
    | ManifestTooLarge !Int
    | ManifestTruncated
    | ManifestMalformed !String
    | ManifestChecksumMismatch
    | TooManyComponents !Int
    | ComponentIdTooLong !ComponentId
    | DuplicateComponentId !ComponentId
    | PayloadTooLarge !ComponentId !Word64
    | TotalPayloadTooLarge !Integer
    | UnknownRequiredComponent !ComponentId
    | MissingRequiredComponent !ComponentId
    | OverlappingComponents !ComponentId !ComponentId
    | TruncatedPayload !ComponentId
    | ComponentChecksumMismatch !ComponentId
    | TrailingBytes !Int
    | UnsupportedComponentVersion !ComponentId !Word32
    | ComponentDecodeError !ComponentId !String
    deriving (Show, Eq)

-- | 4-byte magic prefix on every envelope. Numerically the same "SYRA"
--   value the pre-#759 flat format used ('World.Save.Types.saveMagic')
--   — defined independently here (rather than imported) so this module
--   stays free of any dependency on "World.Save.Types".
envelopeMagic ∷ Word32
envelopeMagic = 0x53595241

-- | FNV-1a, 64-bit. A stable, simple, documented hash used PURELY to
--   detect accidental corruption (bit rot, truncation, a bad copy) —
--   requirement 9. This is NOT a cryptographic checksum: it offers no
--   protection against a deliberately crafted file, only against
--   accidental damage. Covers exactly the bytes passed in — callers
--   hash either the manifest's own encoded bytes or one component's
--   raw payload bytes, never anything else.
fnv1a64 ∷ BS.ByteString → Word64
fnv1a64 = BS.foldl' step fnvOffsetBasis
  where
    fnvOffsetBasis = 0xcbf29ce484222325
    fnvPrime       = 0x00000100000001b3
    step acc byte = (acc `xor` fromIntegral byte) * fnvPrime

-- | Big-endian fixed-width encode/decode for the envelope's own header
--   scalars. Hand-rolled (not cereal) so the envelope's OWN framing
--   bytes are laid out under this module's explicit control rather
--   than an incidental library encoding. 'decodeW32'/'decodeW64'
--   assume the input is EXACTLY 4/8 bytes — callers must slice first.
encodeW32 ∷ Word32 → BS.ByteString
encodeW32 w = BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

decodeW32 ∷ BS.ByteString → Word32
decodeW32 = BS.foldl' (\acc byte → (acc `shiftL` 8) .|. fromIntegral byte) 0

encodeW64 ∷ Word64 → BS.ByteString
encodeW64 w = BS.pack [ fromIntegral (w `shiftR` (8 * i)) | i ← [7,6..0 ∷ Int] ]

decodeW64 ∷ BS.ByteString → Word64
decodeW64 = BS.foldl' (\acc byte → (acc `shiftL` 8) .|. fromIntegral byte) 0

-- | Length of a component id's own text, for the
--   'elMaxComponentIdLength' check.
componentIdLength ∷ ComponentId → Int
componentIdLength (ComponentId t) = T.length t
