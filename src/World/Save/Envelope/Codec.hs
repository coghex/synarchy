{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The pure, side-effect-free tagged-envelope codec (issue #759,
--   save-overhaul B1): encode a set of (id, version, required, bytes)
--   component specs into one framed, checksummed 'BS.ByteString', and
--   decode + fully validate one back into a 'DecodedEnvelope' before
--   any caller looks at a specific component's payload.
--
--   Byte layout (all multi-byte integers big-endian, hand-rolled via
--   "World.Save.Envelope.Types"'s @encodeW32@/@encodeW64@ rather than
--   cereal, so the envelope's own framing is under this module's
--   explicit control):
--
--   > magic(4) | envelopeVersion(4) | manifestLength(8)   -- header, 16 bytes
--   > manifest bytes (cereal Generic 'EnvelopeManifest')  -- manifestLength bytes
--   > manifestChecksum(8)                                 -- fnv1a64 of the manifest bytes
--   > component payloads, back-to-back at their declared (cdOffset, cdLength)
--
--   Encoding lays components out in canonical order (component-id
--   ascending) with contiguous, non-overlapping offsets starting at 0
--   — the SAME inputs always produce the SAME bytes (requirement 10).
--   Decoding validates the COMPLETE structure — magic, version,
--   manifest shape + checksum, every documented limit, duplicate ids,
--   the required/optional contract, payload bounds, per-payload
--   checksums, and forbidden trailing bytes — before returning
--   anything a caller could inspect (requirement 7); nothing here
--   decodes a specific component's payload into its own application
--   type — that is "World.Save.Envelope"'s job.
module World.Save.Envelope.Codec
    ( DecodedEnvelope(..)
    , ComponentSpec
    , encodeEnvelope
    , decodeEnvelope
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import World.Save.Envelope.Types

-- | One component to encode: identifier, schema version, the writer's
--   own required/optional declaration, and its raw payload bytes.
type ComponentSpec = (ComponentId, Word32, Bool, BS.ByteString)

-- | A fully-validated, decoded envelope: every declared component's
--   payload bytes, keyed by id, already checksum-verified — whether or
--   not the caller goes on to interpret that component's payload as a
--   specific application type.
data DecodedEnvelope = DecodedEnvelope
    { deVersion  ∷ !Word32
    , deManifest ∷ !EnvelopeManifest
    , dePayloads ∷ !(HM.HashMap ComponentId BS.ByteString)
    } deriving (Show, Eq)

headerSize ∷ Int
headerSize = 16

-- | Encode a canonically-ordered, checksummed envelope. Rejects (via
--   'Left', never a partial write) a duplicate id or any spec that
--   would itself violate @limits@ — the writer is held to the exact
--   same discipline a reader will later enforce.
encodeEnvelope ∷ EnvelopeLimits → Word32 → [ComponentSpec]
              → Either EnvelopeError BS.ByteString
encodeEnvelope limits envVersion specs =
    case validateEncodeSpecs limits specs manifestBytes of
        (e:_) → Left e
        []    → Right encoded
  where
    sorted           = L.sortOn (\(cid,_,_,_) → cid) specs
    descriptors      = layoutDescriptors sorted
    manifest         = EnvelopeManifest descriptors
    manifestBytes    = S.encode manifest
    manifestChecksum = fnv1a64 manifestBytes
    payloadBytes     = BS.concat [ p | (_,_,_,p) ← sorted ]
    header           = encodeW32 envelopeMagic <> encodeW32 envVersion
                     <> encodeW64 (fromIntegral (BS.length manifestBytes))
    encoded          = header <> manifestBytes <> encodeW64 manifestChecksum
                     <> payloadBytes

-- | Every specs-level violation, most-relevant first. A writer that
--   trips one of these has a bug — these limits are only ever meant to
--   catch a genuinely pathological input, not routine saves.
validateEncodeSpecs ∷ EnvelopeLimits → [ComponentSpec] → BS.ByteString
                    → [EnvelopeError]
validateEncodeSpecs limits specs manifestBytes = concat
    [ [ DuplicateComponentId cid | cid ← dupes ]
    , [ TooManyComponents n | let n = length specs, n > elMaxComponents limits ]
    , [ ComponentIdTooLong cid | (cid,_,_,_) ← specs
      , componentIdLength cid > elMaxComponentIdLength limits ]
    , [ PayloadTooLarge cid len | (cid,_,_,p) ← specs
      , let len = fromIntegral (BS.length p), len > elMaxPayloadBytes limits ]
    , [ TotalPayloadTooLarge totalLen
      | totalLen > toInteger (elMaxTotalPayloadBytes limits) ]
    , [ ManifestTooLarge (BS.length manifestBytes)
      | BS.length manifestBytes > elMaxManifestBytes limits ]
    ]
  where
    counts   = HM.fromListWith (+) [ (cid, 1 ∷ Int) | (cid,_,_,_) ← specs ]
    dupes    = [ cid | (cid, n) ← HM.toList counts, n > 1 ]
    totalLen = sum [ toInteger (BS.length p) | (_,_,_,p) ← specs ] ∷ Integer

-- | Assign each sorted component its contiguous offset, starting at 0.
layoutDescriptors ∷ [ComponentSpec] → [ComponentDescriptor]
layoutDescriptors = go 0
  where
    go _   []                        = []
    go off ((cid,ver,req,payload):rest) =
        let len  = fromIntegral (BS.length payload) ∷ Word64
            desc = ComponentDescriptor
                { cdId = cid, cdVersion = ver, cdRequired = req
                , cdOffset = off, cdLength = len
                , cdChecksum = fnv1a64 payload
                }
        in desc : go (off + len) rest

-- | Decode + fully validate an envelope's structure before returning
--   anything. @knownIds@ is the set of component ids this reader knows
--   how to interpret (used to judge a writer's own @cdRequired@ flag);
--   @readerRequiredIds@ is this reader's OWN hard requirement,
--   independent of what the writer declared (requirement 6).
decodeEnvelope ∷ EnvelopeLimits → Word32 → HS.HashSet ComponentId
              → HS.HashSet ComponentId → BS.ByteString
              → Either EnvelopeError DecodedEnvelope
decodeEnvelope limits expectedVersion knownIds readerRequiredIds bytes = do
    when (BS.length bytes < headerSize) (Left TruncatedHeader)
    let magic       = decodeW32 (BS.take 4 bytes)
        version     = decodeW32 (BS.take 4 (BS.drop 4 bytes))
        manifestLen = decodeW64 (BS.take 8 (BS.drop 8 bytes))
        afterHeader = BS.drop headerSize bytes
    when (magic ≢ envelopeMagic) (Left BadMagic)
    when (version ≢ expectedVersion) (Left (UnsupportedEnvelopeVersion version))
    when (toInteger manifestLen > toInteger (elMaxManifestBytes limits)) $
        Left (ManifestTooLarge (fromIntegral manifestLen))
    when (toInteger manifestLen + 8 > toInteger (BS.length afterHeader)) $
        Left ManifestTruncated
    let manifestBytes  = BS.take (fromIntegral manifestLen) afterHeader
        afterManifest  = BS.drop (fromIntegral manifestLen) afterHeader
        storedChecksum = decodeW64 (BS.take 8 afterManifest)
        payloadSection = BS.drop 8 afterManifest
        actualChecksum = fnv1a64 manifestBytes
    when (storedChecksum ≢ actualChecksum) (Left ManifestChecksumMismatch)
    manifest ← either (Left . ManifestMalformed) Right (S.decode manifestBytes)
    let descs = emComponents manifest
    validateManifestStructure limits knownIds readerRequiredIds descs
    validatePayloadLayout descs (BS.length payloadSection)
    payloads ← forM descs $ \d → do
        let off   = fromIntegral (cdOffset d)
            len   = fromIntegral (cdLength d)
            slice = BS.take len (BS.drop off payloadSection)
        when (fnv1a64 slice ≢ cdChecksum d) $
            Left (ComponentChecksumMismatch (cdId d))
        pure (cdId d, slice)
    pure DecodedEnvelope
        { deVersion  = version
        , deManifest = manifest
        , dePayloads = HM.fromList payloads
        }

-- | Every manifest-shape violation that doesn't depend on the actual
--   payload bytes: limits, duplicates, and the required/optional
--   contract (requirement 6).
validateManifestStructure ∷ EnvelopeLimits → HS.HashSet ComponentId
                          → HS.HashSet ComponentId → [ComponentDescriptor]
                          → Either EnvelopeError ()
validateManifestStructure limits knownIds readerRequiredIds descs =
    case errs of
        (e:_) → Left e
        []    → Right ()
  where
    counts  = HM.fromListWith (+) [ (cdId d, 1 ∷ Int) | d ← descs ]
    dupes   = [ cid | (cid, n) ← HM.toList counts, n > 1 ]
    present = HS.fromList (map cdId descs)
    errs = concat
        [ [ DuplicateComponentId cid | cid ← dupes ]
        , [ TooManyComponents n | let n = length descs, n > elMaxComponents limits ]
        , [ ComponentIdTooLong (cdId d) | d ← descs
          , componentIdLength (cdId d) > elMaxComponentIdLength limits ]
        , [ UnknownRequiredComponent (cdId d) | d ← descs
          , cdRequired d, not (HS.member (cdId d) knownIds) ]
        , [ MissingRequiredComponent rid | rid ← HS.toList readerRequiredIds
          , not (HS.member rid present) ]
        , [ PayloadTooLarge (cdId d) (cdLength d) | d ← descs
          , cdLength d > elMaxPayloadBytes limits ]
        , [ TotalPayloadTooLarge totalLen
          | totalLen > toInteger (elMaxTotalPayloadBytes limits) ]
        ]
    totalLen = sum [ toInteger (cdLength d) | d ← descs ] ∷ Integer

-- | Bounds validation against the ACTUAL payload-section bytes: no two
--   components may overlap, every declared range must fit inside what
--   is actually present (else 'TruncatedPayload'), and nothing may be
--   left over past the highest declared range (else 'TrailingBytes').
--   Gaps between components (dead, undeclared bytes) are tolerated —
--   our own encoder never produces one, and nothing in the contract
--   requires rejecting one.
validatePayloadLayout ∷ [ComponentDescriptor] → Int → Either EnvelopeError ()
validatePayloadLayout descs availableLen = do
    checkOverlaps (L.sortOn cdOffset descs)
    let maxEnd = maximum (0 : [ toInteger (cdOffset d) + toInteger (cdLength d)
                              | d ← descs ])
    when (maxEnd > toInteger availableLen) $
        Left (TruncatedPayload (firstOverrunComponent descs availableLen))
    when (maxEnd < toInteger availableLen) $
        Left (TrailingBytes (availableLen - fromInteger maxEnd))
  where
    firstOverrunComponent ds avail = case overruns of
        (cid:_) → cid
        []      → ComponentId "?"  -- unreachable: maxEnd check above already fired
      where
        overruns = [ cdId d | d ← ds
                   , toInteger (cdOffset d) + toInteger (cdLength d)
                       > toInteger avail ]

checkOverlaps ∷ [ComponentDescriptor] → Either EnvelopeError ()
checkOverlaps sortedByOffset = go sortedByOffset
  where
    go (a:b:rest)
        | toInteger (cdOffset a) + toInteger (cdLength a) > toInteger (cdOffset b)
            = Left (OverlappingComponents (cdId a) (cdId b))
        | otherwise = go (b:rest)
    go _ = Right ()
