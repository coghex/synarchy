"""Synthetic source tree for the current-schema guard's self-test
(issue #2916). Only `--self-test` loads it.

`base_files()` is a minimal repository in the shape the model reads:
two Haskell components sharing one leaf DTO (`alpha` keeps a frozen v1
decoder, `beta` carries a hand-written `Serialize` instance), the
`metadata` component with a frozen v1 mirror, two Lua modules, the
framing sources, the B1 session carrier and both legacy routes ENABLED.
Every case mutates a copy and compares it with this base.
"""
from __future__ import annotations

from pathlib import Path

CABAL = """\
common lang
  default-language: GHC2024
  default-extensions: NoImplicitPrelude, UnicodeSyntax

library
  import: lang
  exposed-modules: World.Save.Envelope
"""

ENVELOPE = """\
module World.Save.Envelope where

currentEnvelopeVersion ∷ Word32
currentEnvelopeVersion = 1

metadataComponentVersion ∷ Word32
metadataComponentVersion = 2

metadataComponentInputVersions ∷ [Word32]
metadataComponentInputVersions =
    [ legacyMetadataComponentVersion
    , metadataComponentVersion ]

legacyMetadataComponentVersion ∷ Word32
legacyMetadataComponentVersion = 1

luaStateComponentVersion ∷ Word32
luaStateComponentVersion = 1

tryLegacyEnvelopeFallbacks ∷ ByteString → Result
tryLegacyEnvelopeFallbacks bytes = case decodeLegacySessionEnvelope bytes of
    NotLegacyShaped → decodeB2SessionEnvelope bytes
    other           → other

tryLegacyMetadataFallbacks ∷ ByteString → Result
tryLegacyMetadataFallbacks bytes = case decodeLegacySessionMetadata bytes of
    NotLegacyShaped → decodeB2SessionMetadata bytes
    other           → other

otherFunction ∷ Int
otherFunction = 0
"""

ENVELOPE_TYPES = """\
module World.Save.Envelope.Types where

data ComponentId = ComponentId Text

data ComponentDescriptor = ComponentDescriptor
    { cdId ∷ !ComponentId, cdVersion ∷ !Word32 }

data EnvelopeManifest = EnvelopeManifest
    { emComponents ∷ ![ComponentDescriptor] }

envelopeMagic ∷ ByteString
envelopeMagic = "SYNA"

fnv1a64 ∷ ByteString → Word64
fnv1a64 = hash64

encodeW32 ∷ Word32 → ByteString
encodeW32 = w32

decodeW32 ∷ ByteString → Word32
decodeW32 = r32

encodeW64 ∷ Word64 → ByteString
encodeW64 = w64

decodeW64 ∷ ByteString → Word64
decodeW64 = r64

"""

ENVELOPE_CODEC = """\
module World.Save.Envelope.Codec where

encodeEnvelope ∷ Int → Int
encodeEnvelope n = n
"""

REGISTRY = """\
module World.Save.Component where

saveComponentRegistry ∷ [SomeCodec]
saveComponentRegistry =
    [ registerComponent alphaCodec
    , registerComponent betaCodec
    ]
"""

COMPONENT_TYPES = """\
module World.Save.Component.Types where

data ComponentError = ComponentError Text
"""

ALPHA = """\
module World.Save.Component.Alpha where

alphaComponentId ∷ ComponentId
alphaComponentId = ComponentId "alpha"

-- | The alpha payload.
data AlphaDTO = AlphaDTO
    { adName   ∷ !Text   -- ^ display name
    , adKind   ∷ !AlphaKindDTO
    , adAlias  ∷ !AlphaAlias
    , adWrap   ∷ !AlphaWrap
    , adShared ∷ !SharedDTO
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)

data AlphaKindDTO = KindA | KindB
    deriving stock (Show, Generic)
    deriving anyclass (Serialize)

type AlphaAlias = Int

newtype AlphaWrap = AlphaWrap Int
    deriving stock (Show, Generic)
    deriving anyclass (Serialize)

data SharedDTO = SharedDTO
    { sdLeft  ∷ !Int
    , sdRight ∷ !Int
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)

data AlphaDTOv1 = AlphaDTOv1
    { a1Name ∷ !Text
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)

migrateAlphaDTOv1
    ∷ AlphaDTOv1 → AlphaDTO
migrateAlphaDTOv1 (AlphaDTOv1 n) = AlphaDTO n KindA 0 (AlphaWrap 0) (SharedDTO 0 0)

alphaCodec ∷ ComponentCodec AlphaDTO
alphaCodec = componentCodec ComponentSpec
    { csComponent     = alphaComponentId
    , csVersion       = 2
    , csRequired      = True
    , csDeps          = []
    , csEncode        = id
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateAlphaDTOv1 ]
    , csValidate      = const []
    }
"""

BETA = """\
module World.Save.Component.Beta where

betaComponentId ∷ ComponentId
betaComponentId = ComponentId "beta"

data BetaDTO = BetaDTO
    { bdCount  ∷ !Int
    , bdQuirk  ∷ !Quirk
    , bdShared ∷ !SharedDTO
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)

betaCodec ∷ ComponentCodec BetaDTO
betaCodec = componentCodec ComponentSpec
    { csComponent     = betaComponentId
    , csVersion       = 1
    , csRequired      = False
    , csDeps          = []
    , csEncode        = id
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }
"""

QUIRK = """\
module Quirk.Types where

data Quirk = Quirk !Int

instance Serialize Quirk where
    put (Quirk n) = putWord8 (fromIntegral n)
    get = Quirk . fromIntegral <$> getWord8

data OffWire = OffA | OffB
    deriving stock (Show, Generic)
    deriving anyclass (Serialize)
"""

SAVE_TYPES = """\
module World.Save.Types where

data SaveMetadata = SaveMetadata
    { smName ∷ !Text
    , smSeed ∷ !Int
    , smExtra ∷ !Int
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)
"""

METADATA_V1 = """\
module World.Save.Compat.MetadataV1 where

data SaveMetadataV1 = SaveMetadataV1
    { smv1Name ∷ !Text
    , smv1Seed ∷ !Int
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)
"""

SESSION_V90 = """\
module World.Save.Compat.SessionV90 where

sessionComponentVersion ∷ Word32
sessionComponentVersion = 90

data SessionV90DTO = SessionV90DTO
    { s90Shared ∷ !SharedDTO
    , s90Name   ∷ !Text
    } deriving stock (Show, Generic)
      deriving anyclass (Serialize)
"""

LUA_ONE = """\
local saveMods = require("scripts.lib.save_modules")
saveMods.register("one", {
    version       = 2,
    inputVersions = { 1, 2 },
    required      = true,
    scope         = "global",
    snapshot = function() return { version = 9 } end,
})
"""

LUA_TWO = """\
local saveMods = require("scripts.lib.save_modules")
saveMods.register("two", {
    version       = 1,
    inputVersions = { 1 },
    required      = false,
    snapshot = function() return {} end,
})
"""

DATA_CODEC = """\
-- shared wire encoding
local M = {}
function M.encode(v) return tostring(v) end
return M
"""


def base_files() -> dict[str, str]:
    return {
        "synarchy.cabal": CABAL,
        "src/World/Save/Envelope.hs": ENVELOPE,
        "src/World/Save/Envelope/Types.hs": ENVELOPE_TYPES,
        "src/World/Save/Envelope/Codec.hs": ENVELOPE_CODEC,
        "src/World/Save/Component.hs": REGISTRY,
        "src/World/Save/Component/Types.hs": COMPONENT_TYPES,
        "src/World/Save/Component/Alpha.hs": ALPHA,
        "src/World/Save/Component/Beta.hs": BETA,
        "src/Quirk/Types.hs": QUIRK,
        "src/World/Save/Types.hs": SAVE_TYPES,
        "src/World/Save/Compat/MetadataV1.hs": METADATA_V1,
        "src/World/Save/Compat/SessionV90.hs": SESSION_V90,
        "app/Main.hs": "module Main where\n\nmain ∷ IO ()\nmain = pure ()\n",
        "scripts/one.lua": LUA_ONE,
        "scripts/two.lua": LUA_TWO,
        "scripts/lib/data_codec.lua": DATA_CODEC,
    }


def write_tree(root: Path, files: dict[str, str]) -> None:
    for rel, text in files.items():
        path = root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
