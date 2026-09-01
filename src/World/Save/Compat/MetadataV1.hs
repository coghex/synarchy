{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}

-- | Frozen v1 shape of the envelope's @"metadata"@ component (#913).
--
--   The @"metadata"@ component is the one component
--   "World.Save.Envelope" owns directly rather than routing through
--   'World.Save.Component.saveComponentRegistry' — it carries listing
--   metadata, not gameplay state, so it must stay readable without
--   decoding anything else. Its schema version reached 2 when #913
--   appended the durable autosave\/manual classification
--   ('World.Save.Types.smAutosave'); this module is that bump's frozen
--   PREDECESSOR shape plus its migration, following the same
--   frozen-DTO boundary rule "World.Save.Compat.SessionV90" states:
--   a historical wire layout is mirrored by a type that never changes
--   again, so a future edit to the LIVE 'SaveMetadata' can not silently
--   reinterpret bytes already on disk.
--
--   Schema version 3 (#2021) appended the per-page generated-world id
--   inventory; its own frozen predecessor is
--   "World.Save.Compat.MetadataV2", and this module stays v1's.
--
--   The migration itself is total and lossless in the only direction
--   that exists: v1 predates the classification entirely, and the
--   issue's contract for that case is explicit — \"legacy saves without
--   an autosave classification are manual saves\" — so every v1
--   generation decodes with 'World.Save.Types.smAutosave' 'False'. That
--   is not a guess: an autosave slot can only ever have been produced by
--   the scheduler this same issue introduces, which never wrote a v1
--   payload.
module World.Save.Compat.MetadataV1
    ( SaveMetadataV1(..)
    , decodeSaveMetadataV1
    , migrateSaveMetadataV1
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Save.Types (SaveMetadata(..))

-- | Frozen mirror of the @"metadata"@ component's v1 payload: exactly
--   'World.Save.Types.SaveMetadata' as it stood before #913, in its
--   then-current field order (stable since v82\/#707). Never change this
--   record — a further metadata schema change adds a NEW frozen type and
--   bumps the component version again.
data SaveMetadataV1 = SaveMetadataV1
    { sm1Name       ∷ !Text
    , sm1Seed       ∷ !Word64
    , sm1WorldSize  ∷ !Int
    , sm1PlateCount ∷ !Int
    , sm1Timestamp  ∷ !Text
    , sm1WorldName  ∷ !(Maybe Text)
    , sm1WorldGloss ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Decode a raw v1 @"metadata"@ payload, reporting a failure in the
--   same "Save format …" vocabulary the modern path uses.
decodeSaveMetadataV1 ∷ BS.ByteString → Either Text SaveMetadataV1
decodeSaveMetadataV1 bytes = case S.decode bytes of
    Left err → Left ("Failed to decode metadata component (v1): "
                        <> T.pack err)
    Right m  → Right m

-- | v1 → current. The one added field takes the documented legacy
--   answer: a save written before the classification existed is a
--   MANUAL save.
migrateSaveMetadataV1 ∷ SaveMetadataV1 → SaveMetadata
migrateSaveMetadataV1 m = SaveMetadata
    { smName       = sm1Name m
    , smSeed       = sm1Seed m
    , smWorldSize  = sm1WorldSize m
    , smPlateCount = sm1PlateCount m
    , smTimestamp  = sm1Timestamp m
    , smWorldName  = sm1WorldName m
    , smWorldGloss = sm1WorldGloss m
    , smAutosave   = False
    -- #2021: a v1 payload predates generated-world identity by two
    -- component versions; its pages carry no id either, and staging
    -- mints fresh ones. See "World.Save.Compat.MetadataV2".
    , smGeneratedWorldIds = []
    }
