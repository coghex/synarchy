{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}

-- | Frozen v2 shape of the envelope's @"metadata"@ component (#2021).
--
--   The @"metadata"@ component reached v3 when #2021 appended the
--   per-page generated-world id inventory
--   ('World.Save.Types.smGeneratedWorldIds'); this module is that bump's
--   frozen PREDECESSOR shape plus its migration, exactly as
--   "World.Save.Compat.MetadataV1" is v2's. The frozen-DTO boundary rule
--   ("World.Save.Compat.SessionV90") is why both exist rather than one:
--   a historical wire layout is mirrored by a type that never changes
--   again, so a future edit to the LIVE 'World.Save.Types.SaveMetadata'
--   cannot silently reinterpret bytes already on disk. Note that this
--   type is NOT reachable by widening 'SaveMetadataV1' — cereal encodes
--   these records positionally, so each accepted version needs its own
--   frozen copy of the exact field list it was written with.
--
--   The migration is total, and its one added field takes the only
--   honest answer: EMPTY. A v2 payload predates generated-world identity
--   entirely, so it names no generated foundation — and inventing one
--   here would be worse than absence, because a metadata id is supposed
--   to mirror an authoritative @world-pages@ id, and a v2 save's pages
--   have none either. The pages get their fresh ids one step later, at
--   transactional load staging ("World.Load.Stage"), and an ordinary
--   save afterwards is what first writes both copies.
module World.Save.Compat.MetadataV2
    ( SaveMetadataV2(..)
    , decodeSaveMetadataV2
    , migrateSaveMetadataV2
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Save.Types (SaveMetadata(..))

-- | Frozen mirror of the @"metadata"@ component's v2 payload: exactly
--   'World.Save.Types.SaveMetadata' as it stood between #913 and #2021.
--   Never change this record — a further metadata schema change adds a
--   NEW frozen type and bumps the component version again.
data SaveMetadataV2 = SaveMetadataV2
    { sm2Name       ∷ !Text
    , sm2Seed       ∷ !Word64
    , sm2WorldSize  ∷ !Int
    , sm2PlateCount ∷ !Int
    , sm2Timestamp  ∷ !Text
    , sm2WorldName  ∷ !(Maybe Text)
    , sm2WorldGloss ∷ !(Maybe Text)
    , sm2Autosave   ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | Decode a raw v2 @"metadata"@ payload, reporting a failure in the
--   same "Save format …" vocabulary the modern path uses.
decodeSaveMetadataV2 ∷ BS.ByteString → Either Text SaveMetadataV2
decodeSaveMetadataV2 bytes = case S.decode bytes of
    Left err → Left ("Failed to decode metadata component (v2): "
                        <> T.pack err)
    Right m  → Right m

-- | v2 → current. Every carried field rides across verbatim; the one
--   added field is empty, for the reason this module's header states.
migrateSaveMetadataV2 ∷ SaveMetadataV2 → SaveMetadata
migrateSaveMetadataV2 m = SaveMetadata
    { smName       = sm2Name m
    , smSeed       = sm2Seed m
    , smWorldSize  = sm2WorldSize m
    , smPlateCount = sm2PlateCount m
    , smTimestamp  = sm2Timestamp m
    , smWorldName  = sm2WorldName m
    , smWorldGloss = sm2WorldGloss m
    , smAutosave   = sm2Autosave m
    , smGeneratedWorldIds = []
    }
