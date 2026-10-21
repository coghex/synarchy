{-# LANGUAGE Strict #-}
-- | Shared fixtures for the "Faction relation store" gate (#2548): tag
--   and cause constructors that fail loudly on a bad fixture string, and
--   the cause sets the pure, codec and lifecycle specs share.
module Test.Headless.Faction.RelationStore.Fixture
    ( tg
    , cause
    , sampleCauses
    , sampleStore
    , otherStore
    ) where

import UPrelude
import Unit.Faction (FactionRelation(..))
import Unit.Faction.Profile
    ( FactionTag, LiveRelationCause(..), mkFactionTag, relationCauseId )
import Unit.Faction.RelationStore
    (RelationCauseStore, relationCauseStoreFromList)

-- | Fails loudly rather than silently degrading, so a fixture typo can
--   never make a case pass vacuously with fewer causes than it reads as
--   having.
tg ∷ Text → FactionTag
tg t = fromMaybe (error ("invalid fixture tag: " ⧺ show t)) (mkFactionTag t)

cause ∷ Text → Text → Text → FactionRelation → LiveRelationCause
cause owner s t r = LiveRelationCause
    { lrcCause    = relationCauseId owner
    , lrcSource   = tg s
    , lrcTarget   = tg t
    , lrcRelation = r }

-- | Five causes that between them exercise every identity rule: two
--   owners on one ordered pair, the reverse direction of that pair, one
--   owner on two pairs, and a pair of SHIPPED tags beside runtime-minted
--   ones nobody declared.
sampleCauses ∷ [LiveRelationCause]
sampleCauses =
    [ cause "order:attack_1"     "raid_a" "raid_b"  RelHostile
    , cause "report:incident_7"  "raid_a" "raid_b"  RelHostile
    , cause "diplomacy:truce_2"  "raid_b" "raid_a"  RelAlly
    , cause "order:attack_1"     "raid_a" "farmers" RelNeutral
    , cause "report:incident_7"  "nomad"  "acolyte" RelHostile
    ]

sampleStore ∷ RelationCauseStore
sampleStore = relationCauseStoreFromList sampleCauses

-- | A store sharing NO cause with 'sampleStore', for "load over a
--   different non-empty live store".
otherStore ∷ RelationCauseStore
otherStore = relationCauseStoreFromList
    [ cause "live:other_1" "team_x" "team_y" RelHostile
    , cause "live:other_2" "team_y" "team_x" RelAlly ]
