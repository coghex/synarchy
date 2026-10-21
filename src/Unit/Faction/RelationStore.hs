{-# LANGUAGE Strict #-}
-- | The live directed relation-cause store (#2548, FTS-6B of the
--   faction-tag arc #2496): the ONE authoritative collection of every
--   live directed cause, and the overlay "Unit.Faction.Profile"'s
--   relation authority takes as an input.
--
--   FTS-1 (#2500) defined a cause as an identity plus a relation value
--   keyed by an ORDERED (source tag, target tag) pair, reduced
--   @hostile > neutral > ally@; FTS-5 (#2521) made every relation
--   consumer read one authority that accepts a list of
--   'LiveRelationCause's, and left that list empty at runtime. This
--   module is the value that fills it: a pure store with no thread, no
--   @IORef@ and no persistence of its own. The @IORef@ that holds it
--   lives on 'Engine.Core.State.EngineEnv', and the wire form is
--   "World.Save.Component.RelationCauses"'s frozen DTO.
--
--   __Identity and merge.__ A cause is keyed by its ordered tag pair AND
--   its 'RelationCauseId' — the identity the owning system minted — so
--   one owner can hold a cause on several pairs and two owners can hold
--   one on the same pair. Re-adding a key merges the two values by
--   'moreSevere', exactly as 'addRelationCause' does, so insertion order
--   cannot change an answer and a friendlier re-add never softens a
--   hostile cause (D-15, D-23). Softening is removal. There is no
--   last-write-wins and no identity deduplication across pairs.
--
--   __The store never contains a malformed tag__ because it holds only
--   'FactionTag's, which only 'mkFactionTag' mints. The boundary that
--   takes TEXT — a script, a saved payload — is 'mkLiveRelationCause',
--   which names the offending tag instead of dropping the cause.
--
--   Canonical form: a pair with no cause left is absent, never an empty
--   map, so derived 'Eq' means "same causes" and a removal that undoes
--   an add returns the earlier store exactly.
module Unit.Faction.RelationStore
    ( RelationCauseStore
    , emptyRelationCauseStore
    , relationCauseStoreNull
    , relationCauseStoreOverlay
    , relationCauseStoreFromList
      -- * The text boundary
    , mkLiveRelationCause
      -- * Operations
    , addStoredCause
    , removeCausesByOwner
    , clearPairByOwner
    ) where

import UPrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Text as T
import Unit.Faction (FactionRelation(..))
import Unit.Faction.Profile
    ( FactionTag, LiveRelationCause(..), RelationCauseId
    , factionTagText, mkFactionTag, moreSevere, relationCauseIdText
    , relationCauseId )

-- | Every live cause, by ordered tag pair then owner identity. Opaque, so
--   the canonical-form invariant above cannot be broken from outside.
newtype RelationCauseStore = RelationCauseStore
    (Map (FactionTag, FactionTag) (Map RelationCauseId FactionRelation))
    deriving (Show, Eq)

-- | No cause exists. This is the state of every session that predates
--   the store, and of a fresh one.
emptyRelationCauseStore ∷ RelationCauseStore
emptyRelationCauseStore = RelationCauseStore Map.empty

relationCauseStoreNull ∷ RelationCauseStore → Bool
relationCauseStoreNull (RelationCauseStore m) = Map.null m

-- | The overlay "Unit.Faction.Profile"'s 'Unit.Faction.Profile.withLiveCauses'
--   takes, in one deterministic order (pair, then owner). Installing it
--   through 'Unit.Faction.Profile.addRelationCause' reproduces this
--   store's merged answers exactly, because the store already holds the
--   merged value per key.
relationCauseStoreOverlay ∷ RelationCauseStore → [LiveRelationCause]
relationCauseStoreOverlay (RelationCauseStore m) =
    [ LiveRelationCause cid s t r
    | ((s, t), causes) ← Map.toAscList m
    , (cid, r) ← Map.toAscList causes ]

-- | Every cause through 'addStoredCause', so the result is the same for
--   any permutation of the input.
relationCauseStoreFromList ∷ [LiveRelationCause] → RelationCauseStore
relationCauseStoreFromList =
    foldl' (flip addStoredCause) emptyRelationCauseStore

-- | A cause from text. 'Left' names the first malformed tag (source
--   before target) so a caller can refuse the whole request; nothing is
--   ever dropped or repaired.
mkLiveRelationCause ∷ RelationCauseId → Text → Text → FactionRelation
                    → Either Text LiveRelationCause
mkLiveRelationCause cid s t r = do
    src ← tag "source" s
    tgt ← tag "target" t
    pure (LiveRelationCause cid src tgt r)
  where
    tag role txt = maybe (Left (malformed role txt)) Right (mkFactionTag txt)
    malformed role txt =
        "cause '" <> relationCauseIdText cid <> "' has a malformed "
            <> role <> " tag " <> tshow txt
            <> " (a tag is non-empty and contains no whitespace)"

-- | Install one cause. Idempotent on its (pair, identity) key; a re-add
--   naming a different relation merges by 'moreSevere'.
addStoredCause ∷ LiveRelationCause → RelationCauseStore → RelationCauseStore
addStoredCause c (RelationCauseStore m) = RelationCauseStore $
    Map.insertWith (Map.unionWith moreSevere)
                   (lrcSource c, lrcTarget c)
                   (Map.singleton (lrcCause c) (lrcRelation c)) m

-- | Remove every cause one owner holds, on every pair. Another owner's
--   cause on the same pair is untouched, and removing an owner that
--   holds nothing is a no-op.
removeCausesByOwner ∷ RelationCauseId → RelationCauseStore → RelationCauseStore
removeCausesByOwner cid (RelationCauseStore m) =
    RelationCauseStore (Map.mapMaybe prune m)
  where prune causes = nonEmpty (Map.delete cid causes)

-- | Remove one owner's cause on exactly one ordered pair — never the
--   reverse direction, another pair of the same owner, or another
--   owner's cause on this pair.
clearPairByOwner ∷ RelationCauseId → FactionTag → FactionTag
                 → RelationCauseStore → RelationCauseStore
clearPairByOwner cid s t (RelationCauseStore m) =
    RelationCauseStore (Map.update (nonEmpty ∘ Map.delete cid) (s, t) m)

nonEmpty ∷ Map k v → Maybe (Map k v)
nonEmpty x | Map.null x = Nothing
           | otherwise  = Just x

-- | The transitional in-memory load bridge ('World.Save.Types.SaveData')
--   derives 'Serialize' wholesale, so the store needs an instance. It is
--   NOT the save wire format — that is the frozen DTO of
--   "World.Save.Component.RelationCauses", which no live type reaches.
--   Decoding goes back through 'mkFactionTag' and rejects an
--   unrecognised relation byte, so a malformed tag can never enter by
--   this door either.
instance S.Serialize RelationCauseStore where
    put = S.put ∘ map row ∘ relationCauseStoreOverlay
      where
        row c = ( relationCauseIdText (lrcCause c)
                , factionTagText (lrcSource c)
                , factionTagText (lrcTarget c)
                , relationByte (lrcRelation c) )
    get = do
        rows ← S.get
        causes ← forM rows $ \(cid, s, t, b) → do
            r ← maybe (fail ("unknown relation byte " ⧺ show b)) pure
                      (relationFromByte b)
            either (fail ∘ T.unpack) pure
                   (mkLiveRelationCause (relationCauseId cid) s t r)
        pure (relationCauseStoreFromList causes)

relationByte ∷ FactionRelation → Word8
relationByte r = case r of
    RelAlly    → 0
    RelNeutral → 1
    RelHostile → 2

relationFromByte ∷ Word8 → Maybe FactionRelation
relationFromByte b = case b of
    0 → Just RelAlly
    1 → Just RelNeutral
    2 → Just RelHostile
    _ → Nothing
