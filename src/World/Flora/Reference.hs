{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | How a SAVE names a flora species (#2243).
--
--   'World.Flora.Types.FloraId' is a runtime handle: it is handed out
--   as the catalog is built, it is what every hot path and the whole
--   Lua boundary use, and it means nothing outside the session that
--   minted it. The catalog itself is never saved
--   (@docs\/persistence_state_inventory.md@), so a persisted ordinal is
--   only as stable as the order the loading build happens to register
--   species in — which is exactly what #2236 set out to remove.
--
--   The durable key is the authored YAML @name@ ('fsName'), the same
--   key #1854 already derives a 'World.Flora.Identity.FloraInstanceId'
--   from and #2241 made unique at content load. Everything a save
--   writes from now on names a species; the numeric alternative below
--   exists ONLY so a payload written before names can be carried
--   through decode unchanged and resolved once, against the loading
--   build's catalog, at the boundary that has one (D-2 in
--   @docs\/flora_species_identity_design.md@).
--
--   Where each spelling lives:
--
--   - the LIVE session (chunk replay, designation records, the Lua
--     verbs) holds 'World.Flora.Types.FloraId' and is untouched by this
--     module;
--   - a 'World.Save.Snapshot.SessionSnapshot', the legacy
--     'World.Save.Types.SaveData' bridge it becomes, and every current
--     component DTO hold 'FloraRef';
--   - 'World.Thread.Command.Save.WriteWorld' converts live → 'FloraRef'
--     at capture ('floraRefForId'), and 'World.Load.Stage' converts
--     'FloraRef' → live at staging ('resolveFloraRef'), each against
--     the catalog it reads for itself.
--
--   Both conversions are total only against a catalog that resolves
--   every reference, which is why each boundary validates first:
--   'World.Save.Types.missingFloraReferences' refuses a load, and
--   WriteWorld refuses a save, naming the reference that did not
--   resolve.
module World.Flora.Reference
    ( FloraRef(..)
    , renderFloraRef
    , floraRefForId
    , resolveFloraRef
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import World.Flora.Types
    ( FloraId(..), FloraCatalog, FloraSpecies(..)
    , findSpeciesByName, lookupSpecies )

-- | A durable reference to a flora species.
--
--   Append-only like every other positionally-serialized sum
--   (@tools\/enum_append_only_audit.py@): 'FloraByName' is tag 0 and is
--   the only one anything writes.
data FloraRef
    = FloraByName !Text
      -- ^ The species' authored YAML @name@. Every reference a save
      --   written by this build carries.
    | FloraByLegacyId !FloraId
      -- ^ The runtime ordinal a pre-#2243 payload recorded, carried
      --   through its component migration verbatim because a pure
      --   migration has no catalog to resolve it against. Resolved once
      --   against the LOADING build's catalog and never written back —
      --   the very next save of that session names the species (D-2).
    deriving (Show, Eq, Generic, Serialize, NFData)

-- | The reference as a load/save diagnostic renders it — a quoted name,
--   or the bare ordinal a legacy payload carried, so a reader can tell
--   at a glance which of the two failed to resolve.
renderFloraRef ∷ FloraRef → Text
renderFloraRef (FloraByName name)   = "species '" <> name <> "'"
renderFloraRef (FloraByLegacyId fi) =
    "legacy species id " <> tshow (unFloraId fi)

-- | The durable reference for a live species handle, or 'Nothing' when
--   the catalog does not know the handle at all.
--
--   Save-side only. 'Nothing' is a refusal, never a fallback: writing a
--   reference the loading build could not resolve is precisely what
--   this arc exists to stop, so
--   'World.Thread.Command.Save.WriteWorld' fails the save rather than
--   inventing a name (requirement 4).
floraRefForId ∷ FloraCatalog → FloraId → Maybe FloraRef
floraRefForId cat fid = FloraByName ∘ fsName <$> lookupSpecies fid cat

-- | The live handle a durable reference names in this build's catalog,
--   or 'Nothing' when it names nothing.
--
--   A name resolves through 'findSpeciesByName' — unique since #2241,
--   so the lookup is unambiguous. A legacy ordinal resolves by
--   EXISTENCE only: the number is reinterpreted in the loading
--   catalog's numbering, which may well name a different authored
--   species than the one that was planted. That is D-2, accepted and
--   documented rather than guarded, because the catalog that minted the
--   number was never saved and refusing every pre-name save would
--   strand them all.
resolveFloraRef ∷ FloraCatalog → FloraRef → Maybe FloraId
resolveFloraRef cat (FloraByName name)   = fst <$> findSpeciesByName name cat
resolveFloraRef cat (FloraByLegacyId fi) = fi <$ lookupSpecies fi cat
