{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlLootTables
    ( LootTableYamlEntry(..)
    , LootTableYamlDef(..)
    , loadLootTableYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | One `{id, weight}` loot table entry. @weight@ is RELATIVE, not a
--   probability, and must be a finite, strictly positive number — see
--   'requireLootWeight' (#1946).
data LootTableYamlEntry = LootTableYamlEntry
    { ltyeId     ∷ !Text
    , ltyeWeight ∷ !Float
    } deriving (Show, Eq, Generic)

-- | Read an entry's REQUIRED @weight@ as a finite, strictly positive
--   number, diagnosing every rejection BY TABLE AND ENTRY (#1946).
--
--   The domain check has to live HERE, at the authoring boundary, and
--   not at the roll site. 'LootTable.Roll.pickByWeight' sums the
--   weights, scales the draw by that total, and walks the entries by
--   running sum with the last entry catching any overshoot
--   (@acc + w ≥ target ∨ null rest@). Every value outside this domain
--   makes that walk degenerate rather than fail:
--
--     * all-zero weights give @total = 0@ and @target = 0@, so
--       @0 + 0 ≥ 0@ and the FIRST entry wins every draw;
--     * a zero beside a positive weight is still selected at draw 0,
--       which is the one outcome an author writing @weight: 0@ was
--       trying to rule out;
--     * a negative weight shrinks the total, so entries after it span
--       running-sum ranges the draw can never reach;
--     * a non-finite weight poisons @total@ and @target@ into @NaN@,
--       every comparison is then false, and the LAST entry is returned
--       unconditionally through the overshoot fallback.
--
--   Clamping at the roll site cannot recover any of that — the sum is
--   already gone — and zero is NOT a disable toggle: an entry that
--   should not be drawn is deleted or commented out, exactly as #1721
--   settled for the sibling @count@ and @rolls@ multiplicities.
--   (Contrast a location's @max_count@, a placement BUDGET whose zero
--   has a documented meaning.)
--
--   Naming the TABLE and the ENTRY is the whole reason this is a named
--   parser rather than a @v .: "weight"@ plus a check, exactly as
--   'Engine.Asset.YamlFlora.requireRegrowthTime' and
--   'Engine.Asset.YamlItems.requirePositiveQuantity' are:
--   'loadLootTableYaml' supplies the failing FILE path in its warning,
--   but an ordinary Aeson field error only reaches for a JSON path like
--   @$.entries[3].weight@ — an index nobody can map back to an entry
--   without counting. The positional index is carried alongside the id
--   for the same reason #1721 carries both: it survives a duplicated or
--   copy-pasted id.
--
--   Taking the whole 'Aeson.Value' rather than decoding to 'Float'
--   first is deliberate for the same reason it is there: YAML's
--   @.nan@/@.inf@ resolve to STRINGS (the yaml package's scalar
--   resolver only recognizes ordinary numeric syntax), so decoding
--   first would surface those as a type error naming neither the table
--   nor what was actually wrong. Both numeric checks still run AFTER
--   narrowing to the stored 32-bit 'Float': an ordinary @1.0e+100@ is a
--   valid 'Scientific' that becomes @Infinity@ there, and an equally
--   ordinary @1.0e-60@ becomes @0.0@ — the value the runtime would
--   actually sum is the only one worth checking.
requireLootWeight
    ∷ Text          -- ^ the owning table's @id@, for the diagnostic
    → Int           -- ^ the entry's 1-based position in @entries@
    → Text          -- ^ the entry's own @id@
    → Aeson.Object
    → Aeson.Parser Float
requireLootWeight tableId entryIx entryId v = do
    mval ← v .:? "weight"
    case mval of
        Nothing  → bad "is required and has no default"
        Just val → case val of
            Aeson.Number s →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then bad ("must be finite, got " <> tshow val)
                     else if f ≤ 0
                       then bad ("must be strictly positive, got " <> tshow f)
                       else pure f
            _ → bad ("must be a relative weight number, got " <> tshow val)
  where
    bad why = fail ∘ T.unpack $
        "loot table '" <> tableId <> "': entry " <> tshow entryIx
        <> " ('" <> entryId <> "'): 'weight' " <> why

-- | Parse one entry, threading the OWNING table's id and the entry's
--   position through so a bad @weight@ is diagnosed by table and entry
--   rather than by list index alone. There is deliberately no
--   'FromJSON' instance: neither name is reachable from inside one,
--   which is the whole point (see 'requireLootWeight'), and a second
--   instance would be a decode path that skips the domain check.
parseLootTableYamlEntry
    ∷ Text → Int → Aeson.Value → Aeson.Parser LootTableYamlEntry
parseLootTableYamlEntry tableId entryIx =
    withObject "LootTableYamlEntry" $ \v → do
        eid ← v .: "id"
        LootTableYamlEntry eid ⊚ requireLootWeight tableId entryIx eid v

-- | The YAML shape of a loot table def. Unlike locations/items/units,
--   one file IS one def — no wrapping list — so the top-level document
--   parses directly into this type.
data LootTableYamlDef = LootTableYamlDef
    { ltydId      ∷ !Text
    , ltydEntries ∷ ![LootTableYamlEntry]
    } deriving (Show, Eq, Generic)

-- | The table's own @id@ is read FIRST so every entry rejection below
--   can name it. @entries: []@ stays valid: an empty table is a defined
--   outcome ('LootTable.Roll.pickByWeight' answers 'Nothing' for it),
--   not a malformed document.
instance FromJSON LootTableYamlDef where
    parseJSON = withObject "LootTableYamlDef" $ \v → do
        tid  ← v .: "id"
        vals ← v .: "entries"
        entries ← forM (zip [1 ∷ Int ..] vals) $ \(entryIx, val) →
            parseLootTableYamlEntry tid entryIx val
        pure (LootTableYamlDef tid entries)

-- | Parse one loot table YAML file. Returns 'Nothing' (with a logged
--   warning) on a parse failure — mirrors 'loadLocationYaml', except
--   there is only ever at most one def per file.
--
--   A rejected @weight@ (#1946) fails the WHOLE document here, so the
--   caller registers nothing and a previously loaded table of the same
--   id survives intact — 'Engine.Scripting.Lua.API.LootTables' inserts
--   only once this has answered 'Just'.
loadLootTableYaml ∷ LoggerState → FilePath → IO (Maybe LootTableYamlDef)
loadLootTableYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse loot table YAML "
                <> T.pack path <> ": " <> tshow err
            return Nothing
        Right def → do
            logDebug logger CatAsset $ "Loaded loot table '"
                <> ltydId def <> "' from " <> T.pack path
            return (Just def)
