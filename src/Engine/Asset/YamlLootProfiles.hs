{-# LANGUAGE Strict, DeriveGeneric #-}
-- | The @data/loot_profiles/*.yaml@ authoring boundary (#2499, epic
--   #1231 PLC-12).
--
--   Every rule here rejects the WHOLE file. A loot profile is one
--   document, and a partially admitted profile is a distribution nobody
--   authored: PLC-13 rolls each entry independently and sizes its lot
--   from the profile-level multiplier, so silently dropping one bad
--   entry would change what a container is worth without changing
--   anything an author can see. Whole-file rejection is also what makes
--   the registry's insert/replace policy safe — a rejected replacement
--   leaves the previously registered profile exactly as it was, the
--   same guarantee 'Engine.Asset.YamlLootTables' gives.
--
--   __Diagnostics carry coordinates, and only the real ones.__ The
--   failing FILE is supplied by 'loadLootProfileYaml' below; each rule
--   adds the profile id once it is known, and a 1-based entry index
--   only for an ENTRY-level rule. A missing @id@ and a bad
--   @quantity_multiplier@ have no entry to name and do not invent one.
--   That is the whole reason the rules are named parsers over the raw
--   'Aeson.Object' rather than @v .: "chance"@ plus a check: an
--   ordinary Aeson field error reaches only for a JSON path like
--   @$.entries[3].chance@, an index nobody can map back to an entry
--   without counting, and it cannot name the profile at all.
--
--   __Item ids are NOT resolved here.__ This module has no registry
--   access; the unknown-item rule lives at the loader
--   ('Engine.Scripting.Lua.API.LootProfiles'), which reads the item
--   registry through the reader capability and applies
--   'lootProfileItemErrors' below to whatever this module decoded.
module Engine.Asset.YamlLootProfiles
    ( LootProfileYamlEntry(..)
    , LootProfileYamlDef(..)
    , loadLootProfileYaml
    , lootProfileItemErrors
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Data.Yaml.Internal (Warning(..))
import Data.Aeson (FromJSON(..), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser, JSONPathElement(..), parseEither)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | One @{item, chance, quantity_factor}@ profile entry, as authored.
data LootProfileYamlEntry = LootProfileYamlEntry
    { lpyeItem           ∷ !Text
    , lpyeChance         ∷ !Float
    , lpyeQuantityFactor ∷ !Int
    } deriving (Show, Eq, Generic)

-- | The YAML shape of a loot profile def. Like a loot table and unlike
--   locations/items/units, one file IS one def — no wrapping list — so
--   the top-level document parses directly into this type.
data LootProfileYamlDef = LootProfileYamlDef
    { lpydId               ∷ !Text
    , lpydMultiplierMin    ∷ !Int
    , lpydMultiplierMax    ∷ !Int
    , lpydEntries          ∷ ![LootProfileYamlEntry]
    } deriving (Show, Eq, Generic)

-----------------------------------------------------------------------
-- The authored-field rules
--
-- Each answers 'Left' with the reason rather than failing, so the
-- CALLER supplies the coordinates it can legitimately name. They are
-- internal: the gate drives every one of them through the PRODUCTION
-- caller ('loadLootProfileYaml' over a real file), because that is
-- where the YAML→'Scientific'→'Float' narrowing each guard is written
-- around actually happens.
-----------------------------------------------------------------------

-- | Read a REQUIRED field as a whole number.
--
--   The lookup is an explicit 'KM.lookup' rather than @.:?@ on purpose:
--   @.:?@ reads an explicit @key: null@ as absent, and the two cases
--   deserve the same rejection here but for opposite reasons — one is
--   an author who forgot the field, the other an author who wrote it
--   and left it empty. Both reach the same message; neither is silently
--   defaulted.
--
--   Integrality is 'Aeson.fromJSON'\'s own @Int@ rule
--   (@Scientific.toBoundedInteger@): @5@ and @5.0@ are the same whole
--   number, @5.5@ is not one, and a magnitude beyond 'Int' is refused
--   rather than silently wrapped — which is what the message means by
--   "the engine can store". YAML's @.nan@/@.inf@ resolve to STRINGS
--   rather than numbers, so they land in this same branch and are named
--   as what they are.
wholeNumberField ∷ Text → Aeson.Object → Either Text Int
wholeNumberField key v = case KM.lookup (Key.fromText key) v of
    Nothing         → Left (quoted key <> " is required and has no default")
    Just Aeson.Null → Left (quoted key <> " is required and has no default")
    Just val        → case Aeson.fromJSON val ∷ Aeson.Result Int of
        Aeson.Success n → Right n
        Aeson.Error _   → Left (quoted key
                                <> " must be a whole number the engine can \
                                   \store, got " <> tshow val)

-- | A whole number that must be strictly positive — @quantity_factor@,
--   and the multiplier's own @min@.
--
--   Zero is NOT a disable toggle, exactly as #1721 settled for the
--   sibling @count@/@rolls@ multiplicities and #1946 for a loot-table
--   @weight@: an entry that should not appear is deleted, and a
--   zero-sized lot is a lot the admission pass would accept and then
--   have nothing to put in. A negative factor is worse — PLC-13 sizes a
--   lot by multiplying it, so it would produce a negative quantity no
--   container capacity check is written to refuse.
positiveWholeField ∷ Text → Aeson.Object → Either Text Int
positiveWholeField key v = wholeNumberField key v ⌦ \n →
    if n ≥ 1
      then Right n
      else Left (quoted key <> " must be a positive whole number, got "
                 <> tshow n)

-- | A REQUIRED independent appearance probability in @[0, 1]@,
--   endpoints INCLUDED: @0@ is an entry that is authored but cannot
--   appear (legitimate while a profile is being tuned) and @1@ an entry
--   that always does.
--
--   Both checks run AFTER narrowing to the stored 32-bit 'Float', for
--   the same reason @Engine.Asset.YamlLootTables.requireLootWeight@
--   narrows first: the value PLC-13 will actually compare a roll
--   against is the 'Float', and an ordinary @1.0e+100@ is a valid
--   'Scientific' that becomes @Infinity@ there. A non-finite chance
--   would make every comparison against it false, which is neither
--   "never appears" nor "always appears" but "appears or not depending
--   on which way the comparison was written".
chanceField ∷ Text → Aeson.Object → Either Text Float
chanceField key v = case KM.lookup (Key.fromText key) v of
    Nothing         → Left (quoted key <> " is required and has no default")
    Just Aeson.Null → Left (quoted key <> " is required and has no default")
    Just val        → case val of
        Aeson.Number s →
            let f = realToFrac s ∷ Float
            in if isNaN f ∨ isInfinite f
                 then Left (quoted key <> " must be finite, got " <> tshow val)
                 else if f < 0 ∨ f > 1
                   then Left (quoted key
                              <> " must be between 0 and 1 inclusive, got "
                              <> tshow f)
                   else Right f
        _ → Left (quoted key <> " must be a probability number, got "
                  <> tshow val)

-- | A REQUIRED non-empty id string.
idField ∷ Text → Aeson.Object → Either Text Text
idField key v = case KM.lookup (Key.fromText key) v of
    Nothing         → Left (quoted key <> " is required and has no default")
    Just Aeson.Null → Left (quoted key <> " is required and has no default")
    Just val        → case val of
        Aeson.String t
            | T.null t  → Left (quoted key <> " must not be empty")
            | otherwise → Right t
        _ → Left (quoted key <> " must be a name string, got " <> tshow val)

quoted ∷ Text → Text
quoted key = "'" <> key <> "'"

-----------------------------------------------------------------------
-- The document
-----------------------------------------------------------------------

-- | Parse one entry, threading the OWNING profile's id and the entry's
--   1-based position through so every rejection is diagnosed by profile
--   and entry rather than by list index alone. There is deliberately no
--   'FromJSON' instance: neither coordinate is reachable from inside
--   one, and a second instance would be a decode path that skips the
--   domain checks.
parseLootProfileYamlEntry
    ∷ Text → Int → Aeson.Value → Aeson.Parser LootProfileYamlEntry
parseLootProfileYamlEntry pid entryIx val = case val of
    Aeson.Object v → do
        -- The item id is read FIRST so the two rules after it can name
        -- the entry by what it holds. When it is unusable there is no
        -- honest name to print, and the coordinates stop at the index.
        item ← orFail (entryAt pid entryIx) (idField "item" v)
        let at = entryFor pid entryIx item
        LootProfileYamlEntry item
            ⊚ orFail at (chanceField "chance" v)
            ⊛ orFail at (positiveWholeField "quantity_factor" v)
    -- The entry is not a block at all. Matched HERE rather than left to
    -- @withObject@, which is the one shape that would have escaped this
    -- module's diagnostic contract: its own failure names neither the
    -- profile nor the entry, only aeson's @$@ path. Both coordinates are
    -- known at this point — the id was read before any entry was — so
    -- there is nothing to recover and every reason to print them.
    _ → fail ∘ T.unpack ∘ entryAt pid entryIx $
            "must be an {item, chance, quantity_factor} block, got "
            <> tshow val

-- | The profile-level @quantity_multiplier@ block: a required object
--   with required whole-number @min@ and @max@, @1 ≤ min ≤ max@.
--
--   Looked up explicitly rather than through @.:?@ because it is a
--   BLOCK with required sub-fields: @.:?@ reads @quantity_multiplier:
--   null@ as absent, and an absent block must be refused, not defaulted.
parseQuantityMultiplier ∷ Text → Aeson.Object → Aeson.Parser (Int, Int)
parseQuantityMultiplier pid v =
    case KM.lookup (Key.fromText "quantity_multiplier") v of
        Nothing → bad "'quantity_multiplier' is required and has no default"
        Just Aeson.Null →
            bad "'quantity_multiplier' is required and has no default"
        Just (Aeson.Object m) → do
            lo ← orFail (profileAt pid)
                        (prefixed "quantity_multiplier"
                                  (positiveWholeField "min" m))
            hi ← orFail (profileAt pid)
                        (prefixed "quantity_multiplier"
                                  (wholeNumberField "max" m))
            if hi ≥ lo
              then pure (lo, hi)
              else bad ("'quantity_multiplier': 'max' must be at least 'min' ("
                        <> tshow lo <> "), got " <> tshow hi)
        Just other →
            bad ("'quantity_multiplier' must be a {min, max} block, got "
                 <> tshow other)
  where
    bad why = fail (T.unpack (profileAt pid why))

-- | Name a sub-block's field the way the author wrote it, so
--   @'min' must be a positive whole number@ reads as
--   @'quantity_multiplier': 'min' …@ rather than losing which block it
--   came from.
prefixed ∷ Text → Either Text α → Either Text α
prefixed block = either (Left ∘ \why → quoted block <> ": " <> why) Right

-- | The three diagnostic coordinate spellings, kept together so no
--   caller invents a fourth. There is deliberately no spelling that
--   names an entry without a profile: an entry index is only meaningful
--   inside a named profile, and the document's @id@ is read before any
--   entry is.
profileAt ∷ Text → Text → Text
profileAt pid why = "loot profile '" <> pid <> "': " <> why

entryAt ∷ Text → Int → Text → Text
entryAt pid ix why = profileAt pid ("entry " <> tshow ix <> ": " <> why)

entryFor ∷ Text → Int → Text → Text → Text
entryFor pid ix item why =
    profileAt pid ("entry " <> tshow ix <> " ('" <> item <> "'): " <> why)

-- | Turn a rule's 'Left' into a parse failure carrying the coordinates
--   the caller supplies.
orFail ∷ (Text → Text) → Either Text α → Aeson.Parser α
orFail at = either (fail ∘ T.unpack ∘ at) pure

-- | The profile's own @id@ is read FIRST so every later rejection can
--   name it, and @entries@ must be a NON-EMPTY list.
--
--   An empty @entries@ is rejected where an empty loot TABLE is
--   accepted, and the difference is not an inconsistency: an empty
--   table is a defined outcome (its single weighted draw answers
--   'Nothing'), while an empty profile is a distribution that can only
--   ever realize nothing — a container paired with it would be
--   indistinguishable from one that was never rolled, which is exactly
--   the state PLC-13's realization flag exists to tell apart.
instance FromJSON LootProfileYamlDef where
    parseJSON = withObject "LootProfileYamlDef" $ \v → do
        pid ← either (fail ∘ T.unpack) pure (idField "id" v)
        (lo, hi) ← parseQuantityMultiplier pid v
        vals ← case KM.lookup (Key.fromText "entries") v of
            Nothing → fail (T.unpack (profileAt pid
                        "'entries' is required and has no default"))
            Just Aeson.Null → fail (T.unpack (profileAt pid
                        "'entries' is required and has no default"))
            Just (Aeson.Array a)
                | null a    → fail (T.unpack (profileAt pid
                                "'entries' must not be empty"))
                | otherwise → pure (V.toList a)
            Just other → fail (T.unpack (profileAt pid
                            ("'entries' must be a list, got " <> tshow other)))
        entries ← forM (zip [1 ∷ Int ..] vals) $ \(entryIx, val) →
            parseLootProfileYamlEntry pid entryIx val
        pure (LootProfileYamlDef pid lo hi entries)

-- | Every entry naming an item def id that is NOT registered, in
--   authored order, each diagnosed by profile and 1-based entry index.
--
--   Applied by the loader AFTER a successful decode, against the live
--   item registry. Items load before loot profiles (see
--   @scripts/startup_loader.lua@), so the registry this is handed is the
--   complete one — the same ordering #917 already relies on for a
--   location's guaranteed significant contents.
--
--   Rejecting the file rather than warning at realization time is D-20:
--   a profile is authored content with no runtime fallback, and an
--   entry naming a deleted item would otherwise silently shrink every
--   container that profile fills, in every world, forever.
lootProfileItemErrors ∷ HS.HashSet Text → LootProfileYamlDef → [Text]
lootProfileItemErrors registered def =
    [ entryFor (lpydId def) ix (lpyeItem e)
        "names no registered item definition"
    | (ix, e) ← zip [1 ∷ Int ..] (lpydEntries def)
    , not (HS.member (lpyeItem e) registered)
    ]

-- | Parse one loot profile YAML file. Returns 'Nothing' (with a logged
--   warning) on a parse failure — mirrors 'loadLootTableYaml', except
--   that a REPEATED key is a rejection here rather than a silent
--   last-one-wins.
--
--   That extra rule is why this decodes through
--   'Yaml.decodeFileWithWarnings' rather than 'Yaml.decodeFileEither'
--   (which is literally the former with the warnings discarded).
--   libyaml resolves a duplicated mapping key by keeping the LAST
--   binding, so a document that says @id@ twice decodes cleanly as
--   whichever id came second — and a file whose profile is not the one
--   its author is reading registers under a name nothing else in the
--   repository mentions. The warning list is the only place that
--   collision is visible. It is reported for a duplicate at ANY depth,
--   not only the document's own @id@: a repeated @chance@ inside an
--   entry is the same authoring mistake with the same silent outcome.
--
--   __Duplicates are settled BEFORE the typed parse runs, and that
--   ordering is load-bearing.__ The file is decoded to a plain
--   'Aeson.Value' first, which cannot fail on schema, so the warning
--   list is always in hand. Decoding straight to 'LootProfileYamlDef'
--   instead loses it on exactly the documents that need it most: a
--   validation failure answers 'Left' with the warnings discarded, so a
--   file that repeated @id@ AND authored a bad @quantity_multiplier@
--   would be rejected by a message quoting the last-wins id — the value
--   the duplicate rule exists to distrust — with the duplicate never
--   mentioned. Same for a repeated @item@ beside a bad @chance@.
--
--   The diagnostic carries the same coordinates every other rule here
--   carries, and for the same reason — but only the ones a duplicated
--   key leaves TRUSTWORTHY, which is what 'duplicateContext' below
--   decides. A duplicate inside an entry is named by profile, 1-based
--   entry index and item, exactly as a bad @chance@ in that same entry
--   would be, at any depth inside that entry.
loadLootProfileYaml ∷ LoggerState → FilePath → IO (Maybe LootProfileYamlDef)
loadLootProfileYaml logger path = do
    result ← Yaml.decodeFileWithWarnings path
    case result of
        Left err → reject (tshow err)
        Right (warnings, val) → case [ p | DuplicateKey p ← warnings ] of
            dups@(_:_) →
                let ctx = duplicateContext dups val
                in reject (T.intercalate "; " (map (duplicateAt ctx val) dups)
                           <> " — a repeated key silently keeps only the \
                              \last binding")
            [] → case Aeson.parseEither parseJSON val of
                Left err  → reject (T.pack err)
                Right def → do
                    logDebug logger CatAsset $ "Loaded loot profile '"
                        <> lpydId def <> "' from " <> T.pack path
                    return (Just def)
  where
    reject why = do
        logWarn logger CatAsset $ "Failed to parse loot profile YAML "
            <> T.pack path <> ": " <> why
        return Nothing

-- | Which coordinates a document's duplicated keys leave TRUSTWORTHY.
--
--   A duplicate is resolved by keeping the LAST binding, so the decoded
--   value of a duplicated key is precisely the value this whole
--   rejection exists to distrust. Naming a profile out of a document
--   that says @id@ twice would print whichever id happened to come
--   second — the exact confusion the rule is here to prevent — and the
--   same holds one level down: when the top-level @entries@ key is
--   itself duplicated, libyaml reports the duplicate keys inside BOTH
--   lists while only the last list decoded, so an @entries[0]@ index no
--   longer picks out the entry that warning came from.
--
--   The same reasoning reaches one level further down. An entry whose
--   own @item@ key repeated has a decoded item name that is likewise
--   just the last binding, so naming the entry BY that item would put
--   an ambiguous value where a coordinate belongs — and would do it for
--   every duplicate in that entry, not only the @item@ one.
--
--   Everything a duplicate did NOT touch stays nameable, which is why
--   this is three answers rather than one all-or-nothing flag.
data DuplicateContext = DuplicateContext
    { dcProfile        ∷ Maybe Text  -- ^ the id, unless @id@ itself repeated
    , dcEntries        ∷ Bool        -- ^ do entry indices still pick out entries?
    , dcAmbiguousItems ∷ [Int]       -- ^ 0-based entries whose @item@ repeated
    }

duplicateContext ∷ [Aeson.JSONPath] → Aeson.Value → DuplicateContext
duplicateContext dups val = DuplicateContext
    { dcProfile = if repeatedTopLevel "id" then Nothing else rawProfileId val
    , dcEntries = not (repeatedTopLevel "entries")
      -- A DIRECT @entries[i].item@ duplicate, and only that: a repeated
      -- @item@ nested inside some sub-block of the entry is a different
      -- key and leaves the entry's own item name alone.
    , dcAmbiguousItems =
        [ i | Just (i, [Aeson.Key k]) ← map entryPath dups
            , Key.toText k ≡ "item" ]
    }
  where
    repeatedTopLevel key = any (≡ [Aeson.Key (Key.fromText key)]) dups

-- | Split a duplicate's path into the 0-based entry index it sits under
--   and the path INSIDE that entry — at any depth, because a duplicate
--   nested in an entry's own sub-block is still that entry's, and
--   losing the entry coordinate for it would contradict the rule that
--   duplicates are reported at any depth.
entryPath ∷ Aeson.JSONPath → Maybe (Int, Aeson.JSONPath)
entryPath (Aeson.Key es : Aeson.Index i : inside)
    | Key.toText es ≡ "entries" = Just (i, inside)
entryPath _ = Nothing

-- | The document's authored @id@, straight off the RAW value, when it
--   is a usable string. Read here rather than off a decoded definition
--   because duplicates are settled before anything is decoded.
rawProfileId ∷ Aeson.Value → Maybe Text
rawProfileId (Aeson.Object o) = case KM.lookup (Key.fromText "id") o of
    Just (Aeson.String t) | not (T.null t) → Just t
    _                                     → Nothing
rawProfileId _ = Nothing

-- | The authored @item@ of the 0-based entry @i@, when it is a usable
--   string. Same reason as 'rawProfileId'.
rawEntryItem ∷ Aeson.Value → Int → Maybe Text
rawEntryItem (Aeson.Object o) i = do
    Aeson.Array es ← KM.lookup (Key.fromText "entries") o
    Aeson.Object e ← es V.!? i
    Aeson.String t ← KM.lookup (Key.fromText "item") e
    guard (not (T.null t))
    pure t
rawEntryItem _ _ = Nothing

-- | One duplicated key's diagnostic, at the finest coordinates
--   'DuplicateContext' allows.
duplicateAt ∷ DuplicateContext → Aeson.Value → Aeson.JSONPath → Text
duplicateAt ctx val path = case dcProfile ctx of
    -- The document's own name is one of the duplicated keys, so there
    -- is nothing to call this profile. The raw path is all there is.
    Nothing  → rawPath
    Just pid → case entryPath path of
        Just (i, inside) | dcEntries ctx → entryCoord pid i (inside `orRaw` i)
        _ → case path of
            [Aeson.Key k] → profileAt pid (dup k)
            [Aeson.Key blk, Aeson.Key k]
                | Key.toText blk ≡ "quantity_multiplier" →
                    profileAt pid (quoted (Key.toText blk) <> ": " <> dup k)
            _ → profileAt pid rawPath
  where
    dup k   = "duplicate key " <> quoted (Key.toText k)
    rawPath = "duplicate key at YAML path " <> renderPath path

    -- The item names the entry only when the item itself is not one of
    -- the ambiguous values.
    entryCoord pid i = case rawEntryItem val i of
        Just item | i `notElem` dcAmbiguousItems ctx → entryFor pid (i + 1) item
        _                                            → entryAt pid (i + 1)

    -- Inside an entry the duplicated key is the LAST path element;
    -- anything before it is the sub-block it sits in, kept so a NESTED
    -- duplicate still says where without losing the entry coordinate.
    orRaw inside i = case reverse inside of
        [Aeson.Key k]            → dup k
        (Aeson.Key k : outer)    → dup k <> " under "
                                     <> renderPath (reverse outer)
        _                        → "duplicate key at YAML path "
                                     <> renderPath (Aeson.Key (Key.fromText "entries")
                                                    : Aeson.Index i : inside)

-- | A duplicate key's raw location, for the cases 'duplicateAt' cannot
--   give real coordinates to. Written out rather than taken from
--   Aeson's own path formatter so the rendering is pinned by this
--   module's own gate.
--
--   Array elements are 0-based here because that is what libyaml
--   reported: this is the raw YAML PATH, printed only when the 1-based
--   entry coordinate would be a guess, and renumbering a path is not
--   something an author could then find.
renderPath ∷ Aeson.JSONPath → Text
renderPath = T.concat ∘ map element
  where
    element (Aeson.Key k)   = "." <> Key.toText k
    element (Aeson.Index i) = "[" <> tshow i <> "]"
