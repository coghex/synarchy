{-# LANGUAGE Strict #-}
-- | The ONE production boundary for minting a complete 'ItemInstance'
--   (#1418, epic #1231 PLC-3).
--
--   Eight production mint sites — starting inventory, starting
--   equipment, starting accessories, dig yields, craft outputs, forage
--   harvest, @unit.addItem@ and @item.spawnGround@ — used to build the
--   record by hand, which is how seven of them ended up never
--   materialising a definition's authored @contents:@ while the eighth
--   did. They all go through 'materializeItem' now, so a first-aid kit
--   arrives stocked however it came into the world, and a field added to
--   'ItemInstance' has exactly one place to be filled in.
--
--   The only production 'ItemInstance' record construction outside this
--   module is @fromItemInstanceDTO@ in
--   "World.Save.Component.PageActivity",
--   which REBUILDS a previously materialised instance from its saved
--   fields rather than minting a new one; the structural guard in
--   @Test.Headless.Item.Materialize@ allows exactly those two.
--
--   Two dependencies are taken as EXPLICIT parameters rather than
--   projected out of 'Engine.Core.State.EngineEnv':
--
--   * the stat RNG, because the world-side dig consumer is deliberately
--     handed @statRNGRef@ as a narrow parameter (see the
--     explicit-narrow-parameter rule on
--     "Engine.Core.Capability.UnitCombat") and must not acquire
--     unit\/combat access by way of the materializer;
--   * the instance-id allocator, so this module needs no engine
--     environment at all and every caller keeps allocating from the one
--     'Engine.Core.State.freshItemInstanceId' counter it already used.
--
--   The shared logger is a third explicit parameter, for the one thing
--   this module reports: a CYCLIC definition graph (#1420). Authored
--   default contents recurse by definition NAME, so a definition listing
--   itself — or two listing each other — used to recurse forever and
--   hang the minting thread with no error and no log line. See
--   'materializeEntries' for the rule and why it is scoped the way it
--   is.
module Item.Materialize
    ( ItemOverrides(..)
    , pristineItem
    , filledItem
    , pristineCondition
    , pristineSharpness
    , materializeItem
    ) where

import UPrelude
import Data.IORef (IORef)
import qualified Data.Text as T
import System.Random (StdGen)
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types
    ( ItemContainer(..), ItemContentEntry(..), ItemDef(..)
    , ItemInstance(..), ItemManager, lookupItemDef )

-- | The caller-supplied values one mint site contributes. Every field is
--   ROOT-SCOPED (#1418 requirement 5): it applies to the instance the
--   caller asked for and to NOTHING materialised beneath it, so a ground
--   item's explicit Lua @quality@ prop or a crafted item's output
--   temperature never leaks into the default contents the root spawns
--   holding.
--
--   The one value a default-content CHILD may carry is its own @fill@,
--   which the materializer takes from the authored 'ItemContentEntry'
--   rather than from here.
data ItemOverrides = ItemOverrides
    { ovFill      ∷ !(Maybe Float)
      -- ^ Explicit fill. 'Nothing' takes the definition's own
      --   @default_fill@ rule; either way the value is clamped to the
      --   container's capacity and forced to 0 for non-containers.
    , ovQuality   ∷ !(Maybe Float)
      -- ^ Explicit quality, used VERBATIM and consuming no draw.
      --   'Nothing' rolls from the definition's @quality:@ spec (100 when
      --   it declares none). The ground-salvage path resolves its own
      --   quality by #1421's rule and passes the result here.
    , ovCondition ∷ !(Maybe Float)
      -- ^ Explicit condition. 'Nothing' is 'pristineCondition' — which
      --   since #1421 is what every freshly made item gets, the
      --   ground-salvage path being the one exception.
    , ovTemp      ∷ !(Maybe Float)
      -- ^ Tracked spawn temperature (°C); 'Nothing' spawns at ambient.
    }

-- | A freshly made item with nothing overridden: the definition's own
--   fill rule, a rolled quality, full condition, ambient temperature.
pristineItem ∷ ItemOverrides
pristineItem = ItemOverrides
    { ovFill = Nothing, ovQuality = Nothing
    , ovCondition = Nothing, ovTemp = Nothing }

-- | 'pristineItem' with an explicit fill the caller supplied (or did
--   not: @filledItem Nothing ≡ pristineItem@).
filledItem ∷ Maybe Float → ItemOverrides
filledItem mFill = pristineItem { ovFill = mFill }

-- | What a freshly made item's condition is (#1421).
pristineCondition ∷ Float
pristineCondition = 100.0

-- | What a freshly made item's edge keenness is: a factory edge, 100% of
--   the definition's base sharpness.
pristineSharpness ∷ Float
pristineSharpness = 100.0

-- | Mint one complete 'ItemInstance' from a definition name, together
--   with its authored default contents to the full authored depth.
--   'Nothing' for an unknown name — and an unknown name consumes no
--   instance id, exactly as the hand-written sites did.
--
--   Draw order per node is quality, then weight, then the whole content
--   subtree, then the instance id. The id is therefore allocated AFTER
--   its descendants': ids are unique and monotonic BY ALLOCATION, and no
--   caller may read parent-before-child ordering into them (requirement
--   4).
materializeItem
    ∷ ItemManager
    → LoggerState   -- ^ where a cyclic definition graph is reported (#1420)
    → IORef StdGen   -- ^ the stat RNG this call site draws from
    → IO Word64      -- ^ instance-id allocator (@freshItemInstanceId env@)
    → ItemOverrides
    → Text
    → IO (Maybe ItemInstance)
materializeItem itemMgr logger rngRef allocId ovr name =
    materializeNode itemMgr logger rngRef allocId [] ovr Nothing name

-- | The shared root\/child body. @mEntries@ is the child list to
--   materialise: 'Nothing' means "this node did not author one, so use
--   the definition's own 'idDefaultContents'".
--
--   @expanding@ is the cycle ancestry (#1420): the definition names
--   whose OWN 'idDefaultContents' are being expanded on the path to
--   here, outermost first. It grows by exactly the name of each node
--   that delegates to its definition, which is the only step that can
--   revisit a definition, and is what 'materializeEntries' tests a
--   delegating child against.
materializeNode
    ∷ ItemManager → LoggerState → IORef StdGen → IO Word64 → [Text]
    → ItemOverrides → Maybe [ItemContentEntry] → Text
    → IO (Maybe ItemInstance)
materializeNode itemMgr logger rngRef allocId expanding ovr mEntries name =
    case lookupItemDef name itemMgr of
        Nothing  → return Nothing
        Just def → do
            qual ← case ovQuality ovr of
                Just q  → return q
                Nothing → rollItemSpec (idQualitySpec def) rngRef
            wght ← rollItemWeight def rngRef
            let (entries, expanding') = case mEntries of
                    Just es → (es, expanding)
                    Nothing → (idDefaultContents def, expanding ⧺ [name])
            contents ← materializeEntries itemMgr logger rngRef allocId
                            expanding' entries
            iid ← allocId
            return $ Just ItemInstance
                { iiDefName     = name
                , iiCurrentFill = resolveDefaultFill def (ovFill ovr)
                , iiQuality     = qual
                , iiCondition   = fromMaybe pristineCondition (ovCondition ovr)
                , iiWeight      = wght
                , iiSharpness   = pristineSharpness
                , iiContents    = contents
                , iiInstanceId  = iid
                , iiTemp        = ovTemp ovr
                  -- #1233: the physical values are SNAPSHOTTED from the
                  -- definition here, exactly like iiWeight above, so a
                  -- later content edit never re-values this instance.
                , iiBulk        = Just (idBulk def)
                , iiStorage     = idStorage def
                }

-- | Materialise an authored content list. Each entry contributes
--   @count@ INDEPENDENT subtrees (never one tree referenced twice); a
--   non-positive count contributes none, and an entry naming an unknown
--   definition is silently dropped — the same skip the hand-written
--   sites did, so a typo in one line of a kit's loadout never costs the
--   kit.
--
--   A CYCLIC entry (#1420) is dropped the same way, with a warning: an
--   entry that DELEGATES to its child definition (no authored
--   @contents:@ of its own) and names a definition already being
--   expanded on this path would re-enter that definition's defaults and
--   never terminate. Only that back edge is dropped — the node holding
--   it, its siblings, and everything above it still materialise, so
--   @a → a@ yields an @a@ without that child and @a → b → a@ keeps the
--   outer @a@ and the nested @b@, minus @b@'s reference back to @a@.
--
--   Two scoping decisions are load-bearing, and both follow from
--   requirement 3 (an acyclic graph materialises exactly as before):
--
--   * the ancestry is the current PATH, never a global visited set. One
--     definition may legitimately appear in separate branches or twice
--     under one @count@ — the shipped first-aid kit does — and a global
--     set would silently drop those.
--   * only a DELEGATING entry is tested. An entry with its own authored
--     @contents:@ does not consult the named definition's defaults at
--     all, so it is a finite authored subtree that terminates on its
--     own; a crate authored holding a crate is legal nesting, not a
--     cycle, and blocking it would drop contents an author explicitly
--     wrote.
--
--   The warning is emitted ONCE per rejected entry, here at the point of
--   detection: no unwinding frame or caller re-reports it, and a @count@
--   of ten repetitions of one bad entry is still one authoring mistake.
materializeEntries
    ∷ ItemManager → LoggerState → IORef StdGen → IO Word64 → [Text]
    → [ItemContentEntry] → IO [ItemInstance]
materializeEntries itemMgr logger rngRef allocId expanding entries =
    catMaybes ∘ concat ⊚ mapM one entries
  where
    one e = case cycleThrough e of
        Just seg → do
            warnCycle logger seg (iceItem e)
            return []
        Nothing → replicateM (max 0 (iceCount e)) $
            materializeNode itemMgr logger rngRef allocId expanding
                (filledItem (iceFill e)) (iceContents e) (iceItem e)
    -- The closed cycle segment this entry would re-enter, if any: the
    -- ancestry from the repeated definition onwards. The acyclic prefix
    -- that merely LED here is dropped — it is not part of the cycle.
    cycleThrough e
        | isJust (iceContents e) = Nothing
        | otherwise = case dropWhile (≢ iceItem e) expanding of
            []  → Nothing
            seg → Just seg

-- | Report one cyclic default-contents entry. @seg@ is the ancestry
--   from the repeated definition onwards, so @seg ⧺ [name]@ closes the
--   loop and names every definition taking part in it.
warnCycle ∷ LoggerState → [Text] → Text → IO ()
warnCycle logger seg name =
    logWarn logger CatAsset $
        "Item default contents: cyclic contents graph "
        <> T.intercalate " → " (map quoted (seg ⧺ [name]))
        <> " — dropping the repeated " <> quoted name <> " entry"
  where quoted t = "'" <> t <> "'"

-- | The fill a fresh instance holds: an explicit caller\/entry fill
--   wins, else the container's authored @default_fill@ (a quinoa sack
--   spawns full, a canteen defaults empty). Both clamp to the
--   container's capacity; a non-container is always 0.
resolveDefaultFill ∷ ItemDef → Maybe Float → Float
resolveDefaultFill def mFill = case idContainer def of
    Just c  → max 0 (min (icCapacity c) (fromMaybe (icDefaultFill c) mFill))
    Nothing → 0
