{-# LANGUAGE Strict #-}
-- | Ground items — items lying in the world (see Item.Ground): spawn,
--   list, remove, temperature tracking, and pickup into a unit's
--   inventory. Split from Engine.Scripting.Lua.API.Items (#577) — item
--   def loading lives in Items.Defs, selection/render-introspection in
--   Items.Render.
module Engine.Scripting.Lua.API.Items.Ground
    ( itemSpawnGroundFn
    , itemListGroundFn
    , itemRemoveGroundFn
    , itemGroundCountFn
    , itemGetGroundTempFn
    , itemSetGroundTempFn
    , itemPickupGroundFn
    , itemGetGroundForUnitFn
    , pickupGroundOnPage
    ) where

import UPrelude
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv, activeWorldStateFrom, freshItemInstanceId)
import Engine.Scripting.Lua.API.Units.Page (unitOwningWorldState)
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem
                   , removeGroundItem)
import Item.Materialize (ItemOverrides(..), materializeItem, pristineItem)
import Item.Roll (GroundConditionBase, mkGroundConditionBase
                 , rollGroundCondition, rollGroundQuality)
import Item.Temperature (effectiveItemTemp)
import Item.Types
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Cursor.Types (CursorState(..))
import World.Types (WorldManager(..), WorldState(..), WorldPageId(..)
                   , WorldGenParams(..), wmWorlds)
import World.Weather.Ambient (ambientTempAt)

-- | Resolve which world page a ground-item op targets: a named page
--   (any in wmWorlds, even hidden / non-active) when a page-id is
--   given, else the active world. Location content-spawning (#90)
--   passes the page id so an item spawned into a hidden secondary
--   page's location lands on THAT page — mirrors
--   'Engine.Scripting.Lua.API.Structure.resolveStructurePage'.
resolveItemPage ∷ EngineEnv → Maybe Text → IO (Maybe WorldState)
resolveItemPage env (Just pid) = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    pure $ lookup (WorldPageId pid) (wmWorlds mgr)
resolveItemPage env Nothing = activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))

-- | item.spawnGround(defName, x, y [, props] [, pageId]) → gid | nil
--   Spawns an item into the world at float tile coords. Optional
--   props table: fill, quality, condition and temp (°C — spawns the
--   item hot/cold; omitted = at ambient, #344).
--   Resting height derives from terrain at render time, so items on
--   slopes sit on the incline and items over dug tiles drop with
--   the terrain. An explicit pageId (slot 5) pins the spawn to that
--   live page (even hidden) instead of the active world — location
--   content-spawning (#90) passes its own page so an item lands on
--   the page its location is on.
--
--   This is the SALVAGE path (#1421) — the one creation site that does
--   not produce a pristine item, because something found lying in the
--   world is pre-owned, not new — and it resolves quality and condition
--   by two DELIBERATELY DIFFERENT rules:
--
--   * @quality@ — an explicit prop REPLACES the roll (the caller is
--     naming the item's workmanship outright), else the definition's
--     own spec, else 'groundQualityFallbackRange'.
--   * @condition@ — TWO independent draws, arithmetically combined:
--     @base − rand(0,20)@ clamped to [0,100], where @base@ is the
--     caller's explicit prop or a @rand(80,100)@ draw. An explicit prop
--     names what the item STARTED at, so it never suppresses the
--     penalty.
--
--   Both live in "Item.Roll" beside the rest of the roll logic.
--
--   An explicit @condition@ outside its declared 0-100 domain — above,
--   below, NaN or either infinity — is REFUSED (#1790): the call
--   answers @nil@, the same failure shape an unknown definition or an
--   unresolvable page already answers with. It cannot be accepted and
--   corrected, because the clamp inside @salvageCondition@ turns any
--   base above 100 into exactly 100 for every allowed penalty — which
--   is the pristine guarantee #1421 says this path does not offer —
--   and turns a non-finite base into a 0 or a 100 no downstream range
--   check can tell from a real roll.
--
--   That check runs BEFORE the definition lookup, before EITHER roll,
--   and before any id is allocated, so a refused spawn spends no draw
--   from the shared stat RNG (quality is rolled first, so a check
--   sitting with the condition roll would already be too late) and
--   leaves the page's ground items, its id allocator and the
--   item-instance counter exactly as they were.
itemSpawnGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSpawnGroundFn env = do
    nameArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    propsTy ← Lua.ltype 4
    pageArg ← Lua.tostring 5
    let getMaybeProp ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Float)
        getMaybeProp key = case propsTy of
            Lua.TypeTable → do
                _ ← Lua.getfield 4 key
                mv ← Lua.tonumber Lua.top
                Lua.pop 1
                pure $ case mv of
                    Just (Lua.Number n) → Just (realToFrac n)
                    _ → Nothing
            _ → pure Nothing
    mFill ← getMaybeProp "fill"
    mQuality ← getMaybeProp "quality"
    mCondition ← getMaybeProp "condition"
    mTemp ← getMaybeProp "temp"
    -- Nothing        — the caller named a condition outside [0, 100];
    -- Just Nothing   — no explicit condition, so the base is drawn;
    -- Just (Just b)  — an explicit base, checked.
    let mBase ∷ Maybe (Maybe GroundConditionBase)
        mBase = case mCondition of
            Nothing → Just Nothing
            Just c  → Just <$> mkGroundConditionBase c
    case (nameArg, xArg, yArg, mBase) of
        (Just nameBS, Just x, Just y, Just base) → do
            let name = TE.decodeUtf8Lenient nameBS
            im ← Lua.liftIO $ readReadOnlyRef
                (crvItemManagerRef (toContentRegistriesViewCapability env))
            mWs ← Lua.liftIO $ resolveItemPage env (TE.decodeUtf8Lenient <$> pageArg)
            case (HM.lookup name (imDefs im), mWs) of
                (Just iDef, Just ws) → do
                    let rng = ucStatRNGRef (toUnitCombatCapability env)
                    -- Salvage quality and condition are #1421's rules and
                    -- stay #1421's rules: resolved HERE, in the two draws
                    -- and the order they always had, then handed to the
                    -- materializer as root-scoped overrides (#1418). They
                    -- describe how THIS item came to be lying in the
                    -- world, so they deliberately do not reach the
                    -- default contents it spawns holding.
                    quality ← Lua.liftIO $ rollGroundQuality iDef mQuality rng
                    condition ← Lua.liftIO $ rollGroundCondition base rng
                    logger ← Lua.liftIO $
                        readIORef (ccLoggerRef (toCoreCapability env))
                    mInst ← Lua.liftIO $ materializeItem im logger rng
                        (freshItemInstanceId env)
                        pristineItem { ovFill      = mFill
                                     , ovQuality   = Just quality
                                     , ovCondition = Just condition
                                     , ovTemp      = mTemp }
                        name
                    case mInst of
                        Nothing → Lua.pushnil >> return 1
                        Just inst → do
                            gid ← Lua.liftIO $
                                atomicModifyIORef' (wsGroundItemsRef ws) $
                                    spawnGroundItem inst (realToFrac x)
                                                         (realToFrac y)
                            Lua.pushinteger (fromIntegral gid)
                            return 1
                _ → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | Push ONE @{id, instanceId, defName, kind, x, y, fill, quality,
--   qualityTier, condition, sharpness, weight}@ ground-item row,
--   leaving it on top of the stack.
--
--   The single place that row shape is built (#1666). The whole-page
--   listing and the owning-page single-entry lookup below both go
--   through it, so the two cannot come to describe the same item
--   differently — which is exactly the divergence a caller that
--   switches from one to the other would be unable to see.
--
--   @instanceId@, @sharpness@ and @kind@ (#1737) are what let the
--   autonomous repair AI treat a ground item as a repair TARGET rather
--   than only as a consumable to haul: claims, the player-priority flag
--   and the post-load reference graph are all keyed by the instance id,
--   and 'scripts.unit_ai_repair_target' scores both wear axes plus the
--   broken-armour band from @condition@ \/ @sharpness@ \/ @kind@. All
--   three are spelled exactly as @unit.getInventory@ spells them, so
--   one severity function reads a held row and a ground row alike:
--
--   * @instanceId@ — the instance's own process-unique id (#67);
--   * @sharpness@ — UNCONDITIONAL, like @condition@, because
--     'iiSharpness' is universal runtime wear state that every item
--     instance carries. It deliberately does NOT follow @qualityTier@'s
--     present-only-when-declared convention, which exists because
--     workmanship is a def-level opt-in;
--   * @kind@ — the DEF's equipment-slot kind, defaulted to @"misc"@
--     when the def is unknown, exactly as @unit.getInventory@ defaults
--     it, so a row is never missing the field a predicate branches on.
pushGroundRow ∷ ItemManager → Int → GroundItem
              → Lua.LuaE Lua.Exception ()
pushGroundRow im gid gi = do
    let inst = giInst gi
        mDef = lookupItemDef (iiDefName inst) im
    Lua.newtable
    Lua.pushinteger (fromIntegral gid)
    Lua.setfield (Lua.nth 2) "id"
    Lua.pushinteger (fromIntegral (iiInstanceId inst))
    Lua.setfield (Lua.nth 2) "instanceId"
    Lua.pushstring (TE.encodeUtf8 (iiDefName (giInst gi)))
    Lua.setfield (Lua.nth 2) "defName"
    Lua.pushnumber (Lua.Number (realToFrac (giX gi)))
    Lua.setfield (Lua.nth 2) "x"
    Lua.pushnumber (Lua.Number (realToFrac (giY gi)))
    Lua.setfield (Lua.nth 2) "y"
    Lua.pushnumber (Lua.Number (realToFrac
        (iiCurrentFill (giInst gi))))
    Lua.setfield (Lua.nth 2) "fill"
    Lua.pushnumber (Lua.Number (realToFrac
        (iiQuality (giInst gi))))
    Lua.setfield (Lua.nth 2) "quality"
    -- Tier label only when the def actually declares a
    -- quality spec (mirrors unit.getInventory / the
    -- equipment queries — #345).
    case mDef of
        Just d | Just _ ← idQualitySpec d →
            case qualityTierLabel d
                     (iiQuality (giInst gi)) of
                Just tier → do
                    Lua.pushstring (TE.encodeUtf8 tier)
                    Lua.setfield (Lua.nth 2) "qualityTier"
                Nothing → pure ()
        _ → pure ()
    Lua.pushnumber (Lua.Number (realToFrac
        (iiCondition (giInst gi))))
    Lua.setfield (Lua.nth 2) "condition"
    Lua.pushnumber (Lua.Number (realToFrac
        (iiSharpness inst)))
    Lua.setfield (Lua.nth 2) "sharpness"
    Lua.pushstring (TE.encodeUtf8 (maybe "misc" idKind mDef))
    Lua.setfield (Lua.nth 2) "kind"
    -- True live mass: empty weight + fill (at the
    -- container's per-unit fill weight) + everything
    -- nested in iiContents, computed recursively. A
    -- stocked first-aid kit weighs its contents, not
    -- just its empty case.
    Lua.pushnumber (Lua.Number (realToFrac
        (itemTotalWeight im (giInst gi))))
    Lua.setfield (Lua.nth 2) "weight"

-- | item.listGround() → array of {id, instanceId, defName, kind, x, y,
--   fill, quality, qualityTier, condition, sharpness, weight}.
--   `weight` is the live total mass (itemTotalWeight: empty weight +
--   fill + nested contents), not the static def weight. `qualityTier`
--   (#345) is present only when the def declares a quality spec;
--   `condition`, `sharpness` (#1737) and `kind` are always present.
--
--   ACTIVE-page scoped, deliberately: this is the UI's listing, and
--   the UI only ever shows the world the player is looking at. A
--   caller that already knows WHICH unit an id belongs to must use
--   'itemGetGroundForUnitFn' instead — an id from here paired with an
--   off-page unit names a different item entirely (#1666).
itemListGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemListGroundFn env = do
    mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
    im  ← Lua.liftIO $ readReadOnlyRef
        (crvItemManagerRef (toContentRegistriesViewCapability env))
    case mWs of
        Nothing → Lua.pushnil >> return 1
        Just ws → do
            gis ← Lua.liftIO $ readIORef (wsGroundItemsRef ws)
            Lua.newtable
            forM_ (zip [1 ∷ Int ..] (HM.toList (gisItems gis))) $
                \(i, (gid, gi)) → do
                    pushGroundRow im gid gi
                    Lua.rawseti (Lua.nth 2) (fromIntegral i)
            return 1

-- | item.getGroundForUnit(uid, gid) → entry|nil, pageResolved
--
--   The owning-page counterpart to 'itemListGroundFn' (#1666): ONE
--   ground row, looked up on the page unit @uid@ is standing on, in
--   the identical shape @item.listGround@ produces — same builder, so
--   the instance id, sharpness and kind #1737's repair scan decides on
--   are by construction the same values the listing showed. Resolved through
--   the same 'unitOwningWorldState' 'itemPickupGroundFn' commits
--   through, so a caller that inspects an entry here and then picks it
--   up is guaranteed to have described the instance it moved — the
--   halves of that contract cannot disagree the way an active-page
--   read paired with an owning-page write did.
--
--   Deliberately NOT a session-wide search and deliberately NOT a
--   whole-page listing: it answers about ONE named unit and ONE id,
--   because a session-wide ground listing would recreate the very
--   same-numbered-gid-on-another-page hazard somewhere else.
--
--   The SECOND return value is the load-bearing one. Lua cannot tell
--   "this page says the item is gone" from "this unit has no live
--   page to ask" out of a lone @nil@, and those must not be treated
--   alike: the first retires an order, the second is not an answer at
--   all and must never fall back to the active page. So:
--
--   * entry, @true@  — present on the carrier's own page;
--   * @nil@, @true@  — that page is live and genuinely has no such id;
--   * @nil@, @false@ — no live page for this unit (or no such unit, or
--     a malformed argument), so nothing was determined.
--
--   Both arguments must be Lua numbers. 'Lua.tointeger' happily
--   coerces the numeric STRING @"3"@, and a query whose answer decides
--   whether an item is retired should not silently accept a
--   type-confused id.
itemGetGroundForUnitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetGroundForUnitFn env = do
    uidArg ← numberArgAt 1
    gidArg ← numberArgAt 2
    case (uidArg, gidArg) of
        (Just u, Just g) → do
            mWs ← Lua.liftIO $
                unitOwningWorldState env (UnitId (fromIntegral u))
            case mWs of
                Nothing → unresolved
                Just ws → do
                    gis ← Lua.liftIO $ readIORef (wsGroundItemsRef ws)
                    im  ← Lua.liftIO $ readReadOnlyRef
                        (crvItemManagerRef (toContentRegistriesViewCapability env))
                    case HM.lookup (fromIntegral g) (gisItems gis) of
                        Nothing → Lua.pushnil
                        Just gi → pushGroundRow im (fromIntegral g) gi
                    Lua.pushboolean True
                    return 2
        _ → unresolved
  where
    unresolved = do
        Lua.pushnil
        Lua.pushboolean False
        return 2

-- | An integer argument that must actually BE a Lua number. Guards the
--   'Lua.tointeger' numeric-string coercion described above; the
--   stack index is absolute because nothing is pushed before it.
numberArgAt ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Lua.Integer)
numberArgAt idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeNumber then pure Nothing else Lua.tointeger idx

-- | item.removeGround(gid) → true | false
itemRemoveGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemRemoveGroundFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    mGi ← Lua.liftIO $
                        atomicModifyIORef' (wsGroundItemsRef ws) $
                            removeGroundItem (fromIntegral n)
                    Lua.pushboolean (isJust mGi)
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | item.groundCount() → n (headless tests / HUD readouts)
itemGroundCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGroundCountFn env = do
    mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
    case mWs of
        Nothing → Lua.pushinteger 0 >> return 1
        Just ws → do
            gis ← Lua.liftIO $ readIORef (wsGroundItemsRef ws)
            Lua.pushinteger (fromIntegral (HM.size (gisItems gis)))
            return 1

-- | item.getGroundTemp(gid) → °C | nil. The ground item's effective
--   temperature (#344): its tracked iiTemp when it's hotter/colder
--   than its surroundings, else the ambient air at its own tile
--   (elevation-corrected — World.Weather.Ambient). nil if the id
--   doesn't exist (or the item is untracked and the page has no gen
--   params to read an ambient from).
itemGetGroundTempFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetGroundTempFn env = do
    idArg ← Lua.tointeger 1
    mT ← case idArg of
        Nothing → pure Nothing
        Just n → Lua.liftIO $ do
            mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
            case mWs of
                Nothing → pure Nothing
                Just ws → do
                    gis ← readIORef (wsGroundItemsRef ws)
                    case HM.lookup (fromIntegral n) (gisItems gis) of
                        Nothing → pure Nothing
                        Just gi → do
                            mp ← readIORef (wsGenParamsRef ws)
                            let mAmb = fmap (\p → ambientTempAt
                                    (wgpSeed p) (wgpPlates p)
                                    (wgpClimateState p) (wgpWorldSize p)
                                    (floor (giX gi)) (floor (giY gi))) mp
                            pure $ case mAmb of
                                Just amb → Just (effectiveItemTemp amb
                                                     (giInst gi))
                                Nothing  → iiTemp (giInst gi)
    case mT of
        Just t  → do
            Lua.pushnumber (Lua.Number (realToFrac t))
            return 1
        Nothing → Lua.pushnil >> return 1

-- | item.setGroundTemp(gid [, temp]) → bool. Sets a ground item's
--   tracked temperature (°C) — the "this item was made hot/cold" hook
--   (#344); the per-page tick then relaxes it toward the tile's
--   ambient. Omitting temp (or passing nil) clears the tracked value —
--   the item reads as "at ambient" again. False if the id doesn't
--   exist.
itemSetGroundTempFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSetGroundTempFn env = do
    idArg ← Lua.tointeger 1
    tArg  ← Lua.tonumber 2
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just n → do
            let mT = case tArg of
                    Just (Lua.Number d) → Just (realToFrac d ∷ Float)
                    _                   → Nothing
            mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    ok ← Lua.liftIO $
                        atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
                            case HM.lookup (fromIntegral n) (gisItems gis) of
                                Nothing → (gis, False)
                                Just gi →
                                    let gi' = gi { giInst = (giInst gi)
                                                     { iiTemp = mT } }
                                    in ( gis { gisItems = HM.insert
                                                 (fromIntegral n) gi'
                                                 (gisItems gis) }
                                       , True )
                    Lua.pushboolean ok
                    return 1

-- | item.pickupGround(uid, gid) → true | false — atomically move a
--   ground item into a unit's inventory, PRESERVING the instance
--   (fill / quality / condition), unlike unit.addItem which builds a
--   fresh instance. Remove-first ordering means two racing pickups
--   can't duplicate the item: the loser's remove returns Nothing.
--   If the unit vanished between remove and insert, the item is
--   re-spawned at its old position (new id) on that same page.
--
--   The gid is resolved on the UNIT'S OWN page (#1208), never the
--   active one: ground items are page-local, so a same-numbered gid
--   on another page is a different item entirely and must not be
--   removed. A missing unit, a unit whose page has no live world, and
--   a gid absent from that page all return false with the ground
--   collections, the unit, and the cursor untouched.
itemPickupGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemPickupGroundFn env = do
    uidArg ← Lua.tointeger 1
    gidArg ← Lua.tointeger 2
    case (uidArg, gidArg) of
        (Just u, Just g) → do
            let uid = UnitId (fromIntegral u)
            mWs ← Lua.liftIO $ unitOwningWorldState env uid
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    ok ← Lua.liftIO $
                        pickupGroundOnPage env ws uid (fromIntegral g)
                    Lua.pushboolean ok
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | The remove → insert → rollback core of 'itemPickupGroundFn',
--   scoped to ONE already-resolved page. Split out so the concurrent-
--   disappearance rollback — which needs the unit to be gone at insert
--   time — is reachable deterministically from the #1208 regression
--   instead of only through a race.
--
--   Every ground and cursor write here targets @ws@ and nothing else,
--   which is what makes "the item is restored to the page it was taken
--   from" true by construction rather than by inspection.
pickupGroundOnPage ∷ EngineEnv → WorldState → UnitId → Int → IO Bool
pickupGroundOnPage env ws uid gid = do
    mGi ← atomicModifyIORef' (wsGroundItemsRef ws) $ removeGroundItem gid
    case mGi of
        -- Nothing was removed, so there is nothing to deselect either:
        -- a pre-removal failure leaves ground, unit and cursor exactly
        -- as they were.
        Nothing → pure False
        Just gi → do
            inserted ← atomicModifyIORef'
                (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst
                                  { uiInventory =
                                      uiInventory inst ++ [giInst gi] }
                            in (um { umInstances =
                                       HM.insert uid inst' (umInstances um) }
                               , True)
            unless inserted $ do
                -- Unit gone: put it back, on the page we took it from.
                _ ← atomicModifyIORef' (wsGroundItemsRef ws) $
                        spawnGroundItem (giInst gi) (giX gi) (giY gi)
                pure ()
            -- Deselect if the picked item was selected. A rollback
            -- clears it too: spawnGroundItem hands the restored
            -- instance a NEW id, so this gid is stale either way.
            atomicModifyIORef' (wsCursorRef ws) $ \cs →
                ( if selectedGroundItem cs ≡ Just gid
                  then cs { selectedGroundItem = Nothing }
                  else cs
                , () )
            pure inserted
