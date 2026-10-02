{-# LANGUAGE Strict #-}
-- | The scenario v1 vocabulary (#2699): every family, field and rule of
--   @docs/scenario_format.md@'s field table, read with the tolerant
--   per-field machinery of "Scenario.Decode.Monad".
--
--   'decodeContent' is run twice over the same parsed document. The
--   first pass records local diagnostics plus one 'Node' per entry
--   (identity, owner, references); "Scenario.Validate" analyses those
--   nodes for duplicate ids, references and cascades; the second pass
--   rebuilds the typed content with that analysis applied. Every local
--   rule is a pure function of the document and the catalog, so both
--   passes reach the same local verdicts and only the first reports.
module Scenario.Decode
    ( decodeContent
    , woundKinds
    , dressingKinds
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (nub)
import qualified Data.Scientific as Sci
import World.Fluid.Exact (fluidUnitsPerZ)
import Gameplay.Tags.Types (GameplayTag)
import Unit.Direction (parseDirectionName)
import Unit.Injury (bruiseCap, maxInjurySeverity)
import Combat.Wounds.Infection (woundHealFloor)
import Structure.Types (StructureSlot(..), slotFromText)
import Scenario.Types
import Scenario.Bounds
import Scenario.Decode.Monad

-- * Top level

topKeys ∷ [Text]
topKeys =
    [ "version", "map", "camera", "terrain", "fluids", "flora"
    , "structures", "buildings", "locations", "units", "ground_items" ]

-- | Decode a migrated, current-version document body.
decodeContent ∷ Int → A.Object → Dec Scenario
decodeContent sourceVersion o = do
    checkKeys "" topKeys o
    md ← readMap o
    cam ← readCamera o
    local (\e → e { envBounds = md ≫= mapTileBounds }) $ do
        terrain    ← family "terrain" decodeTerrain
        fluids     ← family "fluids" decodeFluid
        flora      ← family "flora" decodeFlora
        structures ← family "structures" decodeStructure
        buildings  ← family "buildings" decodeBuilding
        locations  ← family "locations" decodeLocation
        units      ← family "units" decodeUnit
        ground     ← family "ground_items" decodeGroundItem
        pure Scenario
            { scSourceVersion = sourceVersion
            , scMap = md, scCamera = cam
            , scTerrain = terrain, scFluids = fluids, scFlora = flora
            , scStructures = structures, scBuildings = buildings
            , scLocations = locations, scUnits = units
            , scGroundItems = ground }
  where
    family ∷ Text → (Text → A.Value → Dec (Maybe β)) → Dec [β]
    family k dec = authoredOr [] <$> listField "" k o dec

readMap ∷ A.Object → Dec (Maybe MapDimensions)
readMap o = do
    r ← optField "" "map" parseMap o
    pure $ case r of
        Authored d → Just d
        Omitted    → Nothing
  where
    parseMap v = case asObject v of
        Just m | KM.size m ≡ 2 → do
            w ← need "width" m
            h ← need "height" m
            Right (MapDimensions w h)
        _ → Left dimsWanted
    need k m = maybe (Left dimsWanted) (either (const (Left dimsWanted)) Right
                                        ∘ pIntIn 1 maxScenarioDimension)
                     (KM.lookup (K.fromText k) m)
    dimsWanted = "exactly width and height, integers in [1, "
              <> tshow maxScenarioDimension <> "]"

readCamera ∷ A.Object → Dec (Float, Float)
readCamera o = case KM.lookup "camera" o of
    Nothing → pure (0, 0)
    Just v → case asObject v of
        Nothing → do
            diag "camera" (InvalidValue "a mapping") FieldRejected
            pure (0, 0)
        Just c → do
            checkKeys "camera" ["x", "y"] c
            x ← optField "camera" "x" pFloat c
            y ← optField "camera" "y" pFloat c
            pure (authoredOr 0 x, authoredOr 0 y)

-- * Entries

-- | The shared entry skeleton. @header@ checks the entry's own required
--   fields (and its bounds) and, when they hold, yields the stage that
--   reads its optional fields and children; a failed header rejects the
--   entry, and everything it owns is excluded without being read.
entry
    ∷ NodeKind → Maybe Text → [Text] → [Text]
    → (Text → A.Object → ScenarioId → [GameplayTag]
            → Dec (Maybe (Dec (α, [(Text, Text, NodeKind)], [(Text, Text, NodeKind)]))))
    → Text → A.Value → Dec (Maybe α)
entry kind owner keys ownedKeys header p v = do
    skip ← isRejectedPath p
    if skip then pure Nothing else case asObject v of
        Nothing → do
            diag p (InvalidValue "a mapping") EntryRejected
            pure Nothing
        Just o → do
            checkKeys p ("id" : "tags" : keys) o
            (sid, mid) ← readId p o
            tags ← readTags p o
            h ← header p o sid tags
            case h of
                Nothing → do
                    emitNode (Node p kind mid owner False [] [])
                    excludeOwned p ownedKeys o
                    pure Nothing
                Just body → do
                    (x, req, opt) ← body
                    emitNode (Node p kind mid owner True req opt)
                    pure (Just x)

-- | Report (and record as dead nodes) every item an excluded owner held:
--   a rejected container's contents never survive (D-17).
excludeOwned ∷ Text → [Text] → A.Object → Dec ()
excludeOwned owner keys o =
    forM_ (ownedItemValues owner keys o) $ \(ip, iv) → excludeItemTree owner ip iv

excludeItemTree ∷ Text → Text → A.Value → Dec ()
excludeItemTree owner ip iv = do
    diag ip (OwnerRejected owner) CascadeRejected
    emitNode (Node ip NodeItem (rawExplicitId iv) (Just owner) False [] [])
    forM_ (maybe [] (ownedItemValues ip ["contents"]) (asObject iv)) $
        \(cp, cv) → excludeItemTree owner cp cv

-- | The item values held under an entry's ownership keys, with paths.
ownedItemValues ∷ Text → [Text] → A.Object → [(Text, A.Value)]
ownedItemValues p keys o = concatMap one keys
  where
    one k = case KM.lookup (K.fromText k) o of
        Just (A.Array xs) →
            [ (indexPath (keyPath p k) i, x) | (i, x) ← zip [0 ..] (toList xs) ]
        Just (A.Object m) →
            [ (keyPath (keyPath p k) (K.toText s), x) | (s, x) ← KM.toList m ]
        _ → []

-- | Look up a required definition: missing, malformed or unknown
--   rejects the entry ('UnknownDefinition' for the last).
definition ∷ Text → Text → A.Object → (Text → Maybe β) → Dec (Maybe (Text, β))
definition p k o look = do
    r ← reqField p k pNonEmptyText o
    case r of
        Nothing → pure Nothing
        Just name → case look name of
            Just b  → pure (Just (name, b))
            Nothing → do
                diag (keyPath p k) (UnknownDefinition name) EntryRejected
                pure Nothing

catalog ∷ Dec ScenarioCatalog
catalog = envCatalog <$> askEnv

-- | Reject an entry whose tile lies outside a finite map.
tileInside ∷ Text → (Integer, Integer) → Dec Bool
tileInside p t = do
    b ← envBounds <$> askEnv
    case b of
        Just bs | not (inBoundsI bs t) → do
            diag p OutsideBounds EntryRejected
            pure False
        _ → pure True

footprintOk ∷ Text → (Int, Int) → Footprint → Dec Bool
footprintOk p a fp = do
    b ← envBounds <$> askEnv
    case b of
        Just bs | not (footprintInside bs a fp) → do
            diag p FootprintOutsideBounds EntryRejected
            pure False
        _ → pure True

noRefs ∷ α → (α, [(Text, Text, NodeKind)], [(Text, Text, NodeKind)])
noRefs x = (x, [], [])

-- ** Terrain and fluids

-- | A patch region: exactly one of @rect@ or @tiles@, clipped to the map
--   with one 'TilesClipped' diagnostic (D-21). Nothing left rejects the
--   patch.
readRegion ∷ Text → A.Object → Dec (Maybe TileRegion)
readRegion p o = case (KM.lookup "rect" o, KM.lookup "tiles" o) of
    (Just _, Just _) → do
        diag p (InvalidValue "exactly one of rect or tiles") EntryRejected
        pure Nothing
    (Nothing, Nothing) → do
        diag (keyPath p "rect") MissingRequired EntryRejected
        pure Nothing
    (Just _, Nothing) → do
        r ← reqField p "rect" parseRect o
        maybe (pure Nothing) clip r
    (Nothing, Just _) → do
        ts ← listField p "tiles" o $ \ip v → case parsePair v of
            Right t → pure (Just t)
            Left why → do
                diag ip (InvalidValue why) FieldRejected
                pure Nothing
        case nub (authoredOr [] ts) of
            [] → do
                diag (keyPath p "tiles") (InvalidValue "a non-empty tile list") EntryRejected
                pure Nothing
            xs → clip (RegionTiles xs)
  where
    parseRect v = case asObject v of
        Just m | KM.size m ≡ 4 → do
            let g k = maybe (Left rectWanted) (either (const (Left rectWanted)) Right ∘ pInt)
                            (KM.lookup (K.fromText k) m)
            x0 ← g "x0"; y0 ← g "y0"; x1 ← g "x1"; y1 ← g "y1"
            Right (RegionRect (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1))
        _ → Left rectWanted
    rectWanted = "a mapping of integers x0, y0, x1, y1"
    parsePair (A.Array xs) | [a, b] ← toList xs = (,) <$> pInt a <*> pInt b
    parsePair _ = Left "a pair [x, y] of integers"
    clip region = do
        b ← envBounds <$> askEnv
        case b of
            Nothing → pure (Just region)
            Just bs → do
                case clipRegion bs region of
                    (Nothing, _) → do
                        diag p OutsideBounds EntryRejected
                        pure Nothing
                    (Just k, dropped) → do
                        when (dropped > 0) $ diag p (ClippedTiles dropped) TilesClipped
                        pure (Just k)

decodeTerrain ∷ Text → A.Value → Dec (Maybe TerrainPatch)
decodeTerrain = entry NodeTerrain Nothing
    ["rect", "tiles", "material", "surface_z", "slope"] [] $ \p o sid tags → do
        cat ← catalog
        mat ← definition p "material" o
                (\n → if HS.member n (catMaterials cat) then Just () else Nothing)
        z ← reqField p "surface_z" pInt o
        region ← if isJust mat ∧ isJust z then readRegion p o else pure Nothing
        pure $ do
            (m, ()) ← mat
            sz ← z
            r ← region
            Just $ do
                slope ← optField p "slope" (fmap fromIntegral ∘ pIntIn 0 15) o
                pure (noRefs (TerrainPatch sid tags r m sz slope))

decodeFluid ∷ Text → A.Value → Dec (Maybe FluidPatch)
decodeFluid = entry NodeFluid Nothing ["rect", "tiles", "fluid", "surface_z"] [] $
    \p o sid tags → do
        kind ← reqField p "fluid" (pOneOf [ (fluidKindName k, k) | k ← [minBound .. maxBound] ]) o
        surf ← reqField p "surface_z" pEighths o
        region ← if isJust kind ∧ isJust surf then readRegion p o else pure Nothing
        pure $ do
            k ← kind
            s ← surf
            r ← region
            Just (pure (noRefs (FluidPatch sid tags r k s)))
  where
    -- The exact fluid plane ('World.Fluid.Exact'): 4.5 → 36 eighths.
    -- Read from the document's own decimal, never through a Float, so
    -- nothing is rounded onto or off the plane.
    pEighths (A.Number s) =
        maybe (Left planeWanted) Right
              (Sci.toBoundedInteger (s * fromIntegral fluidUnitsPerZ))
    pEighths _ = Left planeWanted
    planeWanted = "a number on the exact fluid plane (a multiple of 1/"
               <> tshow fluidUnitsPerZ <> ")"

-- ** Flora and structures

decodeFlora ∷ Text → A.Value → Dec (Maybe FloraEntry)
decodeFlora = entry NodeFlora Nothing ["species", "x", "y", "z", "age", "health"] [] $
    \p o sid tags → do
        cat ← catalog
        sp ← definition p "species" o
                (\n → if HS.member n (catFlora cat) then Just () else Nothing)
        x ← reqField p "x" pInt o
        y ← reqField p "y" pInt o
        inside ← case (sp, x, y) of
            (Just _, Just xv, Just yv) → tileInside p (toInteger xv, toInteger yv)
            _ → pure False
        pure $ do
            (s, ()) ← sp
            xv ← x
            yv ← y
            guard inside
            Just $ do
                z ← optField p "z" pInt o
                age ← optField p "age" (pFloatMin 0) o
                health ← optField p "health" (pFloatIn 0 1) o
                pure (noRefs (FloraEntry sid tags s xv yv z age health))

-- | The piece kind a structure slot draws from its pack.
slotKind ∷ StructureSlot → Text
slotKind s = case s of
    SFloor   → "floor"
    SCeiling → "ceiling"
    SWire    → "wire"
    SWallNE  → "wall"
    SWallNW  → "wall"
    SWallSE  → "wall"
    SWallSW  → "wall"
    SPostN   → "post"
    SPostE   → "post"
    SPostS   → "post"
    SPostW   → "post"

decodeStructure ∷ Text → A.Value → Dec (Maybe StructurePiece)
decodeStructure = entry NodeStructure Nothing ["pack", "piece", "x", "y", "z"] [] $
    \p o sid tags → do
        cat ← catalog
        pack ← definition p "pack" o (\n → HM.lookup n (catStructurePacks cat))
        piece ← reqField p "piece" pPiece o
        ok ← case (pack, piece) of
            (Just (pn, kinds), Just (pt, slot))
                | not (HS.member (slotKind slot) kinds) → do
                    diag (keyPath p "piece")
                         (UnknownDefinition (pn <> ":" <> pt)) EntryRejected
                    pure False
            _ → pure True
        x ← reqField p "x" pInt o
        y ← reqField p "y" pInt o
        inside ← case (x, y) of
            (Just xv, Just yv) | ok, isJust pack, isJust piece →
                tileInside p (toInteger xv, toInteger yv)
            _ → pure False
        pure $ do
            (pn, _) ← pack
            (pt, _) ← piece
            xv ← x
            yv ← y
            guard (ok ∧ inside)
            Just $ do
                z ← optField p "z" pInt o
                pure (noRefs (StructurePiece sid tags pn pt xv yv z))
  where
    pPiece v = do
        t ← pText v
        case slotFromText t of
            Just s | T.toLower t ≡ t → Right (t, s)
            _ → Left "a structure slot: floor, ceiling, wall_ne, wall_nw, \
                     \wall_se, wall_sw, post_n, post_e, post_s, post_w or wire"

-- ** Buildings and locations

decodeBuilding ∷ Text → A.Value → Dec (Maybe BuildingEntry)
decodeBuilding = entry NodeBuilding Nothing
    [ "definition", "x", "y", "z", "storage", "build_progress"
    , "materials_delivered", "power_charge" ]
    ["storage", "materials_delivered"] $ \p o sid tags → do
        cat ← catalog
        def ← definition p "definition" o (\n → HM.lookup n (catBuildings cat))
        x ← reqField p "x" pInt o
        y ← reqField p "y" pInt o
        inside ← case (def, x, y) of
            (Just (_, bc), Just xv, Just yv) → footprintOk p (xv, yv) (bcFootprint bc)
            _ → pure False
        pure $ do
            (dn, bc) ← def
            xv ← x
            yv ← y
            guard inside
            Just $ do
                z ← optField p "z" pInt o
                storage ← listField p "storage" o (decodeItem p)
                progress ← optField p "build_progress" (pFloatIn 0 (bcBuildWork bc)) o
                delivered ← listField p "materials_delivered" o $ \ip v →
                    decodeMaterial p (bcMaterials bc) ip v
                charge ← case bcPowerCapacity bc of
                    Just cap → optField p "power_charge" (pFloatIn 0 cap) o
                    Nothing → case KM.lookup "power_charge" o of
                        Nothing → pure Omitted
                        Just _ → do
                            diag (keyPath p "power_charge")
                                 (InvalidValue "no charge: this building stores no power")
                                 FieldRejected
                            pure Omitted
                pure (noRefs (BuildingEntry sid tags dn xv yv z storage progress delivered charge))

-- | A delivered material: an ordinary item whose definition must be one
--   of the building's materials.
decodeMaterial ∷ Text → HS.HashSet Text → Text → A.Value → Dec (Maybe ItemEntry)
decodeMaterial owner mats ip v = do
    let defName = asObject v ≫= \io → case KM.lookup "definition" io of
            Just (A.String t) → Just t
            _ → Nothing
    case defName of
        Just d | not (HS.member d mats) → do
            skip ← isRejectedPath ip
            unless skip $ do
                diag (keyPath ip "definition")
                     (InvalidValue "a material this building consumes") EntryRejected
                emitNode (Node ip NodeItem (rawExplicitId v) (Just owner) False [] [])
                forM_ (maybe [] (ownedItemValues ip ["contents"]) (asObject v)) $
                    \(cp, cv) → excludeItemTree ip cp cv
            pure Nothing
        _ → decodeItem owner ip v

decodeLocation ∷ Text → A.Value → Dec (Maybe LocationEntry)
decodeLocation = entry NodeLocation Nothing ["definition", "x", "y", "significant_items"] [] $
    \p o sid tags → do
        cat ← catalog
        def ← definition p "definition" o (\n → HM.lookup n (catLocations cat))
        x ← reqField p "x" pInt o
        y ← reqField p "y" pInt o
        inside ← case (def, x, y) of
            (Just (_, lc), Just xv, Just yv) → footprintOk p (xv, yv) (lcFootprint lc)
            _ → pure False
        pure $ do
            (dn, lc) ← def
            xv ← x
            yv ← y
            guard inside
            Just $ do
                env ← askEnv
                bindings ← mapField p "significant_items" o $ \fp key v →
                    case (parseSlot (lcSignificantSlots lc) key, pRef v) of
                        (Left why, _) → do
                            diag fp (InvalidValue why) FieldRejected
                            pure Nothing
                        (_, Left why) → do
                            diag fp (InvalidValue why) FieldRejected
                            pure Nothing
                        (Right slot, Right target)
                            | HS.member fp (envDropped env) → pure Nothing
                            | otherwise → pure (Just (slot, target, fp))
                let bs = authoredOr [] bindings
                pure ( LocationEntry sid tags dn xv yv
                         (HM.fromList [ (slot, ExplicitId t) | (_, (slot, t, _)) ← bs ])
                     , []
                     , [ (fp, t, NodeItem) | (_, (_, t, fp)) ← bs ] )
  where
    -- canonical decimal only ("1", never "01" or "+1"), so two keys can
    -- never name the same slot
    parseSlot n key = case reads (T.unpack key) of
        [(i, "")] | tshow i ≡ key, i ≥ 1 ∧ i ≤ n → Right i
        _ → Left ("a significant-item slot in [1, " <> tshow n <> "], written plainly")

-- | A reference value: the explicit id of another entry.
pRef ∷ Parser Text
pRef v = do
    t ← pText v
    if explicitIdOk t then Right t else Left "an explicit entry id"

-- ** Units

woundKinds ∷ [Text]
woundKinds =
    [ "slash", "stab", "blunt", "fracture", "concussion", "internal"
    , "severed", "arterial", "frostbite" ]

dressingKinds ∷ [Text]
dressingKinds = ["", "bandage", "tourniquet"]

unitKeys ∷ [Text]
unitKeys =
    [ "definition", "x", "y", "z", "name", "facing", "encounter"
    , "stats", "skills", "knowledge", "modifiers", "wounds", "scars"
    , "blood", "immune_response", "immunities"
    , "inventory", "equipment", "accessories" ]

decodeUnit ∷ Text → A.Value → Dec (Maybe UnitEntry)
decodeUnit = entry NodeUnit Nothing unitKeys ["inventory", "equipment", "accessories"] $
    \p o sid tags → do
        cat ← catalog
        def ← definition p "definition" o (\n → HM.lookup n (catUnits cat))
        x ← reqField p "x" pFloat o
        y ← reqField p "y" pFloat o
        enc ← case KM.lookup "encounter" o of
            Nothing → pure (Just Nothing)
            Just _  → fmap Just <$> reqField p "encounter" pRef o
        inside ← case (def, x, y, enc) of
            (Just _, Just xv, Just yv, Just _) → tileInside p (tileOf (xv, yv))
            _ → pure False
        pure $ do
            (dn, uc) ← def
            xv ← x
            yv ← y
            e ← enc
            guard inside
            Just $ do
                z ← optField p "z" pInt o
                name ← optField p "name" pText o
                facing ← optField p "facing" pFacing o
                stats ← overrides p "stats" o (\k → HM.lookup k (ucStats uc))
                skills ← overrides p "skills" o
                            (\k → if HS.member k (ucSkills uc) then Just AuthorableStat else Nothing)
                know ← knowledge p o
                mods ← modifiers p o uc
                wounds ← listField p "wounds" o (decodeWound uc)
                scars ← listField p "scars" o (decodeScar uc)
                blood ← optField p "blood" (pFloatMin 0) o
                immune ← optField p "immune_response" (pFloatIn 0 1) o
                immunities ← immunityMap p o
                inv ← listField p "inventory" o (decodeItem p)
                equip ← mapField p "equipment" o $ \fp slot v →
                    if HS.member slot (ucEquipmentSlots uc)
                        then decodeItem p fp v
                        else do
                            skip ← isRejectedPath fp
                            unless skip $ do
                                diag fp UnknownField FieldRejected
                                emitNode (Node fp NodeItem (rawExplicitId v) (Just p) False [] [])
                                forM_ (maybe [] (ownedItemValues fp ["contents"]) (asObject v)) $
                                    \(cp, cv) → excludeItemTree fp cp cv
                            pure Nothing
                acc ← listField p "accessories" o (decodeItem p)
                let req = [ (keyPath p "encounter", t, NodeLocation) | Just t ← [e] ]
                pure ( UnitEntry
                         { ueId = sid, ueTags = tags, ueDefinition = dn
                         , ueX = xv, ueY = yv, ueZ = z
                         , ueName = name, ueFacing = facing
                         , ueEncounter = ExplicitId <$> e
                         , ueStats = stats, ueSkills = skills
                         , ueKnowledge = know, ueModifiers = mods
                         , ueWounds = wounds, ueScars = scars
                         , ueBlood = blood, ueImmuneResponse = immune
                         , ueImmunities = immunities
                         , ueInventory = inv
                         , ueEquipment = HM.fromList <$> equip
                         , ueAccessories = acc }
                     , req, [] )
  where
    pFacing v = do
        t ← pText v
        maybe (Left "a compass direction (south, north-east, sw, …)") Right
              (parseDirectionName t)

-- | A name-keyed override map (@stats@, @skills@): each entry is checked
--   on its own; an unknown name, a derived stat or a bad value drops
--   only that entry. Explicit names map to their values; every other
--   name stays fallback-eligible.
overrides ∷ Text → Text → A.Object → (Text → Maybe StatRule)
          → Dec (HM.HashMap Text Float)
overrides p k o rule = do
    r ← mapField p k o $ \fp name v → case rule name of
        Nothing → do
            diag fp UnknownField FieldRejected
            pure Nothing
        Just DerivedStat → do
            diag fp DerivedValue FieldRejected
            pure Nothing
        Just AuthorableStat → case pFloat v of
            Right f  → pure (Just f)
            Left why → do
                diag fp (InvalidValue why) FieldRejected
                pure Nothing
    pure (HM.fromList (authoredOr [] r))

knowledge ∷ Text → A.Object → Dec (Authored (HM.HashMap Text Float))
knowledge p o = do
    cat ← catalog
    r ← mapField p "knowledge" o $ \fp name v →
        if not (HS.member name (catKnowledge cat))
            then do
                diag fp UnknownField FieldRejected
                pure Nothing
            else case pFloatMin 0 v of
                Right f  → pure (Just f)
                Left why → do
                    diag fp (InvalidValue why) FieldRejected
                    pure Nothing
    pure (HM.fromList <$> r)

immunityMap ∷ Text → A.Object → Dec (Authored (HM.HashMap Text Float))
immunityMap p o = do
    cat ← catalog
    r ← mapField p "immunities" o $ \fp name v →
        if not (HS.member name (catInfections cat))
            then do
                diag fp (UnknownDefinition name) FieldRejected
                pure Nothing
            else case pFloatIn 0 1 v of
                Right f  → pure (Just f)
                Left why → do
                    diag fp (InvalidValue why) FieldRejected
                    pure Nothing
    pure (HM.fromList <$> r)

-- | Stat/skill modifiers keyed by the stat or skill they modify. A
--   modifier with an invalid @remaining@ is dropped whole rather than
--   silently becoming permanent.
modifiers ∷ Text → A.Object → UnitCatalogEntry
          → Dec (Authored (HM.HashMap Text [ModifierSpec]))
modifiers p o uc = do
    r ← mapField p "modifiers" o $ \fp name v →
        if not (HM.member name (ucStats uc) ∨ HS.member name (ucSkills uc))
            then do
                diag fp UnknownField FieldRejected
                pure Nothing
            else case v of
                A.Array xs → do
                    ys ← forM (zip [0 ..] (toList xs)) $ \(i, mv) →
                        decodeModifier (indexPath fp i) mv
                    pure (Just (catMaybes ys))
                _ → do
                    diag fp (InvalidValue "a list of modifiers") FieldRejected
                    pure Nothing
    pure (HM.fromList <$> r)

decodeModifier ∷ Text → A.Value → Dec (Maybe ModifierSpec)
decodeModifier ip v = case asObject v of
    Nothing → do
        diag ip (InvalidValue "a mapping") EntryRejected
        pure Nothing
    Just o → do
        checkKeys ip ["source", "delta", "percent", "remaining"] o
        src ← reqField ip "source" pNonEmptyText o
        case src of
            Nothing → pure Nothing
            Just s → do
                d ← optField ip "delta" pFloat o
                pc ← optField ip "percent" pFloat o
                -- a timed modifier with nothing left would be expired
                rem' ← case KM.lookup "remaining" o of
                    Nothing → pure (Just Nothing)
                    Just _  → fmap Just <$> reqField ip "remaining" pPositive o
                pure $ ModifierSpec s (authoredOr 0 d) (authoredOr 0 pc) <$> rem'
  where
    pPositive v = do
        f ← pFloat v
        if f > 0 then Right f else Left "a number of seconds > 0"

-- | One wound. A missing or invalid part, kind or severity drops the
--   wound (an 'EntryRejected' list element); an invalid optional field
--   falls back to the fresh-wound default.
decodeWound ∷ UnitCatalogEntry → Text → A.Value → Dec (Maybe WoundSpec)
decodeWound uc ip v = case asObject v of
    Nothing → do
        diag ip (InvalidValue "a mapping") EntryRejected
        pure Nothing
    Just o → do
        cat ← catalog
        checkKeys ip [ "part", "kind", "severity", "age", "bandage", "clot", "heal"
                     , "dressing", "infection", "clean", "infection_type"
                     , "necrosis" ] o
        part ← bodyPart uc ip o
        kind ← reqField ip "kind" (pOneOf [ (k, k) | k ← woundKinds ]) o
        sev ← case kind of
            Just k  → reqField ip "severity" (pFloatIn 0 (severityCap k)) o
            Nothing → pure Nothing
        case (part, kind, sev) of
            (Just pt, Just k, Just s) → do
                age ← optField ip "age" (pFloatMin 0) o
                bandage ← optField ip "bandage" (pFloatIn 0 1) o
                clot ← optField ip "clot" (pFloatIn 0 1) o
                heal ← optField ip "heal" (pFloatIn woundHealFloor 1) o
                dressing ← optField ip "dressing" (pOneOf [ (d, d) | d ← dressingKinds ]) o
                infection ← optField ip "infection" (pFloatIn 0 1) o
                clean ← optField ip "clean" pBool o
                itype ← optField ip "infection_type"
                    (\x → do
                        t ← pText x
                        if T.null t ∨ HS.member t (catInfections cat)
                            then Right t
                            else Left "an infection definition id or \"\"") o
                necrosis ← optField ip "necrosis" (pFloatIn 0 1) o
                pure (Just (WoundSpec pt k s age bandage clot heal dressing
                                      infection clean itype necrosis))
            _ → pure Nothing
  where
    severityCap k = if k ≡ "blunt" then bruiseCap else maxInjurySeverity

decodeScar ∷ UnitCatalogEntry → Text → A.Value → Dec (Maybe ScarSpec)
decodeScar uc ip v = case asObject v of
    Nothing → do
        diag ip (InvalidValue "a mapping") EntryRejected
        pure Nothing
    Just o → do
        checkKeys ip ["part", "kind", "severity", "age"] o
        part ← bodyPart uc ip o
        kind ← reqField ip "kind" (pOneOf [ (k, k) | k ← woundKinds ]) o
        sev ← reqField ip "severity" (pFloatIn 0 maxInjurySeverity) o
        case ScarSpec <$> part <*> kind <*> sev of
            Nothing → pure Nothing
            Just mk → Just ∘ mk <$> optField ip "age" (pFloatMin 0) o

bodyPart ∷ UnitCatalogEntry → Text → A.Object → Dec (Maybe Text)
bodyPart uc ip o = do
    r ← reqField ip "part" pNonEmptyText o
    case r of
        Just pt | not (HS.member pt (ucBodyParts uc)) → do
            diag (keyPath ip "part") (UnknownDefinition pt) EntryRejected
            pure Nothing
        _ → pure r

-- * Items

itemKeys ∷ [Text]
itemKeys =
    [ "definition", "fill", "quality", "condition", "sharpness", "weight"
    , "bulk", "storage_capacity", "temperature", "contents" ]

-- | One owned item (inventory, equipment, accessory, storage, delivered
--   material, or a container's contents), recursively.
decodeItem ∷ Text → Text → A.Value → Dec (Maybe ItemEntry)
decodeItem owner = entry NodeItem (Just owner) itemKeys ["contents"] itemHeader

itemHeader ∷ Text → A.Object → ScenarioId → [GameplayTag]
           → Dec (Maybe (Dec (ItemEntry, [(Text, Text, NodeKind)], [(Text, Text, NodeKind)])))
itemHeader p o sid tags = do
    cat ← catalog
    def ← definition p "definition" o (\n → HM.lookup n (catItems cat))
    pure $ do
        (dn, ic) ← def
        Just (noRefs <$> itemBody p o sid tags dn ic)

itemBody ∷ Text → A.Object → ScenarioId → [GameplayTag] → Text → ItemCatalogEntry
         → Dec ItemEntry
itemBody p o sid tags dn ic = do
    fill ← optField p "fill" pFill o
    quality ← optField p "quality" (pFloatIn 0 100) o
    condition ← optField p "condition" (pFloatIn 0 100) o
    sharpness ← optField p "sharpness" (pFloatIn 0 100) o
    weight ← optField p "weight" (pFloatMin 0) o
    bulk ← optNullable p "bulk" (pFloatMin 0) o
    storage ← optNullable p "storage_capacity" pStorage o
    temp ← optField p "temperature" pTemp o
    contents ← if icHoldsItems ic
        then listField p "contents" o (decodeItem p)
        else case KM.lookup "contents" o of
            Nothing → pure Omitted
            -- an ordinary item's current contents are explicitly empty
            Just (A.Array xs) | null xs → pure (Authored [])
            Just _ → do
                skip ← isRejectedPath (keyPath p "contents")
                unless skip $ do
                    diag (keyPath p "contents")
                         (InvalidValue "no contents: this item holds no items") FieldRejected
                    forM_ (ownedItemValues p ["contents"] o) $
                        \(cp, cv) → excludeItemTree (keyPath p "contents") cp cv
                pure Omitted
    pure (ItemEntry sid tags dn fill quality condition sharpness weight bulk
                    storage temp contents)
  where
    pStorage v = case asObject v of
        Just m | KM.size m ≡ 2
               , Just w ← KM.lookup "weight" m, Right wf ← pFloatMin 0 w
               , Just b ← KM.lookup "bulk" m, Right bf ← pFloatMin 0 b
               → Right (StorageCapacity wf bf)
        _ → Left "null or exactly weight (kg ≥ 0) and bulk (litres ≥ 0)"
    pFill v = case icFluidCapacity ic of
        Just cap → pFloatIn 0 cap v
        Nothing  → do
            f ← pFloat v
            if f ≡ 0 then Right 0 else Left "0: this item holds no fluid"
    -- absolute zero is the one physical floor
    pTemp (A.String "ambient") = Right AtAmbient
    pTemp v = case pFloatMin (-273.15) v of
        Right t → Right (TrackedTemp t)
        Left _  → Left "\"ambient\" or a temperature in °C ≥ -273.15"

decodeGroundItem ∷ Text → A.Value → Dec (Maybe GroundItemEntry)
decodeGroundItem = entry NodeItem Nothing ("x" : "y" : itemKeys) ["contents"] $
    \p o sid tags → do
        cat ← catalog
        def ← definition p "definition" o (\n → HM.lookup n (catItems cat))
        x ← reqField p "x" pFloat o
        y ← reqField p "y" pFloat o
        inside ← case (def, x, y) of
            (Just _, Just xv, Just yv) → tileInside p (tileOf (xv, yv))
            _ → pure False
        pure $ do
            (dn, ic) ← def
            xv ← x
            yv ← y
            guard inside
            Just (noRefs ∘ GroundItemEntry xv yv <$> itemBody p o sid tags dn ic)
