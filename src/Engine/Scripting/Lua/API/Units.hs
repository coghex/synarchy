{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units
    ( loadUnitYamlFn
    , unitSpawnFn
    , unitDestroyFn
    , unitSetPosFn
    , unitMoveToFn
    , unitJumpFn
    , unitStopFn
    , unitGetPosFn
    , unitGetInfoFn
    , unitListFn
    , unitListDefsFn
    , unitListAnimationsFn
    , unitSelectFn
    , unitDeselectAllFn
    , unitGetSelectedFn
    , unitIsSelectedFn
    , unitHitTestAtFn
    , unitHitTestInRectFn
    , unitSetSelectionFn
    , unitSetAnimFn
    , unitSetAnimOverrideFn
    , unitClearAnimOverrideFn
    , unitSetFacingFn
    , unitSetFrozenFn
    , unitSetForceLoopFn
    , unitCollapseFn
    , unitCrawlFn
    , unitReviveFn
    , unitKillFn
    , unitRecomputeBodyFn
    , unitGetStatFn
    , unitGetStatBaseFn
    , unitSetStatFn
    , unitGetAllStatsFn
    , unitAddModifierFn
    , unitRemoveModifierFn
    , unitGetModifiersFn
    , unitClearModifiersFn
    , unitGetAllIdsFn
    , unitGetActivityFn
    , unitGetCurrentAnimFn
    , unitGetJumpReachFn
    , unitLungeImpactSpeedFn
    , unitGetSkillFn
    , unitSetSkillFn
    , unitGetKnowledgeFn
    , unitSetKnowledgeFn
    , unitGetKnowledgeListFn
    , unitAddXPFn
    , unitGetAllSkillsFn
    , unitGetInventoryFn
    , unitGetItemContentsFn
    , unitTreatBleedingFn
    , unitTreatInfectionFn
    , unitFrostbiteFn
    , unitInjureFn
    , unitDrinkFn
    , unitEatFn
    , unitFeedFn
    , unitGetCaloriesFn
    , unitPickupFn
    , unitRemoveItemFn
    , unitTransferItemToBuildingFn
    , unitTransferItemToUnitFn
    , unitDepositToCargoFn
    , unitWithdrawFromCargoFn
    , unitGetCarryingWeightFn
    , unitTransitionToFn
    , unitGetPoseFn
    , unitGetFactionFn
    , unitExistsFn
    , unitGetAttackRangeFn
    , unitGetAttackCooldownFn
    , unitGetAnimDurationFn
    , unitGetMaxSpeedFn
    , unitGetEquippedWeaponWeightFn
    , unitGetWeaponWieldedFromFn
    , unitGetWoundSeverityOnFn
    , unitGetWoundsFn
    , unitGetScarsFn
    , unitGetImmunitiesFn
    , unitGetInsulationFn
    , unitDropEquipmentToGroundFn
    , unitGetBloodFn
    , unitGetPainFn
    , unitGetLastAttackerFn
    , unitGetWeaponClassFn
    , unitModifyItemFillFn
    , unitRepairItemFn
    , unitAddItemFn
    , unitGetVisibleTilesFn
    , unitGetFrameTextureFn
    , unitGetPortraitTextureFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM, forM_, unless, when)
import Data.IORef (readIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified System.Random as Random
import Engine.Core.State (EngineEnv(..), activeWorldState, activeWorldPage, freshItemInstanceId)
import World.Page.Types (WorldPageId(..))
import Infection.Types (InfectionDef(..), lookupInfection)
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlUnits (UnitYamlDef(..), UnitYamlAnim(..),
                               UnitYamlStat(..), UnitYamlSkill(..),
                               UnitYamlBody(..), UnitYamlBodyAttr(..),
                               UnitYamlInventoryEntry(..),
                               UnitYamlModifier(..),
                               UnitYamlBodyPart(..),
                               UnitYamlLayer(..),
                               UnitYamlNaturalWeapon(..),
                               UnitYamlStrike(..),
                               UnitYamlNaturalResistance(..),
                               loadUnitYaml)
import Engine.Asset.YamlNames (loadNamePool)
import System.FilePath (takeDirectory, (</>), (<.>))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import Unit.Thread.Command (recomputeBodyDerivedStats)
import Unit.Thread.Movement (jumpMaxTiles, maxJumpHeight, lungeImpactSpeed)
import Unit.Direction (Direction(..))
import Unit.Render (pickFrame)
import Unit.Sim.Types (Pose(..), UnitActivity(..), UnitSimState(..)
                      , MoveTarget(..), UnitThreadState(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (Camera2D(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..)
                      , BuildingManager(..))
import Data.List (foldl')
import Item.Types (ItemInstance(..), itemMatches, itemContentsSig)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemInstance(..), ItemDef(..), ItemContainer(..)
                  , ItemFood(..), ItemWeapon(..), ItemBuff(..)
                  , ItemManager(..), lookupItemDef, itemTotalWeight)
import Item.Ground (spawnGroundItem)
import Combat.Wounds (bleedRateFor, kindBleedFactor)
import Combat.Types (pushInjuryEvent)
import Unit.LineOfSight (unitVisibleTiles)
import Unit.Stats (rollStat, effectiveStat, applySkillXP)
import qualified Unit.Selection as Sel
import qualified Unit.HitTest as HitTest
import World.Types (WorldManager(..), WorldState(..), WorldTileData(..),
                    LoadedChunk(..), ChunkCoord(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

-- * YAML loading

loadUnitYamlFn ∷ EngineEnv → LuaBackendState
               → Lua.LuaE Lua.Exception Lua.NumResults
loadUnitYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadUnitYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name      = uydName def
                        spritePath = T.unpack (uydSprite def)

                    handle ← loadAndRegister env backendState lteq
                                 ("unit_" <> name) spritePath

                    -- Load the optional authored portrait (info panel).
                    -- Nothing → the UI mirrors the live animation frame.
                    portraitH ← case uydPortrait def of
                        Nothing → return Nothing
                        Just p  → Just <$> loadAndRegister env backendState lteq
                                     ("unit_" <> name <> "_portrait")
                                     (T.unpack p)

                    -- Resolve the name pool (#264). The id maps to a
                    -- file alongside the units dir: data/names/<id>.yaml.
                    mNamePool ← case uydNamePool def of
                        Nothing     → return Nothing
                        Just poolId → do
                            let poolPath = takeDirectory (takeDirectory filePath)
                                           </> "names" </> T.unpack poolId <.> "yaml"
                            Just <$> loadNamePool logger poolPath

                    -- Load directional sprites (if any)
                    dirMap ← foldM (\acc (dirKey, texPath) →
                        case parseDirKey dirKey of
                            Nothing → do
                                logWarn logger CatAsset $
                                    "Unknown direction key '" <> dirKey
                                    <> "' in unit " <> name <> ", skipping"
                                return acc
                            Just dir → do
                                h ← loadAndRegister env backendState lteq
                                        ("unit_" <> name <> "_" <> dirKey)
                                        (T.unpack texPath)
                                return (Map.insert dir h acc)
                        ) Map.empty (Map.toList (uydDirectionalSprites def))

                    -- Load animations (if any)
                    animMap ← foldM (\accA (animName, animDef) → do
                        frameMap ← foldM (\accF (dirKey, framePaths) →
                            case parseDirKey dirKey of
                                Nothing → do
                                    logWarn logger CatAsset $
                                        "Unknown direction '" <> dirKey
                                        <> "' in anim '" <> animName
                                        <> "' of unit " <> name <> ", skipping"
                                    return accF
                                Just dir → do
                                    handles ← mapM (\(i, p) →
                                        loadAndRegister env backendState lteq
                                            ("unit_" <> name
                                             <> "_" <> animName
                                             <> "_" <> dirKey
                                             <> "_" <> T.pack (show i))
                                            (T.unpack p)
                                        ) (zip [(0 ∷ Int)..] framePaths)
                                    return (Map.insert dir
                                              (V.fromList handles) accF)
                            ) Map.empty (Map.toList (uyaFrames animDef))
                        let anim = Animation
                                { aFps    = uyaFps animDef
                                , aLoop   = uyaLoop animDef
                                , aFlip   = uyaFlip animDef
                                , aFrames = frameMap
                                }
                        return (HM.insert animName anim accA)
                        ) HM.empty (Map.toList (uydAnimations def))

                    let stateAnims = HM.fromList (Map.toList (uydStateAnimations def))
                        body = uydBody def
                        -- height goes into the live-stat layer (it's
                        -- immutable post-spawn but readable forever).
                        -- bulk + bodyfat are spawn-only inputs — they
                        -- live in `udBodyTemplates` so the lazy-roll
                        -- in `getOrRollStat` can't surface them later
                        -- as fresh rolls divorced from the unit's
                        -- actual body composition.
                        statTemplates = HM.fromList $
                            ("height", (uybaMean (uybHeight body),
                                        uybaRange (uybHeight body))) :
                            [ (sname, (uysBase s, uysRange s))
                            | (sname, s) ← Map.toList (uydStats def)
                            ]
                        bodyTemplates = HM.fromList
                            [ ("bulk",    (uybaMean (uybBulk body),
                                           uybaRange (uybBulk body)))
                            , ("bodyfat", (uybaMean (uybBodyfat body),
                                           uybaRange (uybBodyfat body)))
                            ]
                        skillTemplates = HM.fromList
                            [ (sname, (uyskBase s, uyskRange s))
                            | (sname, s) ← Map.toList (uydSkills def)
                            ]
                        knowledgeTemplates = HM.fromList
                            [ (kname, (uyskBase s, uyskRange s))
                            | (kname, s) ← Map.toList (uydKnowledge def)
                            ]

                    -- Expand each entry by its count. Each repetition
                    -- becomes a distinct ItemInstance (quality /
                    -- condition rolls fire per copy); the drop
                    -- priority rides along for the spawn-time
                    -- capacity check.
                    let startingInv =
                            [ (uyieItem e, uyieFill e, uyieDropPriority e)
                            | e ← uydStartingInventory def
                            , _ ← [1 .. max 1 (uyieCount e)]
                            ]
                        bodyParts =
                            [ BodyPart
                                { bpId              = uybpId p
                                , bpName            = maybe (uybpId p) id (uybpName p)
                                , bpParent          = uybpParent p
                                , bpVital           = uybpVital p
                                , bpAreaWeight      = uybpAreaWeight p
                                , bpTacticalValue   = uybpTacticalValue p
                                , bpBleedFactor     = uybpBleedFactor p
                                , bpHeightLow       = uybpHeightLow p
                                , bpHeightHigh      = uybpHeightHigh p
                                , bpLayers          =
                                    [ ( maybe (uylMaterial l) id (uylName l)
                                      , uylMaterial l, uylThickness l )
                                    | l ← uybpLayers p ]
                                , bpTargetable      = uybpTargetable p
                                , bpDepth           = uybpDepth p
                                }
                            | p ← uydBodyParts def
                            ]
                        natRes = NaturalResistance
                            { nrSlash = uynrSlash (uydNaturalResistance def)
                            , nrStab  = uynrStab  (uydNaturalResistance def)
                            , nrBlunt = uynrBlunt (uydNaturalResistance def)
                            }
                        toStrike s = StrikeProfile
                            { spEff        = uysEff s
                            , spMaterial   = uysMaterial s
                            , spBladeCm    = uysBladeLength s
                            , spSharpness  = uysSharpness s
                            , spImpactArea = uysImpactArea s
                            , spMass       = uysMass s
                            , spLength     = if uysLength s > 0
                                             then uysLength s
                                             else uysBladeLength s
                            , spCenterOfMass = uysCenterOfMass s
                            , spName         = uysName s
                            }
                        natWeapon = case uydNaturalWeapon def of
                            Nothing → Nothing
                            Just nw → Just NaturalWeapon
                                { nwWeaponClass          = uynwWeaponClass nw
                                , nwEffectiveBladeLength = uynwEffectiveBladeLength nw
                                , nwAttackCooldown       = uynwAttackCooldown nw
                                , nwSlash                = toStrike (uynwSlash nw)
                                , nwStab                 = toStrike (uynwStab nw)
                                , nwBlunt                = toStrike (uynwBlunt nw)
                                , nwComboAttack          = uynwComboAttack nw
                                }
                        defMods =
                            [ ( uymStat m
                              , StatModifier
                                  { smDelta   = uymDelta m
                                  , smSource  = uymSource m
                                  , smExpiry  = Nothing
                                  , smPercent = uymPercent m
                                  }
                              )
                            | m ← uydModifiers def
                            ]
                        unitDef = UnitDef
                            { udName          = name
                            , udNamePool      = mNamePool
                            , udDisplayName   = uydDisplayName def
                            , udTexture       = handle
                            , udPortrait      = portraitH
                            , udDirSprites    = dirMap
                            , udBaseWidth     = uydBaseWidth def
                            , udMaxSpeed      = uydMaxSpeed def
                            , udRunThreshold  = uydRunThreshold def
                            , udAnimations    = animMap
                            , udStateAnims    = stateAnims
                            , udEagerStats    = uydEagerStats def
                            , udStatTemplates = statTemplates
                            , udBodyTemplates = bodyTemplates
                            , udSkillTemplates = skillTemplates
                            , udKnowledgeTemplates = knowledgeTemplates
                            , udStartingInventory = startingInv
                            , udEquipmentClass    = uydEquipmentClass def
                            , udStartingEquipment = HM.fromList
                                (Map.toList (uydStartingEquipment def))
                            , udStartingAccessories = uydStartingAccessories def
                            , udBodyParts        = bodyParts
                            , udNaturalResistance = natRes
                            , udNaturalWeapon    = natWeapon
                            , udModifiers        = defMods
                            }
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        (um { umDefs = HM.insert name unitDef (umDefs um) }, ())

                    logDebug logger CatAsset $
                        "Registered unit def: " <> name
                        <> " (handle " <> T.pack (show handle) <> ")"
                        <> " (" <> T.pack (show (Map.size dirMap))
                        <> " directional sprites, "
                        <> T.pack (show (HM.size animMap))
                        <> " animations)"

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadUnitYaml: loaded " <> T.pack (show total)
                    <> " unit definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | Prettify a def name for UI display when no explicit display_name is
--   set: underscores → spaces, each word capitalised. "bear_brown" →
--   "Bear Brown", "acolyte" → "Acolyte".
prettifyDefName ∷ Text → Text
prettifyDefName = T.unwords . map capWord . T.words . T.map underToSpace
  where
    underToSpace c = if c ≡ '_' then ' ' else c
    capWord w = case T.uncons w of
        Nothing      → w
        Just (c, cs) → T.cons (toUpperC c) cs
    toUpperC c = if c ≥ 'a' ∧ c ≤ 'z'
                 then toEnum (fromEnum c - 32)
                 else c

-- | Surface Z at a tile in ONE specific world. The unit's height must
--   come from the same page the unit is stamped into — walking wmVisible
--   instead can read another page's terrain (or 0) when more than one
--   world is live, which reintroduces #196 as a wrong-height spawn.
surfaceZInWorld ∷ WorldState → Int → Int → IO (Maybe Int)
surfaceZInWorld ws gx gy = do
    let (chunkCoord, (lx, ly)) = globalToChunk gx gy
    td ← readIORef (wsTilesRef ws)
    pure $ case lookupChunk chunkCoord td of
        Just lc → Just ((lcSurfaceMap lc) VU.! columnIndex lx ly)
        Nothing → Nothing

-- | Spawn a unit. If gridZ is omitted, looks up surface elevation.
--   Falls back to Z=0 if chunk isn't loaded. Returns unit ID or -1.
--
--   Signature: unit.spawn(defName, gx, gy, [gz], [factionId], [pageId])
--   factionId is the spawn-time faction tag — "player" for player-
--   controlled, "wildlife" for everything else. Defaults to
--   "wildlife" when omitted. The arg can sit at slot 4 (when gz is
--   omitted) or slot 5 (when both are supplied); both shapes work
--   so callers don't have to pass an explicit nil for gz.
--   pageId (slot 6) optionally pins the spawn to a specific live world
--   page instead of the active one. A building spawning a unit must
--   pass its OWN page here: scoping the caller's per-tick scan to the
--   active page is not enough, because the active page can change (a
--   queued world.show/hide on the world thread) between the scan and
--   this call, which would otherwise route the unit into the wrong
--   world (#196). Omitted → the active world, as before.
unitSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSpawnFn env = do
    nameArg     ← Lua.tostring 1
    xArg        ← Lua.tonumber 2
    yArg        ← Lua.tonumber 3
    -- Discriminate slot 4 by Lua type, not by coercion. `tointeger`
    -- succeeds on numeric strings (Lua auto-coerces), so a numeric
    -- faction tag like "5" would silently land in the z-slot and
    -- the faction would default to "wildlife". The actual Lua type
    -- tag is set by the caller and isn't subject to coercion.
    slot4Ty     ← Lua.ltype 4
    zArg        ← case slot4Ty of
        Lua.TypeNumber → Lua.tointeger 4
        _              → return Nothing
    factionArg4 ← case slot4Ty of
        Lua.TypeString → Lua.tostring 4
        _              → return Nothing
    factionArg5 ← Lua.tostring 5
    pageArg6    ← Lua.tostring 6

    case nameArg of
        Nothing → do
            Lua.pushnumber (-1)
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
                gx = case xArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0
                -- Resolve faction: slot 5 wins if present, else slot 4
                -- (only when it's actually a Lua string), else default.
                factionId = case factionArg5 of
                    Just fbs → TE.decodeUtf8 fbs
                    Nothing → case factionArg4 of
                        Just fbs → TE.decodeUtf8 fbs
                        Nothing  → "wildlife"

            result ← Lua.liftIO $ do
                -- Check def exists
                um ← readIORef (unitManagerRef env)
                -- Resolve the world the unit will belong to. An explicit
                -- pageId (slot 6) pins it to that live page; otherwise it
                -- defaults to the active world (#78). A unit needs a world
                -- to live in, so reject the spawn when the target page
                -- doesn't resolve to a live world.
                mActive ← case pageArg6 of
                    Just pbs → do
                        let pid = WorldPageId (TE.decodeUtf8 pbs)
                        wm ← readIORef (worldManagerRef env)
                        pure $ (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                    Nothing  → activeWorldPage env
                case (HM.lookup name (umDefs um), mActive) of
                    (Nothing, _) → return (-1)
                    (_, Nothing) → do
                        logger ← readIORef (loggerRef env)
                        logWarn logger CatAsset
                            "unit.spawn: no world to spawn into"
                        return (-1)
                    (Just _, Just (pageId, ws)) → do
                        -- Resolve Z from the SAME page the unit is stamped
                        -- into (ws), not whatever world happens to be
                        -- visible — otherwise an explicit pageId could be
                        -- stamped correctly yet take another page's height
                        -- (or 0) when several worlds are live (#196).
                        gz ← case zArg of
                            Just n  → return (fromIntegral n)
                            Nothing → do
                                let gxi = floor gx ∷ Int
                                    gyi = floor gy ∷ Int
                                mSurf ← surfaceZInWorld ws gxi gyi
                                case mSurf of
                                    Just z  → return z
                                    Nothing → do
                                        logger ← readIORef (loggerRef env)
                                        logWarn logger CatAsset $
                                            "unit.spawn: chunk not loaded at ("
                                            <> T.pack (show gxi) <> ", "
                                            <> T.pack (show gyi)
                                            <> "), defaulting Z=0"
                                        return 0

                        -- Allocate ID
                        uid ← atomicModifyIORef' (unitManagerRef env) $ \um' →
                            let (uid', um'') = nextUnitId um'
                            in (um'', uid')

                        -- Enqueue spawn command, stamped with the active
                        -- world so the unit is world-scoped (#78).
                        Q.writeQueue (unitQueue env) $
                            UnitSpawn uid name gx gy gz factionId pageId

                        return (fromIntegral (unUnitId uid) ∷ Int)

            Lua.pushnumber (Lua.Number (fromIntegral result))
            return 1

unitDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDestroy uid
            Lua.pushboolean True
            return 1

-- | Teleport a unit. If gridZ is omitted, looks up surface elevation.
unitSetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetPosFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← Lua.tonumber 2
    yArg  ← Lua.tonumber 3
    zArg  ← Lua.tointeger 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                gx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                mGz = case zArg of
                         Just z  → Just (fromIntegral z)
                         Nothing → Nothing
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTeleport uid gx gy mGz
            Lua.pushboolean True
            return 1

-- | Order a unit to walk to a target. Speed defaults to 2.0 tiles/sec.
unitMoveToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitMoveToFn env = do
    idArg    ← Lua.tointeger 1
    xArg     ← Lua.tonumber 2
    yArg     ← Lua.tonumber 3
    speedArg ← Lua.tonumber 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                tx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                ty = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                speed = case speedArg of
                            Just (Lua.Number v) → realToFrac v
                            _                   → 2.0
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitMoveTo uid tx ty speed
            Lua.pushboolean True
            return 1

-- | unit.jump(uid, gx, gy) — order a unit to LEAP to target tile (gx,gy).
--   The unit thread launches a gravity arc if the gap is within reach
--   (jumping skill + agility/strength) and the unit is standing; otherwise
--   it's a no-op. Returns true if the command was enqueued (not whether
--   the leap will be in range — that's decided on the unit thread).
unitJumpFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitJumpFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← Lua.tointeger 2
    yArg  ← Lua.tointeger 3
    case (idArg, xArg, yArg) of
        (Just n, Just tx, Just ty) → do
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitJump (UnitId (fromIntegral n))
                         (fromIntegral tx) (fromIntegral ty)
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

unitStopFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitStopFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitStop uid
            Lua.pushboolean True
            return 1

-- | unit.collapse(id) — transition the unit into the Collapsed state.
--   The state's anim is resolved via udStateAnims ("collapsed" → name);
--   a non-looping anim plays once and pickFrame holds the last frame.
--   Collapsed units ignore subsequent UnitMoveTo commands.
unitCollapseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCollapseFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitCollapse uid
            Lua.pushboolean True
            return 1

-- | unit.crawl(id) — drop a conscious-but-can't-walk unit (legs broken
--   or severed) to a sustained Crawling pose. Unlike collapse, a crawling
--   unit still accepts move commands and crawls slowly toward its goal;
--   any in-flight target is preserved. unit.revive stands it back up.
unitCrawlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCrawlFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitCrawl uid
            Lua.pushboolean True
            return 1

-- | unit.revive(id) — transition a Collapsed unit through the
--   Reviving state and back to Idle. The reviving-state anim plays
--   (typically the collapse anim in reverse via uiAnimReverse). No-op
--   if the unit isn't currently Collapsed.
unitReviveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitReviveFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitRevive uid
            Lua.pushboolean True
            return 1

-- | unit.recomputeBody(uid) — re-derive strength / max_hydration /
--   max_hunger / carrying_capacity from the unit's current body_mass /
--   lean_mass / fat_mass. Call this from Lua after directly mutating
--   any body-composition stat (Phase 3 regrowth, Phase 4 catabolism).
--   Returns true if the unit exists, false otherwise. No-op if the
--   unit's stat map is missing body_mass / lean_mass / height.
unitRecomputeBodyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRecomputeBodyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiStats =
                                recomputeBodyDerivedStats (uiStats inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1

-- | unit.kill(uid) — terminal. Snaps the unit to the Dead pose and
--   clears all in-flight state. Dead units are filtered out of AI,
--   ignore further commands, and never revive. Issued by the Lua
--   survival code when hydration drops below 5 % or stamina hits 0.
unitKillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitKillFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitKill uid
            Lua.pushboolean True
            return 1

-- | unit.drink(uid) — start the drinking animation on an Idle unit.
--   Engine handles the auto-revert to Idle when the anim finishes.
--   Stat / canteen effects are applied Lua-side BEFORE calling this
--   (see scripts/unit_ai drink action); the engine doesn't know about
--   sip amounts.
unitDrinkFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDrinkFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDrink uid
            Lua.pushboolean True
            return 1

-- | unit.eat(uid) — start the eating animation on an Idle unit.
--   Mirror of unit.drink. Nutrition + inventory mutation happen
--   Lua-side BEFORE this call (see scripts/unit_ai's eat_from_inventory
--   action) — the engine only owns the state/anim/timer.
unitEatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitEatFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitEat uid
            Lua.pushboolean True
            return 1

-- | unit.feed(uid, itemId) — consume one food item from the unit's
--   inventory and credit its calories to the hunger pool (clamped to
--   max_hunger; the overflow is wasted, the item still consumed). Plays
--   the eat animation. Returns the kcal actually credited on success, or
--   nil if the unit doesn't carry the item or the item has no food data
--   (the codebase signals failure with nil rather than raising). The
--   authoritative consume-and-credit primitive; the auto-eat AI routes
--   through it.
unitFeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitFeedFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
            mCredited ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                case lookupItemDef defName itemMgr >>= idFood of
                    Nothing   → pure Nothing   -- no food data → can't feed
                    Just food → atomicModifyIORef' (unitManagerRef env) $ \um →
                        case HM.lookup uid (umInstances um) of
                            Nothing → (um, Nothing)
                            -- Require a LIVE hunger pool: max_hunger is seeded
                            -- for any body unit (incl. wildlife), but only a
                            -- present "hunger" stat means the unit actually
                            -- runs the calorie system. Feeding one that
                            -- doesn't would conjure a permanent, never-
                            -- draining pool. Reject without consuming the item.
                            Just u  → case HM.lookup "hunger" (uiStats u) of
                                Nothing  → (um, Nothing)  -- no hunger system
                                Just cur → case removeFirstByName defName (uiInventory u) of
                                    Nothing     → (um, Nothing)  -- not carried
                                    Just newInv →
                                        let stats0 = uiStats u
                                            maxH   = HM.lookupDefault (cur + ifCalories food)
                                                         "max_hunger" stats0
                                            newH   = min maxH (cur + ifCalories food)
                                            u'     = u { uiInventory = newInv
                                                       , uiStats = HM.insert "hunger" newH stats0 }
                                        in ( um { umInstances = HM.insert uid u' (umInstances um) }
                                           , Just (newH - cur) )
            case mCredited of
                Just credited → do
                    Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitEat uid
                    Lua.pushnumber (Lua.Number (realToFrac credited))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getCalories(uid) — the unit's current calorie pool (the
--   "hunger" stat, kcal in the stomach). nil if the unit / stat is
--   absent. Pairs with max_hunger for the fraction.
unitGetCaloriesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetCaloriesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            let uid = UnitId (fromIntegral n)
            mVal ← Lua.liftIO $ getEffectiveStat env uid "hunger"
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        Nothing → do
            Lua.pushnil
            return 1

-- | unit.pickup(uid) — start the "picking up" animation on an Idle
--   unit. Same shape as drink; used for canteen refilling.
unitPickupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitPickupFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitPickup uid
            Lua.pushboolean True
            return 1

-- | unit.transitionTo(uid, poseName, stride?) — initiate a pose
--   transition. poseName is one of "standing", "crouching", "crawling",
--   "collapsed". Optional stride defaults to 1; pass 2 (or higher) to
--   skip frames when chaining transitions back-to-back.
--   No-op if the unit is already in that pose or mid-transition.
unitTransitionToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransitionToFn env = do
    idArg     ← Lua.tointeger 1
    mPoseBS   ← Lua.tostring 2
    mStrideArg ← Lua.tointeger 3
    let stride = case mStrideArg of
            Just s | s ≥ 1 → fromIntegral s
            _              → 1
    case (idArg, mPoseBS >>= parsePose . TE.decodeUtf8) of
        (Just n, Just target) → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTransitionTo uid target stride
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

parsePose ∷ Text → Maybe Pose
parsePose "standing"  = Just Standing
parsePose "crouching" = Just Crouching
parsePose "crawling"  = Just Crawling
parsePose "collapsed" = Just Collapsed
parsePose "climbing"  = Just Climbing
parsePose "falling"   = Just Falling
parsePose _           = Nothing

-- | unit.getPose(uid) — returns the unit's current pose as a string:
--   "standing" / "crouching" / "crawling" / "collapsed". nil if the
--   unit doesn't exist. Reads `uiPose`, mirrored from `usPose` by
--   Unit.Thread.publishToRender every tick.
unitGetPoseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPoseFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPose ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiPose <$> HM.lookup uid (umInstances um))
            case mPose of
                Just label → do
                    Lua.pushstring (TE.encodeUtf8 label)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.getFaction(uid) → string | nil
--   Returns the spawn-time-assigned faction tag ("player", "wildlife",
--   etc.). Used by the right-click menu's hostile/friendly check.
unitGetFactionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetFactionFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mFac ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiFactionId <$> HM.lookup uid (umInstances um))
            case mFac of
                Just f  → Lua.pushstring (TE.encodeUtf8 f) >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.exists(uid) → bool
--   True iff the engine still has a UnitInstance for this id. Used by
--   the AI to drop attack/move goals when their target is destroyed.
unitExistsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitExistsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            exists ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (HM.member uid (umInstances um))
            Lua.pushboolean exists
            return 1

-- | unit.getAttackRange(uid) → float (tiles) | nil
--
--   Computes melee reach as `(height / 2.4) + (blade_length / 100)`
--   where height is in metres (read from `uiStats["height"]`) and
--   blade_length is in centimetres. Looks up the blade length from
--   (in priority order):
--     1. equipped right_hand weapon
--     2. equipped left_hand weapon
--     3. unit-def's natural_weapon.effective_blade_length
--     4. 0 (bare arm reach only)
--   Returns nil if the unit doesn't exist OR has no rolled height
--   stat.
unitGetAttackRangeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAttackRangeFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mRange ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case HM.lookup "height" (uiStats inst) of
                        Nothing → return Nothing
                        Just h  →
                            let armReach = h / 2.4
                                blade    = resolveBladeLength im um inst
                            in return (Just (armReach + blade / 100.0))
            case mRange of
                Just r  → Lua.pushnumber (Lua.Number (realToFrac r))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getAttackCooldown(uid) → float seconds | nil
--
--   Cooldown between swings. Read from the equipped weapon
--   (right_hand → left_hand) or natural_weapon, with a 1.5s fallback.
unitGetAttackCooldownFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAttackCooldownFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mCD ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → return $ Just (resolveCooldown im um inst)
            case mCD of
                Just c  → Lua.pushnumber (Lua.Number (realToFrac c))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getAnimDuration(uid, animName) → float seconds | nil
--
--   Total play time of one animation: frame count / fps (frames are the
--   longest per-direction track). Used by combat to hold a one-shot
--   swing override on screen for its real length before the AI reverts
--   to the combat-idle stance — otherwise the next AI tick overwrites
--   the swing before a single frame shows.
unitGetAnimDurationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAnimDurationFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBs) → do
            let uid      = UnitId (fromIntegral n)
                animName = TE.decodeUtf8 nameBs
            mDur ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                        Nothing  → return Nothing
                        Just def → case HM.lookup animName (udAnimations def) of
                            Nothing → return Nothing
                            Just an →
                                let fps     = aFps an
                                    nFrames = maximum
                                        (0 : map V.length
                                                 (Map.elems (aFrames an)))
                                in if fps ≤ 0 ∨ nFrames ≡ 0
                                   then return Nothing
                                   else return
                                       (Just (fromIntegral nFrames / fps))
            case mDur of
                Just (d ∷ Float) → Lua.pushnumber (Lua.Number (realToFrac d))
                                       >> return 1
                Nothing          → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.getMaxSpeed(uid) → float (tiles/sec) | nil
--
--   Per-species top speed at a full sprint, read from the unit-def's
--   udMaxSpeed. AI candidates derive their per-action speed as a
--   fraction of this. Stat-based modulation (agility, injuries) lives
--   elsewhere — this returns the raw def-level cap.
unitGetMaxSpeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetMaxSpeedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mSpeed ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case HM.lookup (uiDefName inst)
                                              (umDefs um) of
                        Just d  → return (Just (udMaxSpeed d))
                        Nothing → return Nothing
            case mSpeed of
                Just s  → Lua.pushnumber (Lua.Number (realToFrac s))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getEquippedWeaponWeight(uid) → float kg | nil
--
--   The "weight" of whatever weapon would swing if this unit attacked
--   right now. For equipped weapons: the item's idWeight. For natural
--   weapons (bear paws, fists): derived as body_mass × 0.04 — a paw is
--   ~4% of the animal's mass, the same fraction across species so we
--   never need a per-creature knob. Returns nil if the unit doesn't
--   exist.
unitGetEquippedWeaponWeightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetEquippedWeaponWeightFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mW ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → return $ Just
                        (resolveWeaponWeight im inst)
            case mW of
                Just w  → Lua.pushnumber (Lua.Number (realToFrac w))
                            >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getWeaponWieldedFrom(uid) → string body-part id | nil
--
--   The body part the swing originates from. Used by the cooldown
--   formula to look up the wound severity on that part — injured
--   arms slow you down. For equipped weapons: derived from the slot
--   ("right_hand" → "right_arm", "left_hand" → "left_arm"). For
--   natural weapons: a species-default. Stage 4 will add a YAML
--   override (`wielded_from:` on weapon / natural_weapon blocks) for
--   creatures whose claws hang off a non-default body part.
unitGetWeaponWieldedFromFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWeaponWieldedFromFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPart ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → return $ Just
                        (resolveWieldedFrom (uiEquipment inst)
                                            (uiDefName inst))
            case mPart of
                Just p  → Lua.pushstring (TE.encodeUtf8 p) >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getWoundSeverityOn(uid, partId) → float | nil
--
--   Sum of EFFECTIVE severity (heal eases it, necrosis floors it) for
--   all current wounds on the given body part — so the gate eases as the
--   wound mends, matching the bleed/pain consumers. Used by the AI's
--   attack-mode picker and cooldown formula to gate heavy attacks when
--   the weapon arm is hurt. Returns 0 (not nil) for
--   a part with no wounds — only returns nil if the unit itself
--   doesn't exist.
unitGetWoundSeverityOnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWoundSeverityOnFn env = do
    idArg   ← Lua.tointeger 1
    partArg ← Lua.tostring 2
    case (idArg, partArg) of
        (Just n, Just pbs) → do
            let uid     = UnitId (fromIntegral n)
                partId  = TE.decodeUtf8 pbs
            mTotal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let s = sum [ woundEffSeverity w
                                    | w ← uiWounds inst
                                    , woundPart w ≡ partId ]
                        in return (Just s)
            case mTotal of
                Just s  → Lua.pushnumber (Lua.Number (realToFrac s))
                            >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | Walk slot priority for an equipped weapon's blade length; fall
--   back to the unit-def's natural_weapon; finally 0.
resolveBladeLength
    ∷ ItemManager → UnitManager → UnitInstance → Float
resolveBladeLength im um inst =
    case firstEquippedWeapon im (uiEquipment inst) of
        Just w  → iwBladeLength w
        Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
            Just d  → maybe 0.0 nwEffectiveBladeLength (udNaturalWeapon d)
            Nothing → 0.0

-- | Weapon weight for the currently-wielded weapon. Equipped item:
--   idWeight (kg). Natural weapon: body_mass × 0.04 — a small fraction
--   of body mass that scales correctly across species (a bear paw at
--   ~12 kg, a small predator's claw at <1 kg) without per-species
--   tuning. Returns 0 if no weapon and no body_mass stat exists.
resolveWeaponWeight ∷ ItemManager → UnitInstance → Float
resolveWeaponWeight im inst =
    case firstEquippedItemDef im (uiEquipment inst) of
        Just d  → idWeight d
        Nothing →
            let bodyMass = HM.lookupDefault 0.0 "body_mass" (uiStats inst)
            in  bodyMass * 0.04

-- | Body part the wielded weapon swings from. Slot-driven for
--   equipped weapons; species-defaulted for natural weapons. Returns
--   a body-part id that unit.getWoundSeverityOn can look up — must
--   match the `id` declared in the unit YAML's body_parts list.
--
--   Future: prefer a YAML `wielded_from:` field on the weapon /
--   natural_weapon block once that's parsed, so creatures whose
--   claws hang off non-default body parts (tail strikes, head butts)
--   can declare it in data.
resolveWieldedFrom
    ∷ HM.HashMap Text ItemInstance → Text → Text
resolveWieldedFrom eq defName
    | HM.member "right_hand" eq = "r_arm"
    | HM.member "left_hand"  eq = "l_arm"
    | otherwise                  = case defName of
        "bear_brown" → "r_fore_leg"
        "acolyte"    → "r_arm"
        _            → "head"

-- | Same priority chain for cooldown; default 1.5 s if nothing
--   useful is found.
resolveCooldown
    ∷ ItemManager → UnitManager → UnitInstance → Float
resolveCooldown im um inst =
    case firstEquippedWeapon im (uiEquipment inst) of
        Just w  → iwAttackCooldown w
        Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
            Just d  → maybe 1.5 nwAttackCooldown (udNaturalWeapon d)
            Nothing → 1.5

firstEquippedWeapon
    ∷ ItemManager → HM.HashMap Text ItemInstance → Maybe ItemWeapon
firstEquippedWeapon im eq = go ["right_hand", "left_hand"]
  where
    go [] = Nothing
    go (slot:rest) = case HM.lookup slot eq of
        Nothing → go rest
        Just it → case lookupItemDef (iiDefName it) im of
            Just d | Just w ← idWeapon d → Just w
            _ → go rest

-- | Same slot priority as firstEquippedWeapon, but returns the full
--   ItemDef so callers can read non-weapon fields (idWeight, idMaterial).
--   Only matches slots whose item has a weapon block — i.e. the unit
--   is actually wielding a weapon, not a torch.
firstEquippedItemDef
    ∷ ItemManager → HM.HashMap Text ItemInstance → Maybe ItemDef
firstEquippedItemDef im eq = go ["right_hand", "left_hand"]
  where
    go [] = Nothing
    go (slot:rest) = case HM.lookup slot eq of
        Nothing → go rest
        Just it → case lookupItemDef (iiDefName it) im of
            Just d | Just _ ← idWeapon d → Just d
            _ → go rest

-- | unit.getWounds(uid) → array of { part, kind, severity, at } | nil
--
--   Newest-first. Sub-cleanup-threshold wounds are removed by the
--   per-tick wound subsystem so this only returns currently-active
--   wounds. Returns nil if unit doesn't exist.
unitGetWoundsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWoundsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            infMgr ← Lua.liftIO $ readIORef (infectionManagerRef env)
            mWounds ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    let parts = maybe [] udBodyParts
                                  (HM.lookup (uiDefName inst) (umDefs um))
                    pure (uiWounds inst, parts)
            case mWounds of
                Nothing → Lua.pushnil >> return 1
                Just (ws, parts) → do
                    -- Index for the subpart → macro-part rollup + vital
                    -- lookup, so Lua impairment/death logic can work on
                    -- subpart wounds without knowing the body tree.
                    let idx = HM.fromList [(bpId p, p) | p ← parts]
                        vitalOf pid = maybe False bpVital (HM.lookup pid idx)
                        -- Walk parents until a targetable macro-part (the
                        -- limb the combat log names); bounded by part count.
                        macroOf pid = climb (length parts) pid
                        climb 0 pid = pid
                        climb k pid = case HM.lookup pid idx of
                            Just p | bpTargetable p → pid
                                   | otherwise → case bpParent p of
                                       Just par → climb (k - 1) par
                                       Nothing  → pid
                            Nothing → pid
                    Lua.newtable
                    forM_ (zip [1..] ws) $ \(i, w) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (woundPart w))
                        Lua.setfield (-2) "part"
                        Lua.pushstring (TE.encodeUtf8 (macroOf (woundPart w)))
                        Lua.setfield (-2) "macro"
                        Lua.pushboolean (vitalOf (woundPart w))
                        Lua.setfield (-2) "vital"
                        Lua.pushstring (TE.encodeUtf8 (woundKind w))
                        Lua.setfield (-2) "kind"
                        -- `severity` is the ACUTE/mechanical effective
                        -- severity (inflicted × (1 − heal)) — the trauma
                        -- itself, easing as the wound mends. It deliberately
                        -- does NOT fold in the necrosis floor: rot is a
                        -- separate failure mode (gangrene/sepsis, with its
                        -- own death path) reported separately as `necrosis`,
                        -- so the acute organ-failure meters (suffocation,
                        -- neuro, shock, visceral) read this without a rotting
                        -- wound spuriously driving an acute-trauma meter.
                        -- Consumers that drive IMPAIRMENT/bleed (limp/crawl,
                        -- injured-anim, bleeding) read `severityEffective`
                        -- below instead. `severityInflicted` is the original,
                        -- `heal` the 0..1 healing progress.
                        Lua.pushnumber (Lua.Number (realToFrac
                            (woundSeverity w * (1 - woundHeal w))))
                        Lua.setfield (-2) "severity"
                        -- `severityEffective` is the engine's full effective
                        -- severity — max(acute, necrosis) via woundEffSeverity,
                        -- the SAME value the Haskell bleed/pain/impairment
                        -- paths (bleedRateFor, painFor, injurySpeedMult, …)
                        -- use. Lua consumers that must stay in lockstep with
                        -- those (locomotion limp/crawl, injured-anim, bleeding
                        -- badge) read this so a healed-but-necrotic wound still
                        -- impairs, matching the engine.
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundEffSeverity w)))
                        Lua.setfield (-2) "severityEffective"
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundSeverity w)))
                        Lua.setfield (-2) "severityInflicted"
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundHeal w)))
                        Lua.setfield (-2) "heal"
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundAt w)))
                        Lua.setfield (-2) "at"
                        -- First-aid dressing: fraction of natural bleed
                        -- that still seeps (1.0 untreated, < 1.0 = bandaged).
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundBandage w)))
                        Lua.setfield (-2) "bandage"
                        -- Clotting progress 0..1 (bleed × (1−clot); 1 = stopped).
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundClot w)))
                        Lua.setfield (-2) "clot"
                        -- Dressing type: "" / "bandage" / "tourniquet".
                        Lua.pushstring (TE.encodeUtf8 (woundDressing w))
                        Lua.setfield (-2) "dressing"
                        -- Infection level 0..1 (drives the sepsis meter;
                        -- antibiotics cure it) and the clean/disinfected flag
                        -- (antiseptic prevention — a clean wound won't infect).
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundInfection w)))
                        Lua.setfield (-2) "infection"
                        Lua.pushboolean (woundClean w)
                        Lua.setfield (-2) "clean"
                        -- Which infection (id) + its display name / icon /
                        -- category, resolved from the catalogue. Empty id =
                        -- not yet typed (generic "infected" naming in Lua).
                        Lua.pushstring (TE.encodeUtf8 (woundInfectionType w))
                        Lua.setfield (-2) "infectionType"
                        let mInf = if woundInfectionType w ≡ ""
                                     then Nothing
                                     else lookupInfection (woundInfectionType w) infMgr
                        Lua.pushstring (TE.encodeUtf8
                            (maybe "" infName mInf))
                        Lua.setfield (-2) "infectionName"
                        Lua.pushstring (TE.encodeUtf8
                            (maybe "" infIcon mInf))
                        Lua.setfield (-2) "infectionIcon"
                        Lua.pushstring (TE.encodeUtf8
                            (maybe "" infCategory mInf))
                        Lua.setfield (-2) "infectionCategory"
                        -- Necrosis (dead tissue) 0..1: drives the rot display
                        -- + the gangrene death cause.
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundNecrosis w)))
                        Lua.setfield (-2) "necrosis"
                        Lua.rawseti (-2) i
                    return 1

-- | unit.getScars(uid) → array of { part, kind, severity, at } | nil
--   Permanent marks left by severe wounds that finished healing.
--   `severity` is the wound's inflicted severity (how bad it was);
--   `at` is the game-time it healed. Empty table if none; nil if the
--   unit doesn't exist.
unitGetScarsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetScarsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mScars ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiScars <$> HM.lookup uid (umInstances um))
            case mScars of
                Nothing → Lua.pushnil >> return 1
                Just scars → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] scars) $ \(i, sc) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (scarPart sc))
                        Lua.setfield (-2) "part"
                        Lua.pushstring (TE.encodeUtf8 (scarKind sc))
                        Lua.setfield (-2) "kind"
                        Lua.pushnumber (Lua.Number (realToFrac (scarSeverity sc)))
                        Lua.setfield (-2) "severity"
                        Lua.pushnumber (Lua.Number (realToFrac (scarAt sc)))
                        Lua.setfield (-2) "at"
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | unit.getImmunities(uid) → array of { type, name, icon, level } | nil.
--   Acquired per-type immunity (level 0..1), resolved to display name + icon
--   from the infection catalogue (falls back to the id + immunity.png).
--   Sorted strongest-first. Empty table if none.
unitGetImmunitiesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetImmunitiesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            infMgr ← Lua.liftIO $ readIORef (infectionManagerRef env)
            mImm ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiImmunities <$> HM.lookup uid (umInstances um))
            case mImm of
                Nothing → Lua.pushnil >> return 1
                Just immMap → do
                    let rows = L.sortBy (\a b → compare (snd b) (snd a))
                                        (HM.toList immMap)
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] rows) $ \(i, (tid, lvl)) → do
                        let mInf = lookupInfection tid infMgr
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 tid)
                        Lua.setfield (-2) "type"
                        Lua.pushstring (TE.encodeUtf8 (maybe tid infName mInf))
                        Lua.setfield (-2) "name"
                        Lua.pushstring (TE.encodeUtf8 "immunity")
                        Lua.setfield (-2) "icon"
                        Lua.pushnumber (Lua.Number (realToFrac lvl))
                        Lua.setfield (-2) "level"
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | unit.getInsulation(uid) → total thermal insulation (Float) of the unit's
--   worn gear — summed `idInsulation` over equipped items + accessories. Read
--   by scripts/thermo.lua to slow heat loss (dress for the climate). 0 if the
--   unit doesn't exist or wears nothing insulating.
unitGetInsulationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInsulationFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnumber 0 >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            total ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → 0
                    Just inst →
                        let worn = HM.elems (uiEquipment inst)
                                 ++ uiAccessories inst
                            insOf it = maybe 0 idInsulation
                                         (lookupItemDef (iiDefName it) im)
                        in sum (map insOf worn)
            Lua.pushnumber (Lua.Number (realToFrac (total ∷ Float)))
            return 1

-- | unit.getBlood(uid) → { current, max } | nil
--
--   max is body_mass × 0.075 (real-world blood ratio). current is
--   the spawn-time-seeded value minus per-tick bleed loss. Both in
--   litres.
unitGetBloodFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetBloodFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPair ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let bm = HM.lookupDefault 70.0 "body_mass"
                                                 (uiStats inst)
                            rate = maybe 0.0 (\d → bleedRateFor d inst)
                                     (HM.lookup (uiDefName inst) (umDefs um))
                        in return $ Just (uiBlood inst, bm * 0.075, rate)
            case mPair of
                Nothing → Lua.pushnil >> return 1
                Just (cur, mx, rate) → do
                    Lua.newtable
                    Lua.pushnumber (Lua.Number (realToFrac cur))
                    Lua.setfield (-2) "current"
                    Lua.pushnumber (Lua.Number (realToFrac mx))
                    Lua.setfield (-2) "max"
                    Lua.pushnumber (Lua.Number (realToFrac rate))
                    Lua.setfield (-2) "bleedRate"
                    return 1

-- | unit.getWeaponClass(uid) → string | nil
--
--   The active weapon-class string ("dagger", "unarmed", …) the
--   unit would actually use to swing right now. Resolution chain:
--   equipped right_hand → equipped left_hand → unit-def's
--   natural_weapon → "unarmed" fallback. Used by the AI's combat
--   effectiveness helper to read the relevant skill stat off the
--   unit's skills map.
unitGetWeaponClassFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWeaponClassFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mClass ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case firstEquippedWeapon im
                                       (uiEquipment inst) of
                        Just w  → return $ Just (iwWeaponClass w)
                        Nothing →
                            case HM.lookup (uiDefName inst) (umDefs um) of
                                Just d → case udNaturalWeapon d of
                                    Just nw → return $ Just
                                                  (nwWeaponClass nw)
                                    Nothing → return $ Just "unarmed"
                                Nothing → return $ Just "unarmed"
            case mClass of
                Just c  → Lua.pushstring (TE.encodeUtf8 c)
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getLastAttacker(uid) → { uid, at } | nil
--
--   Reads the runtime-only last-attacker memory written by
--   Combat.Resolution. Returns nil if no incoming hit has landed
--   yet on this unit. The AI is responsible for deciding whether
--   the memory is fresh enough to act on (compare `at` against
--   `engine.gameTime()`).
unitGetLastAttackerFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetLastAttackerFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAtt ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing   → return Nothing
                    Just inst → case uiLastAttackerUid inst of
                        Nothing     → return Nothing
                        Just attRaw → return $ Just
                                          (attRaw, uiLastAttackerAt inst)
            case mAtt of
                Nothing → Lua.pushnil >> return 1
                Just (attRaw, at) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral attRaw)
                    Lua.setfield (-2) "uid"
                    Lua.pushnumber (Lua.Number (realToFrac at))
                    Lua.setfield (-2) "at"
                    return 1

-- | unit.getPain(uid) → number | nil
--
--   Pain accumulator derived live from the wound list:
--     pain = sum(effective_severity × kind_pain_factor)
--   (effective severity eases as the wound heals, floored by necrosis).
--   Used by the AI (future retreat candidate), the unit-info UI,
--   and the combat hit-roll's pain penalty (computed Haskell-side
--   in Combat.Resolution; this getter mirrors the same formula
--   for Lua reads). Unclamped (the resolution code clamps via
--   painCeiling internally).
unitGetPainFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPainFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPain ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let pain = sum
                              [ woundEffSeverity w * kindPainFactor
                                                      (woundKind w)
                              | w ← uiWounds inst ]
                        in return $ Just pain
            case mPain of
                Just p  → Lua.pushnumber (Lua.Number (realToFrac p))
                                >> return 1
                Nothing → Lua.pushnil >> return 1
  where
    -- Mirror Combat.Resolution.kindPainFactor. Kept in lockstep
    -- manually; if either changes, update both.
    kindPainFactor ∷ Text → Float
    kindPainFactor "slash" = 1.0
    kindPainFactor "stab"  = 1.2
    kindPainFactor "blunt" = 1.5
    kindPainFactor _       = 1.0

-- | unit.modifyItemFill(uid, defName, delta) → actual applied delta
--   (after clamp to [0, capacity]). Adjusts the fill of the FIRST item
--   matching defName. Returns 0 if no such item / no container / unit
--   missing.
unitModifyItemFillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitModifyItemFillFn env = do
    idArg    ← Lua.tointeger 1
    nameArg  ← Lua.tostring 2
    deltaArg ← Lua.tonumber 3
    case (idArg, nameArg, deltaArg) of
        (Just n, Just nameBS, Just (Lua.Number d)) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
                delta   = realToFrac d ∷ Float
            applied ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Float)
                        Just inst →
                            let (newInv, app) =
                                    adjustFirstFill itemMgr defName delta
                                        (uiInventory inst)
                                inst' = inst { uiInventory = newInv }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, app)
            Lua.pushnumber (Lua.Number (realToFrac applied))
            return 1
        _ → do
            Lua.pushnumber 0
            return 1

-- | unit.repairItem(uid, instanceId, conditionDelta[, sharpnessDelta])
--   → table | nil. The low-level repair primitive (#300): the single
--   verb that ADJUSTS an item instance's two wear axes —
--   `iiCondition` (structural wear, gates breakage) and `iiSharpness`
--   (edge keenness, gates penetration) — in place, PRESERVING the
--   exact `iiInstanceId` (#67) so the physical item keeps its identity.
--
--   The item is found by `instanceId` across the unit's inventory,
--   equipment, AND worn accessories (so a degraded weapon can be
--   restored whether it's stowed or wielded, and a worn item like the
--   technogoggles is reachable too). Both deltas are applied additively
--   and the RESULT is clamped to 0..100. Positive restores (repair);
--   negative wears (the inverse — used by hazards / tests).
--   `sharpnessDelta` defaults to 0, so a furnace can pass condition-only
--   and a whetstone sharpness-only.
--
--   Repairing a WORN accessory also refreshes the unit's `uiModifiers`,
--   because accessory buffs can be condition-scaled (technogoggles'
--   perception buff) and are baked into the modifier map at equip time —
--   without the refresh a repaired accessory would keep its degraded
--   buff. (Equipped weapons/armour don't bake buffs into `uiModifiers`,
--   so the inventory/equipment branches need no such refresh.)
--
--   This is deliberately policy-free: restore *rates*, station↔axis
--   mapping, broken-item rules, and resource cost all live above it
--   (#301). It just moves the numbers and reports what actually changed.
--
--   Returns a table { defName, condition, sharpness, conditionApplied,
--   sharpnessApplied } with the post-clamp axis values and the actual
--   applied deltas (0 when an axis is already at a bound), or nil if the
--   unit doesn't exist or holds no instance with that id.
unitRepairItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRepairItemFn env = do
    idArg    ← Lua.tointeger 1
    instArg  ← Lua.tointeger 2
    condArg  ← Lua.tonumber 3
    sharpArg ← Lua.tonumber 4
    case (idArg, instArg) of
        (Just n, Just iidI) → do
            let uid    = UnitId (fromIntegral n)
                iid    = fromIntegral iidI ∷ Word64
                condD  = case condArg of
                    Just (Lua.Number d) → realToFrac d ∷ Float
                    _                   → 0
                sharpD = case sharpArg of
                    Just (Lua.Number d) → realToFrac d ∷ Float
                    _                   → 0
            -- For the accessory branch we may need the item def to refresh
            -- condition-scaled buffs; read the manager once up front.
            itemMgr ← Lua.liftIO $ readIORef (itemManagerRef env)
            mRes ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing   → (um, Nothing)
                    Just inst →
                        -- Inventory, then equipment, then worn accessories;
                        -- an instance id is unique to one physical item so at
                        -- most one branch fires.
                        let commit inst' = um { umInstances =
                                  HM.insert uid inst' (umInstances um) }
                        in case repairInList iid condD sharpD (uiInventory inst) of
                            Just (inv', _, r) →
                                (commit inst { uiInventory = inv' }, Just r)
                            Nothing →
                              case repairInEquip iid condD sharpD
                                                 (uiEquipment inst) of
                                Just (eq', _, r) →
                                    (commit inst { uiEquipment = eq' }, Just r)
                                Nothing →
                                  case repairInList iid condD sharpD
                                                    (uiAccessories inst) of
                                    Just (accs', _, r) →
                                        -- Re-derive buffs from the WHOLE worn
                                        -- list (with the repaired instance in
                                        -- it), in equip order, so a same-source
                                        -- duplicate's last-equipped copy stays
                                        -- the live one — repairing an older
                                        -- duplicate must not hijack the buff.
                                        let mods' = refreshAccessoryBuffs itemMgr
                                                      accs' (uiModifiers inst)
                                        in ( commit inst { uiAccessories = accs'
                                                         , uiModifiers   = mods' }
                                           , Just r )
                                    Nothing → (um, Nothing)
            case mRes of
                Nothing → Lua.pushnil >> return 1
                Just (defName, cond1, sharp1, cApp, sApp) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 defName)
                    Lua.setfield (-2) "defName"
                    Lua.pushnumber (Lua.Number (realToFrac cond1))
                    Lua.setfield (-2) "condition"
                    Lua.pushnumber (Lua.Number (realToFrac sharp1))
                    Lua.setfield (-2) "sharpness"
                    Lua.pushnumber (Lua.Number (realToFrac cApp))
                    Lua.setfield (-2) "conditionApplied"
                    Lua.pushnumber (Lua.Number (realToFrac sApp))
                    Lua.setfield (-2) "sharpnessApplied"
                    return 1
        _ → Lua.pushnil >> return 1

-- | Apply repair deltas to one instance, clamping each axis to 0..100.
--   Returns the new instance plus a report tuple (defName, newCondition,
--   newSharpness, conditionApplied, sharpnessApplied) where the *applied*
--   figures are the post-clamp differences — so a caller/test sees that
--   topping up an already-full axis applied 0.
applyRepair ∷ Float → Float → ItemInstance
            → (ItemInstance, (Text, Float, Float, Float, Float))
applyRepair condD sharpD it =
    let cond0  = iiCondition it
        sharp0 = iiSharpness it
        cond1  = clampWear (cond0 + condD)
        sharp1 = clampWear (sharp0 + sharpD)
    in ( it { iiCondition = cond1, iiSharpness = sharp1 }
       , (iiDefName it, cond1, sharp1, cond1 - cond0, sharp1 - sharp0) )
  where clampWear x = max 0 (min 100 x)

-- | Repair the first instance in a list whose id matches; Nothing if
--   none does. Preserves list order (the slot is rewritten in place).
--   Also returns the repaired instance so the caller can react to its
--   new condition (the accessory branch re-derives its buffs).
repairInList ∷ Word64 → Float → Float → [ItemInstance]
             → Maybe ([ItemInstance], ItemInstance
                     , (Text, Float, Float, Float, Float))
repairInList _   _     _      []     = Nothing
repairInList iid condD sharpD (x:xs)
    | iiInstanceId x ≡ iid =
        let (x', r) = applyRepair condD sharpD x in Just (x' : xs, x', r)
    | otherwise =
        (\(rest, ri, r) → (x : rest, ri, r))
            <$> repairInList iid condD sharpD xs

-- | Repair the equipped instance whose id matches; Nothing if no slot
--   holds it.
repairInEquip ∷ Word64 → Float → Float → HM.HashMap Text ItemInstance
              → Maybe ( HM.HashMap Text ItemInstance, ItemInstance
                      , (Text, Float, Float, Float, Float) )
repairInEquip iid condD sharpD eq =
    case [ (slot, it) | (slot, it) ← HM.toList eq, iiInstanceId it ≡ iid ] of
        ((slot, it) : _) →
            let (it', r) = applyRepair condD sharpD it
            in Just (HM.insert slot it' eq, it', r)
        [] → Nothing

-- | Re-derive worn-accessory buffs into the unit's `uiModifiers` after a
--   repair changed one accessory's condition, so a condition-scaled buff
--   (e.g. technogoggles' perception) tracks the repair. Folds the buffs
--   of EVERY worn accessory over the modifier map IN LIST ORDER, exactly
--   as a fresh sequence of `equipAccessory` calls would: same-source
--   modifiers collapse (dedup by display_name) so the LAST-equipped copy
--   of a duplicated accessory wins. Re-deriving from the whole list — not
--   just the repaired instance — is what keeps repairing an *older*
--   duplicate from silently switching the live buff to its condition.
refreshAccessoryBuffs ∷ ItemManager → [ItemInstance]
                      → HM.HashMap Text [StatModifier]
                      → HM.HashMap Text [StatModifier]
refreshAccessoryBuffs itemMgr accs mods0 =
    foldl' (\mods inst → applyAccessoryBuffs itemMgr inst mods) mods0 accs

-- | Apply one accessory's buffs to a modifier map. Mirrors
--   `Equipment.applyItemBuffs`: the modifier source is the item's
--   display_name and same-source modifiers on a stat collapse, so this
--   REPLACES that source's stale modifier rather than stacking. A no-op
--   for items with no buffs (or no def in scope).
applyAccessoryBuffs ∷ ItemManager → ItemInstance
                    → HM.HashMap Text [StatModifier]
                    → HM.HashMap Text [StatModifier]
applyAccessoryBuffs itemMgr inst mods =
    case lookupItemDef (iiDefName inst) itemMgr of
        Nothing   → mods
        Just iDef →
            let src  = idDisplayName iDef
                cond = iiCondition inst
                applyOne acc b =
                    let delta = if ibScalesWithCondition b
                                  then ibAmount b * (cond / 100)
                                  else ibAmount b
                        m = StatModifier { smDelta = delta, smSource = src
                                         , smExpiry = Nothing, smPercent = 0 }
                        existing = HM.lookupDefault [] (ibStat b) acc
                        others   = filter (\x → smSource x ≢ src) existing
                    in HM.insert (ibStat b) (m : others) acc
            in foldl' applyOne mods (idBuffs iDef)

-- | unit.addItem(uid, defName, fill) → bool. Adds a new ItemInstance
--   to the unit's inventory. Fill is clamped to the def's container
--   capacity (or zeroed for non-containers). Returns false if the
--   unit or item def doesn't exist.
unitAddItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddItemFn env = do
    idArg    ← Lua.tointeger 1
    nameArg  ← Lua.tostring 2
    fillArg  ← Lua.tonumber 3
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
                fillIn  = case fillArg of
                    Just (Lua.Number d) → realToFrac d ∷ Float
                    _ → 0
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                case lookupItemDef defName itemMgr of
                    Nothing → return False
                    Just def → do
                        let clampedFill = case idContainer def of
                                Just c  → max 0 (min fillIn (icCapacity c))
                                Nothing → 0
                        qual ← rollItemSpec (idQualitySpec def)
                                            (statRNGRef env)
                        cond ← rollItemSpec (idConditionSpec def)
                                            (statRNGRef env)
                        wght ← rollItemWeight def (statRNGRef env)
                        iid ← freshItemInstanceId env
                        let inst' = ItemInstance
                                { iiDefName     = defName
                                , iiCurrentFill = clampedFill
                                , iiQuality     = qual
                                , iiCondition   = cond
                                , iiWeight      = wght
                                , iiSharpness   = 100.0
                                , iiContents    = []
                                , iiInstanceId  = iid
                                }
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, False)
                                Just u  →
                                    let u' = u { uiInventory =
                                                  uiInventory u ++ [inst'] }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) },
                                        True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.removeItem(uid, defName) → bool. Removes the FIRST inventory
--   instance with the matching defName. Returns true if something was
--   removed, false if the unit doesn't exist or has no such item.
--   Used by Phase 5 eat_from_inventory to consume food after the
--   nutrition is applied.
unitRemoveItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRemoveItemFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
            removed ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just u  →
                        case removeFirstByName defName (uiInventory u) of
                            Nothing      → (um, False)
                            Just newInv  →
                                let u' = u { uiInventory = newInv }
                                in (um { umInstances = HM.insert uid u'
                                                        (umInstances um) }, True)
            Lua.pushboolean removed
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | Drop the first ItemInstance whose defName matches. Returns Nothing
--   if no match (caller can distinguish "removed" from "not found").
removeFirstByName ∷ Text → [ItemInstance] → Maybe [ItemInstance]
removeFirstByName _ [] = Nothing
removeFirstByName name (x:xs)
    | iiDefName x ≡ name = Just xs
    | otherwise          = (x :) <$> removeFirstByName name xs

-- | Like removeFirstByName but EXTRACTS the popped instance so the
--   caller can route it somewhere else (e.g. into a building's
--   biMaterialsDelivered).
popFirstByName ∷ Text → [ItemInstance] → Maybe (ItemInstance, [ItemInstance])
popFirstByName _ [] = Nothing
popFirstByName name (x:xs)
    | iiDefName x ≡ name = Just (x, xs)
    | otherwise          = fmap (\(it, rest) → (it, x:rest))
                                (popFirstByName name xs)

-- | Like 'popFirstByName' but also reports the 0-based index the item
--   was removed from. Cross-owner transfers carry this so a failed
--   move can splice the instance back at its ORIGINAL position rather
--   than the front — UI (unit.getInventory / building.getStorage) and
--   gameplay rely on stable insertion order, so a rolled-back transfer
--   must leave the source list byte-for-byte unchanged.
popFirstByNameIx ∷ Text → [ItemInstance]
                → Maybe (ItemInstance, Int, [ItemInstance])
popFirstByNameIx = go 0
  where
    go _ _    [] = Nothing
    go i name (x:xs)
        | iiDefName x ≡ name = Just (x, i, xs)
        | otherwise          = fmap (\(it, j, rest) → (it, j, x:rest))
                                    (go (i + 1) name xs)

-- | Insert @x@ at index @i@. If @i@ is past the end (the list shrank
--   under a concurrent edit between pop and rollback) it appends —
--   graceful degradation, never a crash.
insertAt ∷ Int → a → [a] → [a]
insertAt i x xs = let (pre, post) = splitAt i xs in pre ++ x : post

-- | Like 'popFirstByNameIx' but matches on an arbitrary predicate, so a
--   caller can target a specific 'iiInstanceId' (#67) instead of the
--   first defName match. Same all-or-nothing index reporting so a
--   rolled-back transfer can splice the instance back at its original
--   slot. Pair with 'itemMatches' to get id-then-defName fallback.
popFirstWhereIx ∷ (ItemInstance → Bool) → [ItemInstance]
                → Maybe (ItemInstance, Int, [ItemInstance])
popFirstWhereIx p = go 0
  where
    go _ [] = Nothing
    go i (x:xs)
        | p x       = Just (x, i, xs)
        | otherwise = fmap (\(it, j, rest) → (it, j, x:rest))
                           (go (i + 1) xs)

-- | unit.dropEquipmentToGround(uid, slotId) → bool
--
-- Removes whatever is equipped in `slotId` and drops it on the ground at
-- the unit's tile, PRESERVING the exact ItemInstance (condition /
-- sharpness / quality / fill). Used when a hand or arm is severed or
-- destroyed and the unit can no longer hold its weapon. Returns false if
-- the slot is empty, the unit is gone, or no world is active (the item is
-- left equipped rather than vanishing).
unitDropEquipmentToGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDropEquipmentToGroundFn env = do
    uidArg  ← Lua.tointeger 1
    slotArg ← Lua.tostring 2
    case (uidArg, slotArg) of
        (Just n, Just slotBS) → do
            let uid    = UnitId (fromIntegral n)
                slotId = TE.decodeUtf8 slotBS
            -- Resolve the world FIRST so we never strip the item from the
            -- unit when there's nowhere to drop it.
            mWs ← Lua.liftIO $ activeWorldU env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    mDrop ← Lua.liftIO $
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, Nothing)
                                Just inst →
                                    case HM.lookup slotId (uiEquipment inst) of
                                        Nothing → (um, Nothing)
                                        Just it →
                                            let inst' = inst
                                                  { uiEquipment =
                                                      HM.delete slotId
                                                        (uiEquipment inst) }
                                            in ( um { umInstances =
                                                        HM.insert uid inst'
                                                          (umInstances um) }
                                               , Just (it, uiGridX inst,
                                                           uiGridY inst) )
                    case mDrop of
                        Nothing → Lua.pushboolean False >> return 1
                        Just (it, gx, gy) → do
                            _ ← Lua.liftIO $
                                atomicModifyIORef' (wsGroundItemsRef ws) $
                                    spawnGroundItem it gx gy
                            Lua.pushboolean True
                            return 1
        _ → Lua.pushboolean False >> return 1

-- | The active (shown) world, or the first one if none is explicitly
--   shown. Mirrors the helper in API.Items.
activeWorldU ∷ EngineEnv → IO (Maybe WorldState)
activeWorldU = activeWorldState

-- | unit.transferItemToBuilding(uid, bid, defName) → bool. Atomic
--   move of one ItemInstance from the unit's inventory to the
--   building's biMaterialsDelivered list for that defName. Preserves
--   quality / condition / currentFill on the instance so a delivered
--   electric motor at 100% comes back out at 100% (or its degraded
--   state) on a future deconstruction.
--
--   Returns true on success. If the unit lacks a matching item OR
--   the building has vanished between pop and deliver, returns false
--   and logs a warning (the latter case is a tiny race window — the
--   AI just queried the building one tick prior).
unitTransferItemToBuildingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferItemToBuildingFn env = do
    uidArg  ← Lua.tointeger 1
    bidArg  ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    case (uidArg, bidArg, nameArg) of
        (Just nU, Just nB, Just nameBS) → do
            let uid     = UnitId (fromIntegral nU)
                bid     = BuildingId (fromIntegral nB)
                defName = TE.decodeUtf8 nameBS
            mItem ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, Nothing)
                    Just u →
                        case popFirstByNameIx defName (uiInventory u) of
                            Nothing → (um, Nothing)
                            Just (item, ix, newInv) →
                                let u' = u { uiInventory = newInv }
                                in (um { umInstances = HM.insert uid u'
                                                                (umInstances um) }
                                   , Just (item, ix))
            case mItem of
                Nothing → do
                    Lua.pushboolean False
                    return 1
                Just (item, ix) → do
                    delivered ← Lua.liftIO $
                        atomicModifyIORef' (buildingManagerRef env) $ \bm →
                            case HM.lookup bid (bmInstances bm) of
                                Nothing → (bm, False)
                                Just inst →
                                    let current = HM.lookupDefault [] defName
                                                    (biMaterialsDelivered inst)
                                        newMap  = HM.insert defName (item : current)
                                                    (biMaterialsDelivered inst)
                                        inst'   = inst
                                            { biMaterialsDelivered = newMap }
                                    in (bm { bmInstances = HM.insert bid inst'
                                                            (bmInstances bm) }
                                       , True)
                    unless delivered $ do
                        -- Destination vanished between pop and deliver:
                        -- splice the popped instance back into the
                        -- unit's inventory at its ORIGINAL index so the
                        -- move stays all-or-nothing — list order is
                        -- gameplay/UI-visible, so a rollback must leave
                        -- the source unchanged, not move the item.
                        restored ← Lua.liftIO $
                            atomicModifyIORef' (unitManagerRef env) $ \um →
                                case HM.lookup uid (umInstances um) of
                                    Nothing → (um, False)
                                    Just u →
                                        let u' = u { uiInventory =
                                                       insertAt ix item
                                                         (uiInventory u) }
                                        in (um { umInstances = HM.insert uid u'
                                                                (umInstances um) }
                                           , True)
                        unless restored $ do
                            logger ← Lua.liftIO $ readIORef (loggerRef env)
                            Lua.liftIO $ logWarn logger CatThread $
                                "transferItemToBuilding: building "
                                <> T.pack (show nB)
                                <> " gone between pop and deliver and unit "
                                <> T.pack (show nU)
                                <> " also vanished — "
                                <> defName <> " lost"
                    Lua.pushboolean delivered
                    return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.transferItemToUnit(fromUid, toUid, defName) → bool. Atomic
--   move of one ItemInstance from one unit's inventory to another's.
--   Both units live in the same manager ref, so the pop and the push
--   happen in a single atomicModifyIORef' — the item can never be
--   duplicated or dropped by a thread interleaving. Quality /
--   condition / currentFill are preserved exactly (this is how
--   acolytes pull build materials off the technomule without
--   re-rolling them). No capacity check here — the Lua caller gates
--   on carrying capacity the same way pickup does.
--   Returns false if either unit is missing or the source lacks a
--   matching item; the transfer is all-or-nothing.
unitTransferItemToUnitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferItemToUnitFn env = do
    fromArg ← Lua.tointeger 1
    toArg   ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    case (fromArg, toArg, nameArg) of
        (Just nF, Just nT, Just nameBS) | nF ≠ nT → do
            let fromUid = UnitId (fromIntegral nF)
                toUid   = UnitId (fromIntegral nT)
                defName = TE.decodeUtf8 nameBS
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case (HM.lookup fromUid (umInstances um),
                      HM.lookup toUid   (umInstances um)) of
                    (Just uF, Just uT) →
                        case popFirstByName defName (uiInventory uF) of
                            Nothing → (um, False)
                            Just (item, newInv) →
                                let uF' = uF { uiInventory = newInv }
                                    uT' = uT { uiInventory =
                                                 uiInventory uT ++ [item] }
                                    insts = HM.insert toUid uT'
                                          $ HM.insert fromUid uF'
                                          $ umInstances um
                                in (um { umInstances = insts }, True)
                    _ → (um, False)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.depositToCargo(uid, bid, defName) → bool. Moves one matching
--   ItemInstance from the unit's loose inventory into the building's
--   biStorage. Capacity-checked (rejects if the new total weight
--   would exceed bdStorageCapacity). Quality / condition / fill on
--   the instance are preserved exactly. No adjacency check — that
--   lives in the Lua caller (AI walks the unit close first; the
--   right-click menu only enables when the unit is adjacent).
unitDepositToCargoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDepositToCargoFn env = do
    uidArg  ← Lua.tointeger 1
    bidArg  ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    -- Optional 4th arg: deposit the EXACT inventory instance (#67), so a
    -- merged "Store" row stores the canteen the player sees, not the
    -- first defName match. Absent/0 → first match (AI auto-store).
    instArg ← Lua.tointeger 4
    case (uidArg, bidArg, nameArg) of
        (Just nU, Just nB, Just nameBS) → do
            let uid     = UnitId (fromIntegral nU)
                bid     = BuildingId (fromIntegral nB)
                defName = TE.decodeUtf8 nameBS
                wantId  = maybe 0 fromIntegral instArg
            -- Capacity pre-check: read-only snapshot. Weighs the ACTUAL
            -- ItemInstance that will be popped below (the first match in
            -- the unit's inventory) via itemTotalWeight, so fill and
            -- nested contents are counted — the same recursive measure
            -- used for the items already in storage. A filled container
            -- or stocked kit can be kilograms heavier than its def mean,
            -- so checking idWeight here would let it overfill the cargo.
            okFits ← Lua.liftIO $ do
                bm      ← readIORef (buildingManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                um      ← readIORef (unitManagerRef env)
                pure $ fromMaybe False $ do
                    inst         ← HM.lookup bid (bmInstances bm)
                    def          ← HM.lookup (biDefName inst) (bmDefs bm)
                    u            ← HM.lookup uid (umInstances um)
                    (item, _, _) ← popFirstWhereIx (itemMatches wantId defName)
                                                   (uiInventory u)
                    let cap     = bdStorageCapacity def
                        current = sum (map (itemTotalWeight itemMgr)
                                           (biStorage inst))
                    pure (cap > 0 ∧ current + itemTotalWeight itemMgr item <= cap)
            if not okFits then do
                Lua.pushboolean False
                return 1
            else do
                mItem ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, Nothing)
                        Just u →
                            case popFirstWhereIx (itemMatches wantId defName)
                                                 (uiInventory u) of
                                Nothing → (um, Nothing)
                                Just (item, ix, newInv) →
                                    let u' = u { uiInventory = newInv }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) }
                                       , Just (item, ix))
                case mItem of
                    Nothing → do
                        Lua.pushboolean False
                        return 1
                    Just (item, ix) → do
                        ok ← Lua.liftIO $
                            atomicModifyIORef' (buildingManagerRef env) $ \bm →
                                case HM.lookup bid (bmInstances bm) of
                                    Nothing → (bm, False)
                                    Just inst →
                                        let inst' = inst
                                                { biStorage = item : biStorage inst }
                                        in (bm { bmInstances =
                                                    HM.insert bid inst'
                                                        (bmInstances bm) }
                                           , True)
                        unless ok $ do
                            -- Destination building vanished between pop
                            -- and deposit: splice the popped instance back
                            -- into the unit's inventory at its ORIGINAL
                            -- index (all-or-nothing; order is visible).
                            restored ← Lua.liftIO $
                                atomicModifyIORef' (unitManagerRef env) $ \um →
                                    case HM.lookup uid (umInstances um) of
                                        Nothing → (um, False)
                                        Just u →
                                            let u' = u { uiInventory =
                                                           insertAt ix item
                                                             (uiInventory u) }
                                            in (um { umInstances =
                                                       HM.insert uid u'
                                                         (umInstances um) }
                                               , True)
                            unless restored $ do
                                logger ← Lua.liftIO $ readIORef (loggerRef env)
                                Lua.liftIO $ logWarn logger CatThread $
                                    "depositToCargo: building "
                                    <> T.pack (show nB)
                                    <> " gone between pop and deposit and unit "
                                    <> T.pack (show nU)
                                    <> " also vanished — "
                                    <> defName <> " lost"
                        Lua.pushboolean ok
                        return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.withdrawFromCargo(uid, bid, defName) → bool. Reverse of
--   depositToCargo: pops one matching ItemInstance from biStorage,
--   appends to the unit's loose inventory. Not gated by unit
--   carrying-capacity — units can hold above their cap (with stat
--   penalties handled elsewhere). Adjacency check lives in the Lua
--   caller, same as deposit.
unitWithdrawFromCargoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitWithdrawFromCargoFn env = do
    uidArg  ← Lua.tointeger 1
    bidArg  ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    -- Optional 4th arg: withdraw the EXACT stored instance (#67) the
    -- player clicked in the cargo panel, not the first defName match.
    -- The id targets an item in the BUILDING's storage (exposed by
    -- building.getStorage). Absent/0 → first match.
    instArg ← Lua.tointeger 4
    case (uidArg, bidArg, nameArg) of
        (Just nU, Just nB, Just nameBS) → do
            let uid     = UnitId (fromIntegral nU)
                bid     = BuildingId (fromIntegral nB)
                defName = TE.decodeUtf8 nameBS
                wantId  = maybe 0 fromIntegral instArg
            mItem ← Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, Nothing)
                    Just inst →
                        case popFirstWhereIx (itemMatches wantId defName)
                                             (biStorage inst) of
                            Nothing → (bm, Nothing)
                            Just (item, ix, newStorage) →
                                let inst' = inst { biStorage = newStorage }
                                in (bm { bmInstances = HM.insert bid inst'
                                                          (bmInstances bm) }
                                   , Just (item, ix))
            case mItem of
                Nothing → do
                    Lua.pushboolean False
                    return 1
                Just (item, ix) → do
                    ok ← Lua.liftIO $
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, False)
                                Just u →
                                    let u' = u
                                            { uiInventory = uiInventory u ++ [item] }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) }
                                       , True)
                    unless ok $ do
                        -- Destination unit vanished between pop and
                        -- append: splice the popped instance back into
                        -- the building's storage at its ORIGINAL index
                        -- (all-or-nothing; storage order is visible).
                        restored ← Lua.liftIO $
                            atomicModifyIORef' (buildingManagerRef env) $ \bm →
                                case HM.lookup bid (bmInstances bm) of
                                    Nothing → (bm, False)
                                    Just inst →
                                        let inst' = inst
                                                { biStorage = insertAt ix item
                                                                (biStorage inst) }
                                        in (bm { bmInstances = HM.insert bid inst'
                                                                  (bmInstances bm) }
                                           , True)
                        unless restored $ do
                            logger ← Lua.liftIO $ readIORef (loggerRef env)
                            Lua.liftIO $ logWarn logger CatThread $
                                "withdrawFromCargo: unit "
                                <> T.pack (show nU)
                                <> " gone between pop and append and building "
                                <> T.pack (show nB)
                                <> " also vanished — "
                                <> defName <> " lost"
                    Lua.pushboolean ok
                    return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getCarryingWeight(uid) → Float kg. Sum of full item weight
--   via 'itemTotalWeight' (instance weight + container fill at the
--   container's per-unit fill weight + nested container contents)
--   across loose inventory + equipped slot items + accessories. Worn
--   gear counts the same as carried gear by design: it's the same mass.
--   Used by the auto-store AI utility (fill_fraction = this / cap) and
--   the pickup/fetch capacity gates.
unitGetCarryingWeightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetCarryingWeightFn env = do
    uidArg ← Lua.tointeger 1
    case uidArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mW ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                let weightOf = itemTotalWeight itemMgr
                pure $ do
                    u ← HM.lookup uid (umInstances um)
                    let invW = sum (map weightOf (uiInventory u))
                        eqW  = sum (map weightOf (HM.elems (uiEquipment u)))
                        accW = sum (map weightOf (uiAccessories u))
                    pure (invW + eqW + accW ∷ Float)
            case mW of
                Just w → do
                    Lua.pushnumber (Lua.Number (realToFrac w))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | Helper: adjust the fill of the first ItemInstance matching defName.
--   Clamps to [0, capacity] looked up via the ItemManager. Returns the
--   resulting inventory and the actual applied delta (post-clamp).
adjustFirstFill
    ∷ ItemManager → Text → Float → [ItemInstance] → ([ItemInstance], Float)
adjustFirstFill itemMgr defName delta = go
  where
    go [] = ([], 0)
    go (x : xs)
      | iiDefName x ≡ defName =
          let cap = case lookupItemDef defName itemMgr of
                  Just d  → case idContainer d of
                      Just c  → icCapacity c
                      Nothing → iiCurrentFill x   -- no container → no headroom
                  Nothing → iiCurrentFill x
              newFill = max 0 (min cap (iiCurrentFill x + delta))
              applied = newFill - iiCurrentFill x
              x'      = x { iiCurrentFill = newFill }
          in (x' : xs, applied)
      | otherwise =
          let (xs', applied) = go xs
          in (x : xs', applied)

unitGetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPosFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.pushnumber (Lua.Number (fromIntegral (uiGridZ inst)))
                    return 3

-- * Skills

-- | unit.getSkill(uid, name) — EFFECTIVE skill value (base + active
--   modifier deltas, clamped at 0). Phase F: skills now share the
--   modifier pipeline with stats, so a modifier on "balance" shifts
--   getSkill's result too. nil if undefined.
unitGetSkillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetSkillFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → pure Nothing
                    Just inst → case HM.lookup name (uiSkills inst) of
                        Nothing → pure Nothing
                        Just base → do
                            now ← readIORef (gameTimeRef env)
                            let mods = HM.lookupDefault [] name
                                          (uiModifiers inst)
                            pure (Just (effectiveStat now base mods))
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.setSkill(uid, name, value) — debug override. Sets the skill
--   level directly. Clamps at 0 (negative skill is meaningless under
--   the XP formula). Returns true if the unit exists, false otherwise.
unitSetSkillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetSkillFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
                lvl  = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiSkills =
                                HM.insert name lvl (uiSkills inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getKnowledge(uid, name) → level | nil. nil means the unit does
--   NOT know it (presence in uiKnowledge = known). Raw stored level —
--   callers wanting effective capability multiply by the relevant stat
--   (e.g. intelligence) themselves.
unitGetKnowledgeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetKnowledgeFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ HM.lookup uid (umInstances um)
                         ⌦ (HM.lookup name . uiKnowledge)
            case mVal of
                Just v  → Lua.pushnumber (Lua.Number (realToFrac v)) >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.setKnowledge(uid, name, value) — grant/set a knowledge at `value`
--   (its mere presence marks it KNOWN). Clamps at 0. Used to seed knowledge
--   from a source (book/teacher) or for debug; spawn-known knowledge comes
--   from the unit def's `knowledge:` block. Returns true if the unit exists.
unitSetKnowledgeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetKnowledgeFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
                lvl  = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiKnowledge =
                                HM.insert name lvl (uiKnowledge inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.getKnowledgeList(uid) → array of { name, level } for everything the
--   unit KNOWS. Empty table if it knows nothing / doesn't exist.
unitGetKnowledgeListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetKnowledgeListFn env = do
    idArg ← Lua.tointeger 1
    Lua.newtable
    case idArg of
        Nothing → return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (HM.lookup uid (umInstances um))
            case mInst of
                Nothing → return 1
                Just inst → do
                    forM_ (zip [1 ∷ Int ..] (HM.toList (uiKnowledge inst)))
                        $ \(i, (k, v)) → do
                            Lua.newtable
                            Lua.pushstring (TE.encodeUtf8 k)
                            Lua.setfield (-2) "name"
                            Lua.pushnumber (Lua.Number (realToFrac v))
                            Lua.setfield (-2) "level"
                            Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | unit.addXP(uid, name, amount) — apply XP to a stat OR skill via
--   @newValue = value + amount / max (value^2, 1e-4)@. The lookup
--   tries skills first (typical case), then stats — whichever the
--   unit has under that name gets nudged. There's no separate XP
--   accumulator. Returns the new value, or nil if the unit doesn't
--   have a stat or skill by that name.
--
--   Phase F: this used to be addSkillXP and required the name to be
--   in skills. Now stats can grow this way too — a unit performing
--   manual labour can @unit.addXP(uid, \"strength\", 0.01)@ to slowly
--   build their strength stat over time.
unitAddXPFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddXPFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    amtArg  ← Lua.tonumber 3
    case (idArg, nameArg, amtArg) of
        (Just n, Just nameBS, Just (Lua.Number amt)) → do
            let uid    = UnitId (fromIntegral n)
                name   = TE.decodeUtf8 nameBS
                amount = realToFrac amt
            mVal ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, Nothing)
                        Just inst →
                            -- Try skills first (typical case), then
                            -- stats. Whichever map has the name owns
                            -- the value and gets updated.
                            case HM.lookup name (uiSkills inst) of
                                Just lvl →
                                    let lvl'  = applySkillXP lvl amount
                                        inst' = inst { uiSkills =
                                            HM.insert name lvl' (uiSkills inst) }
                                    in (um { umInstances = HM.insert uid inst'
                                                             (umInstances um) }
                                       , Just lvl')
                                Nothing → case HM.lookup name (uiStats inst) of
                                    Just v →
                                        let v'    = applySkillXP v amount
                                            inst' = inst { uiStats =
                                                HM.insert name v' (uiStats inst) }
                                        in (um { umInstances = HM.insert uid inst'
                                                                 (umInstances um) }
                                           , Just v')
                                    Nothing → (um, Nothing)
            case mVal of
                Just v → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getAllSkills(uid) — returns a Lua table keyed by skill name
--   with @{ level }@ subtables. Phase F: @level@ is the EFFECTIVE
--   value (base + active modifiers, clamped) — matches unit.getSkill.
--   nil if the unit is missing.
unitGetAllSkillsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllSkillsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mEntries ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                now ← readIORef (gameTimeRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    def  ← HM.lookup (uiDefName inst) (umDefs um)
                    let tmpls = udSkillTemplates def
                        mods  = uiModifiers inst
                    pure
                        [ (name, eff)
                        | (name, _) ← HM.toList tmpls
                        , let base = HM.lookupDefault 0 name (uiSkills inst)
                              eff  = effectiveStat now base
                                       (HM.lookupDefault [] name mods)
                        ]
            case mEntries of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just entries → do
                    Lua.newtable
                    forM_ entries $ \(name, lvl) → do
                        Lua.newtable
                        Lua.pushnumber (Lua.Number (realToFrac lvl))
                        Lua.setfield (-2) "level"
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.insert (-2)
                        Lua.rawset (-3)
                    return 1

-- | unit.getJumpReach(uid) → { dist = maxTiles, height = maxMetres } or nil.
--   The unit's leap envelope: max horizontal distance (tiles) and max
--   vertical strike reach (metres), from jumping skill + agility + strength,
--   penalised by body-fat. The AI reads these to decide whether to lunge and
--   to compute the strike-reach it passes to combat.attack.
unitGetJumpReachFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetJumpReachFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mReach ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let jmp = HM.lookupDefault 0.0 "jumping"  (uiSkills inst)
                            agi = HM.lookupDefault 1.0 "agility"   (uiStats inst)
                            str = HM.lookupDefault 1.0 "strength"  (uiStats inst)
                            bm  = HM.lookupDefault 1.0 "body_mass" (uiStats inst)
                            fm  = HM.lookupDefault 0.0 "fat_mass"  (uiStats inst)
                            ff  = if bm > 0 then fm / bm else 0
                        in return (Just ( jumpMaxTiles jmp agi str ff
                                        , maxJumpHeight jmp agi str ff ))
            case mReach of
                Just (d, h) → do
                    Lua.newtable
                    Lua.pushnumber (Lua.Number (realToFrac d))
                    Lua.setfield (-2) "dist"
                    Lua.pushnumber (Lua.Number (realToFrac h))
                    Lua.setfield (-2) "height"
                    return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.lungeImpactSpeed(uid, distTiles) → m/s. How fast the unit's body
--   is moving at the end of a leap of `distTiles` tiles (0 = an in-place
--   vertical pounce). The AI passes this to combat.attack so the lunge's
--   full-body momentum folds into the strike. 0 / nil unit → 0.
unitLungeImpactSpeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitLungeImpactSpeedFn env = do
    idArg ← Lua.tointeger 1
    dArg  ← Lua.tonumber 2
    case idArg of
        Nothing → Lua.pushnumber (Lua.Number 0) >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                d   = case dArg of
                          Just (Lua.Number v) → realToFrac v
                          _                   → 0.0
            mh ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let jmp = HM.lookupDefault 0.0 "jumping"  (uiSkills inst)
                            agi = HM.lookupDefault 1.0 "agility"   (uiStats inst)
                            str = HM.lookupDefault 1.0 "strength"  (uiStats inst)
                            bm  = HM.lookupDefault 1.0 "body_mass" (uiStats inst)
                            fm  = HM.lookupDefault 0.0 "fat_mass"  (uiStats inst)
                            ff  = if bm > 0 then fm / bm else 0
                        in return (Just (maxJumpHeight jmp agi str ff))
            case mh of
                Just h  → Lua.pushnumber (Lua.Number (realToFrac (lungeImpactSpeed d h)))
                                >> return 1
                Nothing → Lua.pushnumber (Lua.Number 0) >> return 1

-- | unit.getActivity(uid) — returns the unit's current sim-thread
--   activity as a string: "idle", "walking", or "collapsed". nil if
--   the unit doesn't exist. Reads `uiActivity`, which is mirrored
--   from `usState` by Unit.Thread.publishToRender every tick.
unitGetActivityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetActivityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAct ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiActivity <$> HM.lookup uid (umInstances um))
            case mAct of
                Just label → do
                    Lua.pushstring (TE.encodeUtf8 label)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.getCurrentAnim(uid) — the unit's resolved animation name
--   (uiCurrentAnim) as a string, "" for a T-pose. nil if the unit
--   doesn't exist. Lets scripts classify exertion that isn't a distinct
--   UnitActivity — mining (pickaxe/shovel work anims) and combat
--   (attack_/combat_ overrides) — for the metabolic burn / hydration drain.
unitGetCurrentAnimFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetCurrentAnimFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAnim ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiCurrentAnim <$> HM.lookup uid (umInstances um))
            case mAnim of
                Just anim → do
                    Lua.pushstring (TE.encodeUtf8 anim)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.getAllIds() — return a Lua array of every live unit's
--   integer id. Useful for per-tick iteration in scripts that don't
--   want to parse the human-readable string from unit.list.
-- | Instances of the ACTIVE world only — the world-scoping boundary for
--   listing / selection so a unit in another world never leaks into the
--   current one (#78). Empty when no world is active.
activeUnits ∷ EngineEnv → IO (HM.HashMap UnitId UnitInstance)
activeUnits env = do
    um ← readIORef (unitManagerRef env)
    mActive ← activeWorldPage env
    pure $ case mActive of
        Just (pid, _) → unitsOnPage pid (umInstances um)
        Nothing       → HM.empty

unitGetAllIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllIdsFn env = do
    ids ← Lua.liftIO $ HM.keys <$> activeUnits env
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] ids) $ \(i, uid) → do
        Lua.pushinteger (fromIntegral (unUnitId uid))
        Lua.rawseti (-2) (fromIntegral i)
    return 1

unitListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListFn env = do
    result ← Lua.liftIO $ do
        -- Active world page only — consistent with unit.getAllIds, so a
        -- unit on another world page never leaks into this listing (#377).
        entries ← HM.toList <$> activeUnits env
        if null entries
        then return "No units spawned"
        else return $ T.unpack $ T.intercalate "\n" $
            map (\(uid, inst) →
                "id=" <> T.pack (show (unUnitId uid))
                <> " " <> uiDefName inst
                <> " (" <> T.pack (show (uiGridX inst))
                <> ", " <> T.pack (show (uiGridY inst))
                <> ", " <> T.pack (show (uiGridZ inst)) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1

-- | unit.listDefs() — Lua array of available unit definition names.
--   These are the keys loadable into `unit.spawn(name, ...)`.
unitListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListDefsFn env = do
    names ← Lua.liftIO $ do
        um ← readIORef (unitManagerRef env)
        return $ HM.keys (umDefs um)
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, name) → do
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.rawseti (-2) i
    return 1

-- | unit.listAnimations(uid) — Lua array of animation names declared
--   for the unit's def. Used by the debug anim panel to enumerate
--   playable animations for the selected unit. Returns nil if the
--   unit or its def can't be found.
unitListAnimationsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListAnimationsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mNames ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    def  ← HM.lookup (uiDefName inst) (umDefs um)
                    pure (HM.keys (udAnimations def))
            case mNames of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just names → do
                    Lua.newtable
                    forM_ (zip [1..] names) $ \(i, name) → do
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.rawseti (-2) i
                    return 1

-- | unit.getInfo(id) — returns a Lua table with the unit's render-visible
--   attributes, or nil if the unit doesn't exist. Used by the info panel.
unitGetInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInfoFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPair ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                uts ← readIORef (utsRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    let mDef = HM.lookup (uiDefName inst) (umDefs um)
                        -- Current locomotion speed (tiles/s) for the
                        -- stamina tick: the active move target's speed
                        -- while the unit is actually walking/running,
                        -- else 0 (idle / transitioning / no target).
                        moveSpeed = case HM.lookup uid (utsSimStates uts) of
                            Just ss | isLocomoting (usState ss) →
                                maybe 0 mtSpeed (usTarget ss)
                            _ → 0
                        -- True while the unit is in a fall KNOCKDOWN (a
                        -- self-timed getup is pending). Lets the survival
                        -- revive logic leave knockdowns to the movement
                        -- tick, and the status panel explain why a unit is
                        -- down ("Knocked down" vs an exhaustion collapse).
                        knockedDown = case HM.lookup uid (utsSimStates uts) of
                            Just ss → maybe False (const True) (usGetUpAt ss)
                            _       → False
                    pure (inst, mDef, moveSpeed, knockedDown)
            case mPair of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (inst, mDef, moveSpeed, knockedDown) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (uiDefName inst))
                    Lua.setfield (-2) "defName"
                    -- Persistent personal name; "" for unnamed units (#264).
                    Lua.pushstring (TE.encodeUtf8 (uiName inst))
                    Lua.setfield (-2) "name"
                    -- Species label: the def's display_name, else a
                    -- prettified def name ("bear_brown" → "Bear Brown").
                    Lua.pushstring (TE.encodeUtf8
                        (fromMaybe (prettifyDefName (uiDefName inst))
                                   (mDef >>= udDisplayName)))
                    Lua.setfield (-2) "displayName"
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.setfield (-2) "gridX"
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.setfield (-2) "gridY"
                    Lua.pushinteger (fromIntegral (uiGridZ inst))
                    Lua.setfield (-2) "gridZ"
                    -- Continuous vertical position. Equal to gridZ
                    -- except during climbs, where it lerps smoothly
                    -- from start-z to top-z.
                    Lua.pushnumber (Lua.Number (realToFrac (uiRealZ inst)))
                    Lua.setfield (-2) "realZ"
                    Lua.pushstring (TE.encodeUtf8 (dirToText (uiFacing inst)))
                    Lua.setfield (-2) "facing"
                    Lua.pushnumber (Lua.Number (realToFrac (uiBaseWidth inst)))
                    Lua.setfield (-2) "baseWidth"
                    Lua.pushstring (TE.encodeUtf8 (uiCurrentAnim inst))
                    Lua.setfield (-2) "currentAnim"
                    Lua.pushnumber (Lua.Number (realToFrac (uiAnimStart inst)))
                    Lua.setfield (-2) "animStart"
                    Lua.pushnumber (Lua.Number (realToFrac moveSpeed))
                    Lua.setfield (-2) "moveSpeed"
                    Lua.pushboolean knockedDown
                    Lua.setfield (-2) "knockedDown"
                    -- equipmentClass is per-def, not per-instance. Only
                    -- present in the table when the def declares one.
                    case mDef >>= udEquipmentClass of
                        Just cls → do
                            Lua.pushstring (TE.encodeUtf8 cls)
                            Lua.setfield (-2) "equipmentClass"
                        Nothing → pure ()
                    return 1

-- | True for activities where the unit is translating across the ground
--   (so its move speed feeds stamina drain). Transitions / idle / drink
--   etc. are stationary.
isLocomoting ∷ UnitActivity → Bool
isLocomoting Walking = True
isLocomoting Running = True
isLocomoting _       = False

dirToText ∷ Direction → Text
dirToText DirS  = "S"
dirToText DirSW = "SW"
dirToText DirW  = "W"
dirToText DirNW = "NW"
dirToText DirN  = "N"
dirToText DirNE = "NE"
dirToText DirE  = "E"
dirToText DirSE = "SE"

-- * Selection

-- | unit.select(id) — replace the selection with a single unit.
--   Returns true if the unit exists, false if not.
unitSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $ Sel.selectUnit env uid
            Lua.pushboolean ok
            return 1

-- | unit.deselectAll() — empty the selection. Always returns true.
unitDeselectAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDeselectAllFn env = do
    Lua.liftIO $ Sel.clearSelection env
    Lua.pushboolean True
    return 1

-- | unit.getSelected() — returns a Lua array of integer unit IDs.
--   Filtered to only live units.
unitGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetSelectedFn env = do
    selected ← Lua.liftIO $ Sel.getSelected env
    let ids = HS.toList selected
    Lua.newtable
    forM_ (zip [1..] ids) $ \(i, uid) → do
        Lua.pushinteger (fromIntegral (unUnitId uid))
        Lua.rawseti (-2) i
    return 1

-- | unit.isSelected(id) — bool.
unitIsSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitIsSelectedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            sel ← Lua.liftIO $ Sel.isSelected env uid
            Lua.pushboolean sel
            return 1

-- | unit.hitTestAt(screenX, screenY) — returns the unit ID under the
--   given framebuffer-pixel coordinates, or nil if no unit is hit.
--
--   Lua side passes the raw GLFW mouse position (window-space pixels,
--   pre-DPI-scaling). `Unit.HitTest.hitTestUnitAt` does the projection
--   and per-unit AABB test against the sprite quad.
unitHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mUid ← Lua.liftIO $ HitTest.hitTestUnitAt env
                                  (realToFrac x) (realToFrac y)
            case mUid of
                Just uid → do
                    Lua.pushinteger (fromIntegral (unUnitId uid))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.hitTestInRect(x1, y1, x2, y2) — returns a Lua array of unit
--   IDs whose sprite-quad center falls inside the screen rect (window
--   pixels). Used by drag-box selection.
unitHitTestInRectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHitTestInRectFn env = do
    x1A ← Lua.tonumber 1
    y1A ← Lua.tonumber 2
    x2A ← Lua.tonumber 3
    y2A ← Lua.tonumber 4
    case (x1A, y1A, x2A, y2A) of
        (Just (Lua.Number a), Just (Lua.Number b),
         Just (Lua.Number c), Just (Lua.Number d)) → do
            ids ← Lua.liftIO $ HitTest.hitTestUnitsInRect env
                                  (realToFrac a) (realToFrac b)
                                  (realToFrac c) (realToFrac d)
            Lua.newtable
            forM_ (zip [1..] ids) $ \(i, uid) → do
                Lua.pushinteger (fromIntegral (unUnitId uid))
                Lua.rawseti (-2) i
            return 1
        _ → do
            Lua.newtable
            return 1

-- | unit.setSelection(idTable) — replace the selection with the given
--   array of unit IDs. IDs not corresponding to live units are filtered
--   out by the underlying setSelection.
unitSetSelectionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetSelectionFn env = do
    n ← Lua.rawlen 1   -- 0 if arg 1 isn't a table; loop body skips
    let go i acc
            | i > fromIntegral n = return acc
            | otherwise = do
                _ ← Lua.rawgeti 1 i
                m ← Lua.tointeger (-1)
                Lua.pop 1
                case m of
                    Just k  → go (i + 1) (UnitId (fromIntegral k) : acc)
                    Nothing → go (i + 1) acc
    ids ← go 1 []
    Lua.liftIO $ Sel.setSelection env (HS.fromList ids)
    Lua.pushboolean True
    return 1

-- | unit.setAnim(id, name) — sets the raw animation name on a unit and
--   resets its start time to now. Empty string clears the animation back
--   to T-pose. No state-name resolution; that's Phase 3's `setUnitAnim`.
--   Returns true on success, false if the unit doesn't exist.
unitSetAnimFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetAnimFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            ok ← Lua.liftIO $ do
                now ← readIORef (gameTimeRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiCurrentAnim = name
                                             , uiAnimStart   = now
                                             }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.setAnimOverride(uid, name) — Lua-driven animation that wins
--   over the engine's state-driven anim resolution. Use this for combat
--   swings (one-shot attack anims), posture animations (sit/lie/sleep
--   for bears), or any visual that doesn't map to a sim (pose, activity)
--   pair. Survives publishToRender each tick. Pass an empty string OR
--   call clearAnimOverride to release.
unitSetAnimOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetAnimOverrideFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            ok ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiAnimOverride = name }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.clearAnimOverride(uid) — releases the override so the engine's
--   state-driven anim resolution resumes on the next sim tick.
unitClearAnimOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitClearAnimOverrideFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiAnimOverride = "" }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.setFacing(uid, dirStr) — force the unit's facing direction.
--   `dirStr` is one of "S", "SW", "W", "NW", "N", "NE", "E", "SE"
--   (case-insensitive; long forms "south", "south-east" also accepted).
--   Used by the debug anim panel to cycle a unit through all 8
--   directions while previewing an animation. Returns true on
--   success, false on unknown direction or missing unit.
unitSetFacingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetFacingFn env = do
    idArg  ← Lua.tointeger 1
    dirArg ← Lua.tostring 2
    case (idArg, dirArg) of
        (Just n, Just dirBS) → do
            let uid    = UnitId (fromIntegral n)
                dirTxt = T.toUpper (TE.decodeUtf8 dirBS)
            case parseDir dirTxt of
                Nothing → do
                    Lua.pushboolean False
                    return 1
                Just dir → do
                    ok ← Lua.liftIO $ atomicModifyIORef'
                                        (unitManagerRef env) $ \um →
                        case HM.lookup uid (umInstances um) of
                            Nothing → (um, False)
                            Just inst →
                                let inst' = inst { uiFacing = dir }
                                in (um { umInstances = HM.insert uid inst'
                                                         (umInstances um) }
                                   , True)
                    Lua.pushboolean ok
                    return 1
        _ → do
            Lua.pushboolean False
            return 1
  where
    parseDir t = case t of
        "S"           → Just DirS
        "SOUTH"       → Just DirS
        "SW"          → Just DirSW
        "SOUTH-WEST"  → Just DirSW
        "SOUTH_WEST"  → Just DirSW
        "W"           → Just DirW
        "WEST"        → Just DirW
        "NW"          → Just DirNW
        "NORTH-WEST"  → Just DirNW
        "NORTH_WEST"  → Just DirNW
        "N"           → Just DirN
        "NORTH"       → Just DirN
        "NE"          → Just DirNE
        "NORTH-EAST"  → Just DirNE
        "NORTH_EAST"  → Just DirNE
        "E"           → Just DirE
        "EAST"        → Just DirE
        "SE"          → Just DirSE
        "SOUTH-EAST"  → Just DirSE
        "SOUTH_EAST"  → Just DirSE
        _             → Nothing

-- | unit.setFrozen(uid, on) — toggle the debug freeze flag. While
--   frozen, the unit thread's `publishToRender` does NOT update the
--   unit's position / facing / anim / activity / pose from sim state
--   — Lua scripts hold full control of those via setAnim / setFacing
--   / setPos. AI keeps ticking but its commands have no visible effect
--   until the flag is cleared.
--
--   Used by the debug anim panel so previewed animations aren't
--   stomped between frames by the still-running AI / sim loop. Don't
--   leave units frozen in saved worlds — the flag is runtime-only
--   (defaults to False on load) but a frozen unit ignores its own AI.
unitSetFrozenFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetFrozenFn env = do
    idArg ← Lua.tointeger 1
    onArg ← Lua.toboolean 2
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                on  = onArg
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiFrozen = on }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1

-- | unit.setForceLoop(uid, on) — debug-only override that makes the
--   renderer loop the current animation even when its YAML
--   `loop: false`. Used by the anim panel so one-shot animations
--   (attacks, transitions, death) play continuously while previewed
--   inside a direction window. Clear when leaving preview to restore
--   normal one-shot behaviour.
unitSetForceLoopFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetForceLoopFn env = do
    idArg ← Lua.tointeger 1
    onArg ← Lua.toboolean 2
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                on  = onArg
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiForceLoop = on }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1

-- * Stats

-- | unit.getStat(id, name) — read the EFFECTIVE stat value (base +
--   active modifier deltas, clamped at 0). Lazy-rolls the base if the
--   unit type defines the stat but the value hasn't been rolled yet.
--   Returns nil if the unit doesn't exist or doesn't define this stat.
unitGetStatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetStatFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ getEffectiveStat env uid name
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getStatBase(id, name) — read the RAW rolled value with no
--   modifiers applied. Use this when you want the underlying base —
--   e.g. to compute how much a debuff is shifting the effective value.
--   Lazy-rolls if needed; returns nil if undefined or unit missing.
unitGetStatBaseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetStatBaseFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ getOrRollStat env uid name
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.setStat(id, name, value) — overwrite a stat. Clamps at >= 0.
--   Returns true if the unit exists, false otherwise. No definition
--   check: setting a stat the unit type doesn't declare is allowed
--   (the value becomes accessible via getStat from then on).
unitSetStatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetStatFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
                clamped = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiStats =
                                HM.insert name clamped (uiStats inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getAllStats(id) — return a Lua table with every stat declared
--   by the unit's type. Values are EFFECTIVE (base + active modifiers,
--   clamped). Lazy-rolls any that haven't been rolled. nil if the unit
--   doesn't exist.
unitGetAllStatsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllStatsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPairs ← Lua.liftIO $ effectiveAllStats env uid
            case mPairs of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just pairs → do
                    Lua.newtable
                    forM_ pairs $ \(name, v) → do
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.pushnumber (Lua.Number (realToFrac v))
                        Lua.rawset (-3)
                    return 1

-- | unit.addModifier(id, name, delta, source, durationSec, percent) —
--   add or replace a modifier on a stat. Same @source@ on
--   the same @name@ overwrites the previous entry; different sources
--   stack. @durationSec@ is optional (nil = permanent); when given it
--   is added to the current gameTimeRef value to produce smExpiry,
--   so modifier expiries survive save/load (gameTimeRef is restored
--   on load; POSIX wall-clock isn't). @percent@ is an optional
--   fractional multiplier contribution (0.5 = +50%, applied as
--   (base + Σdelta) × (1 + Σpercent)); nil/absent = 0 (additive only).
--   Returns true on success, false if the unit doesn't exist.
unitAddModifierFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddModifierFn env = do
    idArg     ← Lua.tointeger 1
    nameArg   ← Lua.tostring 2
    deltaArg  ← Lua.tonumber 3
    srcArg    ← Lua.tostring 4
    -- 5 may be nil (permanent) or a number (duration seconds).
    durMaybe  ← Lua.tonumber 5
    -- 6 may be nil (purely additive) or a fractional percent.
    pctMaybe  ← Lua.tonumber 6
    case (idArg, nameArg, deltaArg, srcArg) of
        (Just n, Just nameBS, Just (Lua.Number d), Just srcBS) → do
            let uid   = UnitId (fromIntegral n)
                name  = TE.decodeUtf8 nameBS
                src   = TE.decodeUtf8 srcBS
                delta = realToFrac d
                pct   = case pctMaybe of
                            Just (Lua.Number p) → realToFrac p
                            _                   → 0
            ok ← Lua.liftIO $ do
                expiry ← case durMaybe of
                    Just (Lua.Number dur) → do
                        now ← readIORef (gameTimeRef env)
                        pure (Just (now + realToFrac dur))
                    _ → pure Nothing
                let mod' = StatModifier
                        { smDelta  = delta
                        , smSource = src
                        , smExpiry = expiry
                        , smPercent = pct
                        }
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let existing = HM.lookupDefault []
                                              name (uiModifiers inst)
                                -- Drop any prior entry from the same source.
                                others = filter (\m → smSource m ≠ src)
                                                existing
                                newList = mod' : others
                                inst' = inst { uiModifiers =
                                    HM.insert name newList
                                        (uiModifiers inst) }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.removeModifier(id, source) — remove every modifier owned
--   by @source@ across all stats. Returns the count of removed entries
--   (0 if unit missing or no matches).
unitRemoveModifierFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRemoveModifierFn env = do
    idArg  ← Lua.tointeger 1
    srcArg ← Lua.tostring 2
    case (idArg, srcArg) of
        (Just n, Just srcBS) → do
            let uid = UnitId (fromIntegral n)
                src = TE.decodeUtf8 srcBS
            removed ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Int)
                        Just inst →
                            let mods = uiModifiers inst
                                -- Count before, then filter each list.
                                cnt = sum [ length (filter
                                              (\m → smSource m ≡ src) ms)
                                          | ms ← HM.elems mods ]
                                pruned = HM.map (filter
                                            (\m → smSource m ≠ src)) mods
                                -- Drop now-empty entries to keep the
                                -- map tidy.
                                pruned' = HM.filter (not . null) pruned
                                inst' = inst { uiModifiers = pruned' }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , cnt)
            Lua.pushinteger (fromIntegral removed)
            return 1
        _ → do
            Lua.pushinteger 0
            return 1

-- | unit.getModifiers(id, name) — list every modifier on the named
--   stat as a Lua array of @{delta, percent, source, expiry}@ tables. Expired
--   entries are NOT filtered — caller can compare expiry to os.time().
--   nil if the unit doesn't exist; empty array if no modifiers.
unitGetModifiersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetModifiersFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mMods ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing   → pure Nothing
                    Just inst → pure (Just
                        (HM.lookupDefault [] name (uiModifiers inst)))
            case mMods of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mods → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] mods) $ \(i, m) → do
                        Lua.newtable
                        Lua.pushnumber (Lua.Number (realToFrac (smDelta m)))
                        Lua.setfield (-2) "delta"
                        Lua.pushnumber (Lua.Number (realToFrac (smPercent m)))
                        Lua.setfield (-2) "percent"
                        Lua.pushstring (TE.encodeUtf8 (smSource m))
                        Lua.setfield (-2) "source"
                        case smExpiry m of
                            Just t  → do
                                Lua.pushnumber (Lua.Number (realToFrac t))
                                Lua.setfield (-2) "expiry"
                            Nothing → pure ()
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.clearModifiers(id) — drop every modifier on every name
--   for this unit. Returns the count of removed entries. Mainly for
--   tests / debug; production code should usually use
--   removeModifier per-source.
unitClearModifiersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitClearModifiersFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            cnt ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Int)
                        Just inst →
                            let c = sum (length <$>
                                         HM.elems (uiModifiers inst))
                                inst' = inst { uiModifiers = HM.empty }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , c)
            Lua.pushinteger (fromIntegral cnt)
            return 1

-- | Read a stat or skill's base value. Phase F: lookup is unified —
--   stats first, then skills as a fallback. Lazy-rolls a stat
--   template if the value isn't cached and eager_stats is false.
--   Returns Nothing if undefined under either category.
getOrRollStat ∷ EngineEnv → UnitId → Text → IO (Maybe Float)
getOrRollStat env uid name = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return Nothing
        Just inst →
            case HM.lookup name (uiStats inst) of
                Just v  → return (Just v)
                Nothing → case HM.lookup name (uiSkills inst) of
                    -- Skills are always eager-rolled; if it's in
                    -- uiSkills the value is final.
                    Just v  → return (Just v)
                    Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
                        Nothing  → return Nothing
                        Just def → case HM.lookup name (udStatTemplates def) of
                            Nothing     → return Nothing
                            Just (b, r) → do
                                v ← atomicModifyIORef' (statRNGRef env) $ \g0 →
                                    let (val, g') = rollStat b r g0
                                    in (g', val)
                                -- Cache the rolled value. If the unit was
                                -- destroyed mid-roll, the lookup inside the
                                -- atomic modify finds nothing and we silently
                                -- drop — never resurrects a zombie unit.
                                atomicModifyIORef' (unitManagerRef env) $ \um' →
                                    case HM.lookup uid (umInstances um') of
                                        Nothing → (um', ())
                                        Just i  →
                                            let i' = i { uiStats =
                                                    HM.insert name v (uiStats i) }
                                            in (um' { umInstances =
                                                    HM.insert uid i'
                                                        (umInstances um') }, ())
                                return (Just v)

-- | Roll every stat declared by the unit's def that hasn't been rolled
--   yet, then return the full set as (name, value) pairs. Returns
--   Nothing if the unit doesn't exist.
rollAllDefinedStats ∷ EngineEnv → UnitId → IO (Maybe [(Text, Float)])
rollAllDefinedStats env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return Nothing
        Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
            Nothing → return (Just (HM.toList (uiStats inst)))
            Just def → do
                let templates = HM.toList (udStatTemplates def)
                -- Roll any missing stats (one getOrRollStat each).
                mapM_ (\(n, _) → getOrRollStat env uid n) templates
                -- Re-read for final values (including any pre-existing).
                um' ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um') of
                    Nothing    → return (Just [])
                    Just inst' → return (Just (HM.toList (uiStats inst')))

-- | Read a stat through the full Phase-C pipeline: lazy-roll the base
--   if needed, then apply this unit's active modifiers at the current
--   game time (gameTimeRef). Returns Nothing if undefined.
getEffectiveStat ∷ EngineEnv → UnitId → Text → IO (Maybe Float)
getEffectiveStat env uid name = do
    mBase ← getOrRollStat env uid name
    case mBase of
        Nothing   → pure Nothing
        Just base → do
            now ← readIORef (gameTimeRef env)
            um ← readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing   → pure (Just base)   -- destroyed mid-call
                Just inst →
                    let mods = HM.lookupDefault [] name
                                  (uiModifiers inst)
                    in pure (Just (effectiveStat now base mods))

-- | Like rollAllDefinedStats, but returns effective values (post-modifier).
--   Reads now once and applies it to every modifier list for consistency.
effectiveAllStats ∷ EngineEnv → UnitId → IO (Maybe [(Text, Float)])
effectiveAllStats env uid = do
    mBases ← rollAllDefinedStats env uid
    case mBases of
        Nothing    → pure Nothing
        Just bases → do
            now ← readIORef (gameTimeRef env)
            um ← readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing   → pure (Just bases)  -- destroyed mid-call
                Just inst →
                    let mods = uiModifiers inst
                        eff (n, b) =
                            (n, effectiveStat now b
                                  (HM.lookupDefault [] n mods))
                    in pure (Just (map eff bases))

-- * Helpers

-- | Accept short uppercase ("S","SW") or long lowercase ("south","south-east").
parseDirKey ∷ Text → Maybe Direction
parseDirKey t = case T.toLower t of
    "s"          → Just DirS
    "sw"         → Just DirSW
    "w"          → Just DirW
    "nw"         → Just DirNW
    "n"          → Just DirN
    "ne"         → Just DirNE
    "e"          → Just DirE
    "se"         → Just DirSE
    "south"      → Just DirS
    "south-west" → Just DirSW
    "west"       → Just DirW
    "north-west" → Just DirNW
    "north"      → Just DirN
    "north-east" → Just DirNE
    "east"       → Just DirE
    "south-east" → Just DirSE
    _            → Nothing

-- | unit.getItemContents(uid, defName[, instanceId]) → array of { defName,
--   displayName, count, fill, condition, weight, ... }, GROUPED by item type
--   (10 bandages → one entry with count=10), for the targeted item-container
--   (the exact instance when an id is given, else the FIRST inventory item
--   matching `defName`). `weight` is each item's TRUE per-instance mass
--   (empty case + fill + nested contents), so a filled bottle reports its
--   real weight, not the empty-bottle def weight. Empty table if it holds
--   nothing; nil if the unit or that item isn't found.
unitGetItemContentsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetItemContentsFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    -- Optional 3rd arg: target a specific container instance by id (#67),
    -- so two same-def kits don't show each other's contents. Absent/0 →
    -- first inventory item matching defName (AI / tooltip callers).
    instArg ← Lua.tointeger 3
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid    = UnitId (fromIntegral n)
                want   = TE.decodeUtf8 nameBS
                wantId = maybe 0 fromIntegral instArg
            mRes ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case [ i | i ← uiInventory inst, itemMatches wantId want i ] of
                            (kit : _) → Just (iiContents kit, itemMgr)
                            []        → Nothing
            case mRes of
                Nothing → Lua.pushnil >> return 1
                Just (contents, itemMgr) → do
                    -- Group identical contents by defName. A kit holds at
                    -- most a handful of types, so a flat defName grouping
                    -- (rather than the cargo panel's defName+quality+cond
                    -- key) reads fine — tools are count 1, consumables have
                    -- no condition spread.
                    -- Carry each item's true per-instance mass (empty case +
                    -- fill + nested contents, via itemTotalWeight) into the
                    -- group, NOT the static def weight: a filled antiseptic /
                    -- antibiotics bottle weighs its contents, so the Contents
                    -- panel must not show the empty-bottle weight. Items in a
                    -- defName group are identical (consumables are fill 0,
                    -- bottles are count 1), so keeping the representative's
                    -- per-item weight is exact and the panel's weight×count
                    -- gives the right total.
                    let grouped = HM.toList $ HM.fromListWith
                            (\(c1, f, cond, w) (c2, _, _, _) → (c1 + c2, f, cond, w))
                            [ ( iiDefName i
                              , (1 ∷ Int, iiCurrentFill i, iiCondition i
                                , itemTotalWeight itemMgr i) )
                            | i ← contents ]
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] grouped) $
                      \(idx, (dname, (cnt, fill, cond, wt))) → do
                        let mDef = lookupItemDef dname itemMgr
                            disp = maybe dname idDisplayName mDef
                            cat  = maybe "Misc" idCategory mDef
                            tex  = maybe (-1) (\d → let TextureHandle t =
                                                          idTexture d in t) mDef
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 dname)
                        Lua.setfield (-2) "defName"
                        Lua.pushstring (TE.encodeUtf8 disp)
                        Lua.setfield (-2) "displayName"
                        Lua.pushstring (TE.encodeUtf8 cat)
                        Lua.setfield (-2) "category"
                        Lua.pushinteger (fromIntegral cnt)
                        Lua.setfield (-2) "count"
                        Lua.pushnumber (Lua.Number (realToFrac wt))
                        Lua.setfield (-2) "weight"
                        Lua.pushinteger (fromIntegral tex)
                        Lua.setfield (-2) "iconTex"
                        Lua.pushnumber (Lua.Number (realToFrac fill))
                        Lua.setfield (-2) "fill"
                        Lua.pushnumber (Lua.Number (realToFrac cond))
                        Lua.setfield (-2) "condition"
                        Lua.rawseti (-2) (fromIntegral idx)
                    return 1
        _ → Lua.pushnil >> return 1

-- ----- Treat action (C MVP) -----

-- | Outcome of a treat attempt. `trSeep` is the wound's new
--   `woundBandage` (fraction of bleed that still seeps): 0 = perfectly
--   sealed, 0.05 = a competent dressing, 1.0 = untreated (on failure).
data TreatResult = TreatResult
    { trOk        ∷ !Bool
    , trSeep      ∷ !Float
    , trBandages  ∷ !Int     -- bandages consumed (incl. wasted on failed tries)
    , trAttempts  ∷ !Int
    , trPart      ∷ !Text
    , trKind      ∷ !Text
    , trMessage   ∷ !Text
    , trMethod    ∷ !Text     -- "" / "bandage" / "tourniquet"
    }

treatFail ∷ Text → TreatResult
treatFail msg = TreatResult False 1.0 0 0 "" "" msg ""

bandageItemName ∷ Text
bandageItemName = "bandage"

-- | The mechanic behind `unit.treatBleeding`. A medic who KNOWS
--   bleed-control dresses the patient's worst-bleeding wound using
--   bandages drawn from a first-aid kit (carried by `owner`, default
--   the medic). Capability = (bleed_control / 100) × intelligence ×
--   tool-condition factor:
--     * each attempt succeeds with probability ≈ 0.15 + capability
--       (experts essentially never miss; the poorly trained may need
--       several tries, wasting a bandage each fail);
--     * on success the dressing's seep fraction is 0.6·(1−cap)² — an
--       expert with high intelligence reaches 0 (bleed stopped dead),
--       a competent hand ~5%, a poor one ~50%.
--   Worn tweezers/scissors in the kit drag the capability down. Tools
--   are reusable (never consumed); only bandages are spent.
treatBleedingIO ∷ EngineEnv → UnitId → UnitId → Maybe UnitId → IO TreatResult
treatBleedingIO env medic patient mOwner = do
    um0 ← readIORef (unitManagerRef env)
    let owner = fromMaybe medic mOwner
    case ( HM.lookup medic   (umInstances um0)
         , HM.lookup patient (umInstances um0)
         , HM.lookup owner   (umInstances um0) ) of
      (Just med, Just pat, Just own) →
        case HM.lookup "bleed_control" (uiKnowledge med) of
          Nothing → pure (treatFail "medic lacks bleed-control knowledge")
          Just level → do
            -- Worst bleeding wound — needed by BOTH the kit-dressing and
            -- the no-supplies tourniquet path, so it's found up front.
            let parts = case HM.lookup (uiDefName pat) (umDefs um0) of
                    Just d  → HM.fromList [(bpId bp, bp) | bp ← udBodyParts d]
                    Nothing → HM.empty
                -- Mirror Combat.Wounds.bleedRateFor: a wound that has
                -- already self-clotted (or been dressed) isn't really
                -- bleeding, so the (1 − clot) and dressing factors must be
                -- in here too. Without them the medic could rank a high-
                -- severity but clotted wound above the one actually seeping
                -- and waste a bandage on it. Severity is the EFFECTIVE
                -- severity (healing eases it, necrosis floors it) — the
                -- same source of truth bleedRateFor squares — so a mostly-
                -- healed wound no longer outranks a fresh seeping one.
                scoreOf w = let effSev = woundEffSeverity w
                            in (effSev * effSev)
                          * kindBleedFactor (woundKind w)
                          * maybe 1.0 bpBleedFactor
                                (HM.lookup (woundPart w) parts)
                          * woundBandage w
                          * (1 - woundClot w)
                bleeders = [ (w, scoreOf w) | w ← uiWounds pat
                           , woundBandage w > 0.02, scoreOf w > 1.0e-4 ]
            case bleeders of
              [] → pure (treatFail "no bleeding wound to treat")
              _  → do
                let (worst, _) = L.foldl1'
                        (\a@(_, sa) b@(_, sb) → if sb > sa then b else a)
                        bleeders
                    targetKey = (woundPart worst, woundKind worst, woundAt worst)
                mIntel ← getEffectiveStat env medic "intelligence"
                let intel    = fromMaybe 1.0 mIntel
                    nLevel   = max 0 (min 1 (level / 100))
                    baseComp = nLevel * intel   -- skill × intelligence (no tools)
                    kits = [ it | it ← uiInventory own
                                , any ((≡ bandageItemName) . iiDefName)
                                      (iiContents it) ]
                case kits of
                  (kit:_) → do
                    -- PROPER DRESSING from the kit (the C-MVP attempt cycle).
                    let bandageCount = length
                            [ () | c ← iiContents kit, iiDefName c ≡ bandageItemName ]
                        toolConds = [ iiCondition c | c ← iiContents kit
                                    , iiDefName c ≡ "tweezers"
                                      ∨ iiDefName c ≡ "scissors" ]
                        toolCond01 = if null toolConds
                            then 0.5
                            else (sum toolConds / fromIntegral (length toolConds)) / 100
                        toolFactor = 0.7 + 0.3 * toolCond01
                        competence = max 0 (min 1.1 (baseComp * toolFactor))
                        pSucc      = max 0.05 (min 0.99 (0.15 + competence))
                        capClamp   = min 1 competence
                        seepBase   = 0.6 * (1 - capClamp) * (1 - capClamp)
                        maxAttempts = 8 ∷ Int
                    localGen ← atomicModifyIORef' (statRNGRef env) Random.splitGen
                    let go gen attemptsLeft used
                          | used >= bandageCount = (False, used, gen)
                          | attemptsLeft ≤ 0     = (False, used, gen)
                          | otherwise =
                              let (r, gen') = Random.randomR (0, 1) gen
                                                ∷ (Float, Random.StdGen)
                                  used' = used + 1
                              in if r < pSucc
                                   then (True,  used', gen')
                                   else go gen' (attemptsLeft - 1) used'
                        (success, consumed, gen2) = go localGen maxAttempts 0
                        (jr, _) = Random.randomR (0, 1) gen2
                                    ∷ (Float, Random.StdGen)
                        seep = if success
                                 then max 0 (min 0.6 (seepBase * (0.9 + 0.2 * jr)))
                                 else 1.0
                        treatXp = if consumed ≤ 0 then 0
                                  else if success then 2.0 else 1.0
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        let um1 = consumeBandages owner consumed um
                            um2 = if success
                                    then setWoundDressing patient targetKey
                                             seep "bandage" um1
                                    else um1
                            um3 = grantKnowledgeXP medic "bleed_control"
                                                   treatXp um2
                            -- PREVENTION: if the kit holds antiseptic, a
                            -- successful dressing also disinfects the wound —
                            -- it won't accumulate infection. Consumes a dose.
                            um4 = if success
                                     ∧ kitHasFill owner antisepticItemName
                                                  antisepticDose um3
                                    then setWoundClean patient targetKey True
                                             (consumeKitFill owner
                                                antisepticItemName
                                                antisepticDose um3)
                                    else um3
                        in (um4, ())
                    let msg = if success then "treated"
                                         else "failed — out of material"
                    pure (TreatResult success seep consumed
                            (max consumed 1) (woundPart worst)
                            (woundKind worst) msg "bandage")
                  [] → do
                    -- NO SUPPLIES → improvise a makeshift tourniquet. Crude
                    -- but better than nothing: it always goes on, consumes
                    -- no material, and stops the bleed only "somewhat" — a
                    -- poor seep (~0.4–0.58, a touch better with skill). Still
                    -- trains the medic a little.
                    let tqSeep = max 0.4 (min 0.58 (0.58 - 0.2 * min 1 baseComp))
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        let um1 = setWoundDressing patient targetKey
                                      tqSeep "tourniquet" um
                            um2 = grantKnowledgeXP medic "bleed_control" 1.0 um1
                        in (um2, ())
                    pure (TreatResult True tqSeep 0 1 (woundPart worst)
                            (woundKind worst) "makeshift tourniquet" "tourniquet")
      _ → pure (treatFail "medic, patient, or kit owner not found")

-- | Drop the first `n` bandage instances from the first inventory item
--   (kit) that holds any. Leaves tools / other contents untouched.
consumeBandages ∷ UnitId → Int → UnitManager → UnitManager
consumeBandages _     0 um = um
consumeBandages owner n um =
    case HM.lookup owner (umInstances um) of
        Nothing  → um
        Just own →
            let inv' = dropFromFirstKit (uiInventory own)
            in um { umInstances =
                      HM.insert owner (own { uiInventory = inv' })
                                (umInstances um) }
  where
    dropFromFirstKit [] = []
    dropFromFirstKit (it:rest)
        | any ((≡ bandageItemName) . iiDefName) (iiContents it) =
            it { iiContents = dropN n (iiContents it) } : rest
        | otherwise = it : dropFromFirstKit rest
    dropN 0 cs = cs
    dropN _ [] = []
    dropN k (c:cs)
        | iiDefName c ≡ bandageItemName = dropN (k - 1) cs
        | otherwise                     = c : dropN k cs

-- | Apply XP to one of a unit's KNOWN knowledges (same diminishing-
--   returns curve as skill XP: newLevel = level + xp / max(level², ε)).
--   No-op if the unit doesn't exist, doesn't know it, or xp ≤ 0 — a
--   knowledge is only trained once a unit actually has it.
grantKnowledgeXP ∷ UnitId → Text → Float → UnitManager → UnitManager
grantKnowledgeXP uid name xp um
    | xp ≤ 0 = um
    | otherwise = case HM.lookup uid (umInstances um) of
        Nothing → um
        Just inst → case HM.lookup name (uiKnowledge inst) of
            Nothing  → um
            Just lvl →
                let inst' = inst { uiKnowledge =
                        HM.insert name (applySkillXP lvl xp)
                                  (uiKnowledge inst) }
                in um { umInstances =
                          HM.insert uid inst' (umInstances um) }

-- | Dress the matching wound on the patient: set its seep (woundBandage)
--   and the dressing type ("bandage" / "tourniquet"). Matched by
--   part+kind+inflicted-time so a concurrent wound-list reshuffle can't
--   dress the wrong wound.
setWoundDressing
    ∷ UnitId → (Text, Text, Double) → Float → Text
    → UnitManager → UnitManager
setWoundDressing patient (pPart, pKind, pAt) seep dressing um =
    case HM.lookup patient (umInstances um) of
        Nothing  → um
        Just pat →
            let ws' = map (\w → if woundPart w ≡ pPart
                                   ∧ woundKind w ≡ pKind
                                   ∧ woundAt w ≡ pAt
                                  then w { woundBandage  = seep
                                         , woundDressing = dressing }
                                  else w)
                          (uiWounds pat)
            in um { umInstances =
                      HM.insert patient (pat { uiWounds = ws' })
                                (umInstances um) }

-- Medical-supply item def-names + per-use doses.
antisepticItemName, antibioticsItemName ∷ Text
antisepticItemName  = "antiseptic"
antibioticsItemName = "antibiotics"

antisepticDose ∷ Float
antisepticDose = 0.05   -- litres per wound disinfection (a 1 L bottle ≈ 20 uses)

antibioticsDose ∷ Float
antibioticsDose = 1.0   -- one pill per cure dose (a 60-pill bottle = 60 doses)

-- | True if the owner carries a kit holding `name` with at least `dose`
--   of fill remaining (antiseptic litres / antibiotic pills).
kitHasFill ∷ UnitId → Text → Float → UnitManager → Bool
kitHasFill owner name dose um = case HM.lookup owner (umInstances um) of
    Nothing  → False
    Just own → any (\it → any hasIt (iiContents it)) (uiInventory own)
  where hasIt c = iiDefName c ≡ name ∧ iiCurrentFill c ≥ dose

-- | Spend `dose` of fill from the first matching item in the first kit
--   (inventory item whose contents hold one with enough). No-op if none.
consumeKitFill ∷ UnitId → Text → Float → UnitManager → UnitManager
consumeKitFill owner name dose um = case HM.lookup owner (umInstances um) of
    Nothing  → um
    Just own →
        let inv' = goInv (uiInventory own)
        in um { umInstances =
                  HM.insert owner (own { uiInventory = inv' }) (umInstances um) }
  where
    enough c = iiDefName c ≡ name ∧ iiCurrentFill c ≥ dose
    goInv [] = []
    goInv (it:rest)
        | any enough (iiContents it) =
            it { iiContents = goContents (iiContents it) } : rest
        | otherwise = it : goInv rest
    goContents [] = []
    goContents (c:cs)
        | enough c  = c { iiCurrentFill = iiCurrentFill c - dose } : cs
        | otherwise = c : goContents cs

-- | Mark the matching wound clean/disinfected (antiseptic prevention) —
--   a clean wound never accumulates infection. Same part+kind+at match as
--   setWoundDressing so a concurrent reshuffle can't hit the wrong wound.
setWoundClean ∷ UnitId → (Text, Text, Double) → Bool → UnitManager → UnitManager
setWoundClean patient (pPart, pKind, pAt) cln um =
    case HM.lookup patient (umInstances um) of
        Nothing  → um
        Just pat →
            let ws' = map (\w → if woundPart w ≡ pPart ∧ woundKind w ≡ pKind
                                   ∧ woundAt w ≡ pAt
                                  then w { woundClean = cln } else w)
                          (uiWounds pat)
            in um { umInstances =
                      HM.insert patient (pat { uiWounds = ws' })
                                (umInstances um) }

-- | Nudge a unit's systemic immune response up by `boost` (clamped to 1).
--   Antibiotics "speed up the response ticker" through this on top of the
--   bacterial-specific knockdown.
bumpImmuneResponse ∷ UnitId → Float → UnitManager → UnitManager
bumpImmuneResponse uid boost um =
    case HM.lookup uid (umInstances um) of
        Nothing  → um
        Just inst →
            let r' = max 0 (min 1 (uiImmuneResponse inst + boost))
            in um { umInstances =
                      HM.insert uid (inst { uiImmuneResponse = r' })
                                (umInstances um) }

-- | Set the matching wound's infection level (antibiotics cure). Same
--   match key as the other wound mutators.
setWoundInfection ∷ UnitId → (Text, Text, Double) → Float → UnitManager → UnitManager
setWoundInfection patient (pPart, pKind, pAt) inf um =
    case HM.lookup patient (umInstances um) of
        Nothing  → um
        Just pat →
            let ws' = map (\w → if woundPart w ≡ pPart ∧ woundKind w ≡ pKind
                                   ∧ woundAt w ≡ pAt
                                  then w { woundInfection = inf } else w)
                          (uiWounds pat)
            in um { umInstances =
                      HM.insert patient (pat { uiWounds = ws' })
                                (umInstances um) }

-- | unit.frostbite(uid, part, necDelta) → new necrosis level (0..1) | nil.
--   Grow (or create) a "frostbite" wound on `part`, adding `necDelta` to its
--   woundNecrosis. The Lua thermo tick calls this for extremities when the unit
--   is cold + poorly perfused (cold-killed tissue = necrosis). The engine's
--   existing necrosis machinery then handles the consequences for free: at
--   necrosis 1.0 a non-vital extremity rots off (propagateSevering), the open
--   wound can get infected (the infection system), and it shows as rot in the
--   Status panel. Returns the frostbite wound's new necrosis level.
unitFrostbiteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitFrostbiteFn env = do
    idArg   ← Lua.tointeger 1
    partArg ← Lua.tostring 2
    deltaArg ← Lua.tonumber 3
    case (idArg, partArg, deltaArg) of
        (Just n, Just partBS, Just (Lua.Number d)) → do
            now ← Lua.liftIO $ readIORef (gameTimeRef env)
            let uid   = UnitId (fromIntegral n)
                part  = TE.decodeUtf8 partBS
                delta = max 0 (realToFrac d)
                isFb w = woundKind w ≡ "frostbite" ∧ woundPart w ≡ part
            result ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, Nothing)
                    Just inst →
                        let ws = uiWounds inst
                            grown = [ if isFb w
                                        then w { woundNecrosis =
                                                   min 1 (woundNecrosis w + delta) }
                                        else w | w ← ws ]
                            newW = Wound
                                { woundPart = part, woundKind = "frostbite"
                                , woundSeverity = 0.3, woundAt = now
                                , woundBandage = 1.0, woundClot = 0.0
                                , woundHeal = 0.0, woundDressing = ""
                                , woundInfection = 0.0, woundClean = False
                                , woundInfectionType = ""
                                , woundNecrosis = min 1 delta }
                            ws' = if any isFb ws then grown else newW : ws
                            inst' = inst { uiWounds = ws' }
                            lvl = maximum (min 1 delta
                                  : [ woundNecrosis w | w ← ws', isFb w ])
                        in ( um { umInstances =
                                    HM.insert uid inst' (umInstances um) }
                           , Just lvl )
            case result of
                Just lvl → Lua.pushnumber (Lua.Number (realToFrac lvl)) >> return 1
                Nothing  → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | The mechanic behind `unit.treatInfection` — the CURE half of the
--   medical loop (antiseptic prevention is folded into treatBleeding). A
--   medic who knows INFECTION-CONTROL administers antibiotics from the kit
--   to the patient's worst-infected wound, cutting its infection by an
--   amount scaled by capability (infection_control / 100 × intelligence),
--   and marking it clean so it won't re-infect. Consumes one antibiotic
--   pill per call.
--   Reuses TreatResult: trSeep carries the wound's NEW infection level,
--   trMethod = "antibiotics".
treatInfectionIO ∷ EngineEnv → UnitId → UnitId → Maybe UnitId → IO TreatResult
treatInfectionIO env medic patient mOwner = do
    um0 ← readIORef (unitManagerRef env)
    infMgr ← readIORef (infectionManagerRef env)
    let owner = fromMaybe medic mOwner
        -- A wound is antibiotic-curable if its infection is bacterial — i.e.
        -- the def lists "antibiotics" in curable_by. An untyped infection
        -- (woundInfectionType "") is treated as bacterial (the default).
        curableW w = case lookupInfection (woundInfectionType w) infMgr of
            Nothing  → True   -- untyped / unknown → assume bacterial
            Just inf → antibioticsItemName `elem` infCurableBy inf
        cureRateW w = maybe 1.0 infCureRate
                        (lookupInfection (woundInfectionType w) infMgr)
    case ( HM.lookup medic   (umInstances um0)
         , HM.lookup patient (umInstances um0) ) of
      (Just med, Just pat) →
        case HM.lookup "infection_control" (uiKnowledge med) of
          Nothing → pure (treatFail "medic lacks infection-control knowledge")
          Just level →
            case [ w | w ← uiWounds pat, woundInfection w > 0.05 ] of
              [] → pure (treatFail "no infected wound to treat")
              infected
                | null (filter curableW infected) →
                    pure (treatFail "infection not treatable with antibiotics")
                | not (kitHasFill owner antibioticsItemName antibioticsDose um0) →
                    pure (treatFail "no antibiotics in kit")
                | otherwise → do
                    let worst = L.foldl1'
                            (\a b → if woundInfection b > woundInfection a
                                    then b else a) (filter curableW infected)
                        targetKey = ( woundPart worst, woundKind worst
                                    , woundAt worst )
                    mIntel ← getEffectiveStat env medic "intelligence"
                    let intel     = fromMaybe 1.0 mIntel
                        nLevel    = max 0 (min 1 (level / 100))
                        cap       = max 0 (min 1.1 (nLevel * intel))
                        -- cure strength scales with capability AND the
                        -- infection's own cure_rate (some bugs resist more).
                        reduction = max 0.15 (min 0.85 (0.2 + 0.6 * cap))
                                    * cureRateW worst
                        newInf    = max 0 (woundInfection worst - reduction)
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        let um1 = consumeKitFill owner antibioticsItemName
                                                 antibioticsDose um
                            um2 = setWoundInfection patient targetKey newInf um1
                            um3 = setWoundClean patient targetKey True um2
                            um4 = grantKnowledgeXP medic "infection_control" 1.5 um3
                            -- Antibiotics also speed up the systemic immune
                            -- response (helps clear other bacterial foci).
                            um5 = bumpImmuneResponse patient
                                    (min 0.5 (0.3 * cap)) um4
                        in (um5, ())
                    pure (TreatResult True newInf 1 1 (woundPart worst)
                            (woundKind worst) "antibiotics administered"
                            "antibiotics")
      _ → pure (treatFail "medic or patient not found")

-- | unit.treatInfection(medicUid, patientUid [, kitOwnerUid]) →
--     { ok, infection, part, kind, message, method } | nil
unitTreatInfectionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTreatInfectionFn env = do
    medicArg ← Lua.tointeger 1
    patArg   ← Lua.tointeger 2
    ownerArg ← Lua.tointeger 3
    case (medicArg, patArg) of
        (Just m, Just p) → do
            let mOwner = (UnitId . fromIntegral) <$> ownerArg
            res ← Lua.liftIO $ treatInfectionIO env
                    (UnitId (fromIntegral m)) (UnitId (fromIntegral p)) mOwner
            Lua.newtable
            Lua.pushboolean (trOk res)
            Lua.setfield (-2) "ok"
            Lua.pushnumber (Lua.Number (realToFrac (trSeep res)))
            Lua.setfield (-2) "infection"
            Lua.pushstring (TE.encodeUtf8 (trPart res))
            Lua.setfield (-2) "part"
            Lua.pushstring (TE.encodeUtf8 (trKind res))
            Lua.setfield (-2) "kind"
            Lua.pushstring (TE.encodeUtf8 (trMessage res))
            Lua.setfield (-2) "message"
            Lua.pushstring (TE.encodeUtf8 (trMethod res))
            Lua.setfield (-2) "method"
            return 1
        _ → Lua.pushnil >> return 1

-- | unit.treatBleeding(medicUid, patientUid [, kitOwnerUid]) →
--     { ok, seep, bandagesUsed, attempts, part, kind, message } | nil
--
--   One full treatment attempt-cycle (the medic keeps trying until the
--   wound is dressed or the kit runs out of bandages). `kitOwnerUid`
--   defaults to the medic — pass the technomule's id to draw from its
--   kit while a different acolyte administers. nil only when the
--   id args are missing; all other failures come back as ok=false with
--   a message.
unitTreatBleedingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTreatBleedingFn env = do
    medicArg ← Lua.tointeger 1
    patArg   ← Lua.tointeger 2
    ownerArg ← Lua.tointeger 3
    case (medicArg, patArg) of
        (Just m, Just p) → do
            let mOwner = (UnitId . fromIntegral) <$> ownerArg
            res ← Lua.liftIO $ treatBleedingIO env
                    (UnitId (fromIntegral m)) (UnitId (fromIntegral p)) mOwner
            Lua.newtable
            Lua.pushboolean (trOk res)
            Lua.setfield (-2) "ok"
            Lua.pushnumber (Lua.Number (realToFrac (trSeep res)))
            Lua.setfield (-2) "seep"
            Lua.pushinteger (fromIntegral (trBandages res))
            Lua.setfield (-2) "bandagesUsed"
            Lua.pushinteger (fromIntegral (trAttempts res))
            Lua.setfield (-2) "attempts"
            Lua.pushstring (TE.encodeUtf8 (trPart res))
            Lua.setfield (-2) "part"
            Lua.pushstring (TE.encodeUtf8 (trKind res))
            Lua.setfield (-2) "kind"
            Lua.pushstring (TE.encodeUtf8 (trMessage res))
            Lua.setfield (-2) "message"
            Lua.pushstring (TE.encodeUtf8 (trMethod res))
            Lua.setfield (-2) "method"
            return 1
        _ → Lua.pushnil >> return 1

-- | unit.injure(uid, part, kind, severity [, bandage]) → bool
--   Stamp an arbitrary wound onto a unit — the Lua hook into the
--   injury system (debug, arena testing, and future gameplay events
--   like traps/hazards). `part` is a body-part id (e.g. "l_thigh",
--   "neck", "torso"), `kind` a wound kind ("slash"/"stab"/"blunt"/
--   "arterial"/"severed"/"internal"/"fracture"/"concussion"),
--   `severity` 0..1, optional `bandage` 0..1 (default 1 = untreated).
--   Prepended so it reads newest-first like a combat wound. Returns
--   false if the unit is missing or required args are absent.
unitInjureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitInjureFn env = do
    idArg   ← Lua.tointeger 1
    partArg ← Lua.tostring 2
    kindArg ← Lua.tostring 3
    sevArg  ← Lua.tonumber 4
    bandArg ← Lua.tonumber 5
    case (idArg, partArg, kindArg, sevArg) of
        (Just n, Just partBS, Just kindBS, Just (Lua.Number sev)) → do
            now ← Lua.liftIO $ readIORef (gameTimeRef env)
            let uid = UnitId (fromIntegral n)
                bandage = case bandArg of
                    Just (Lua.Number b) → max 0 (min 1 (realToFrac b))
                    _                   → 1.0
                w = Wound
                    { woundPart     = TE.decodeUtf8 partBS
                    , woundKind     = TE.decodeUtf8 kindBS
                    , woundSeverity = max 0 (min 1 (realToFrac sev))
                    , woundAt       = now
                    , woundBandage  = bandage
                    , woundClot     = 0.0
                    , woundHeal     = 0.0
                    , woundDressing = ""
                    , woundInfection = 0.0
                    , woundClean    = False
                    , woundInfectionType = ""
                    , woundNecrosis = 0.0 }
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing   → (um, False)
                    Just inst →
                        let inst' = inst { uiWounds = w : uiWounds inst }
                        in (um { umInstances =
                                   HM.insert uid inst' (umInstances um) }, True)
            -- A successful (non-combat) wound feeds the injury log.
            Lua.liftIO $ when ok $
                pushInjuryEvent (injuryEventsRef env) now (fromIntegral n)
                    "injure"
                    [ ("part",      TE.decodeUtf8 partBS)
                    , ("woundKind", TE.decodeUtf8 kindBS)
                    , ("severity",  T.pack (show (woundSeverity w))) ]
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.getInventory(uid) → array of item tables, or nil if the unit
--   doesn't exist. Each table has:
--     defName        — ItemDef key (e.g. "canteen_steel_2l")
--     displayName    — UI-facing name from YAML
--     weight         — empty kg
--     currentFill    — litres held (0 for non-containers)
--     capacity       — litres max (nil for non-containers)
--     holds          — fluid kind (nil for non-containers)
unitGetInventoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInventoryFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mInv ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing   → Nothing
                    Just inst → Just (uiInventory inst, itemMgr)
            case mInv of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (insts, itemMgr) → do
                    Lua.newtable
                    -- Use 1-based Lua indexing for a real array.
                    forM_ (zip [1 ..] insts) $ \(i, inst) → do
                        Lua.newtable
                        let name = iiDefName inst
                            mDef = lookupItemDef name itemMgr
                            baseName = case mDef of
                                Just d  → idDisplayName d
                                Nothing → name
                            -- Condition 0 ⇒ broken (Combat.Resolution
                            -- weapon wear); tag the name everywhere it shows.
                            broken = iiCondition inst ≤ 0
                            displayName = if broken
                                          then baseName <> " (broken)"
                                          else baseName
                            -- True carried mass of THIS instance — empty
                            -- case + fill + nested contents — so a stocked
                            -- kit / filled canteen shows its real weight and
                            -- the inventory footer matches getCarryingWeight,
                            -- and two kits whose contents diverged read as
                            -- different weights (#67A).
                            weight = itemTotalWeight itemMgr inst
                            mContainer = mDef >>= idContainer
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.setfield (-2) "defName"
                        -- Process-unique identity so right-click actions
                        -- (equip / store / contents) target THIS instance,
                        -- not the first inventory match by defName (#67).
                        Lua.pushinteger (fromIntegral (iiInstanceId inst))
                        Lua.setfield (-2) "instanceId"
                        Lua.pushstring (TE.encodeUtf8 displayName)
                        Lua.setfield (-2) "displayName"
                        Lua.pushboolean broken
                        Lua.setfield (-2) "broken"
                        Lua.pushnumber (Lua.Number (realToFrac (iiSharpness inst)))
                        Lua.setfield (-2) "sharpness"
                        Lua.pushnumber (Lua.Number (realToFrac weight))
                        Lua.setfield (-2) "weight"
                        Lua.pushnumber (Lua.Number (realToFrac (iiCurrentFill inst)))
                        Lua.setfield (-2) "currentFill"
                        -- Signature of nested contents so item-containers
                        -- (kits) split by internal state in the row key (#67A).
                        Lua.pushstring (TE.encodeUtf8 (itemContentsSig inst))
                        Lua.setfield (-2) "contentsKey"
                        -- Only surface quality / condition when the def
                        -- actually declares a spec for them — otherwise
                        -- callers (e.g. inventory tooltip) would show
                        -- "100%" for items like canteens / rations that
                        -- conceptually don't have these qualities.
                        case mDef >>= idQualitySpec of
                            Just _ → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiQuality inst)))
                                Lua.setfield (-2) "quality"
                            Nothing → pure ()
                        case mDef >>= idConditionSpec of
                            Just _ → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiCondition inst)))
                                Lua.setfield (-2) "condition"
                            Nothing → pure ()
                        -- Display-side fields the inventory UI needs.
                        -- Defaulted when the def is missing so the
                        -- renderer always sees a complete row.
                        case mDef of
                            Just d → do
                                Lua.pushstring (TE.encodeUtf8 (idKind d))
                                Lua.setfield (-2) "kind"
                                Lua.pushstring (TE.encodeUtf8 (idCategory d))
                                Lua.setfield (-2) "category"
                                Lua.pushstring (TE.encodeUtf8 (idMake d))
                                Lua.setfield (-2) "make"
                                Lua.pushstring (TE.encodeUtf8 (idMaterial d))
                                Lua.setfield (-2) "material"
                                Lua.pushboolean (idUnequippable d)
                                Lua.setfield (-2) "unequippable"
                                unless (null (idBuffs d)) $ do
                                    Lua.newtable
                                    forM_ (zip [1 ∷ Int ..] (idBuffs d))
                                        $ \(j, b) → do
                                            Lua.newtable
                                            Lua.pushstring
                                                (TE.encodeUtf8 (ibStat b))
                                            Lua.setfield (-2) "stat"
                                            Lua.pushnumber (Lua.Number
                                                (realToFrac (ibAmount b)))
                                            Lua.setfield (-2) "amount"
                                            Lua.pushboolean
                                                (ibScalesWithCondition b)
                                            Lua.setfield (-2)
                                                "scalesWithCondition"
                                            Lua.rawseti (-2) (fromIntegral j)
                                    Lua.setfield (-2) "buffs"
                                let TextureHandle tex = idTexture d
                                Lua.pushinteger (fromIntegral tex)
                                Lua.setfield (-2) "iconTex"
                            Nothing → do
                                Lua.pushstring "misc"
                                Lua.setfield (-2) "kind"
                                Lua.pushstring "Misc"
                                Lua.setfield (-2) "category"
                        case mContainer of
                            Just c → do
                                Lua.pushnumber (Lua.Number (realToFrac (icCapacity c)))
                                Lua.setfield (-2) "capacity"
                                Lua.pushstring (TE.encodeUtf8 (icHolds c))
                                Lua.setfield (-2) "holds"
                            Nothing → pure ()
                        case mDef >>= idFood of
                            Just f → do
                                Lua.newtable                -- food
                                Lua.newtable                -- food.nutrition
                                Lua.pushnumber (Lua.Number (realToFrac (ifCalories f)))
                                Lua.setfield (-2) "calories"
                                Lua.setfield (-2) "nutrition"
                                Lua.setfield (-2) "food"
                            Nothing → pure ()
                        case mDef >>= idWeapon of
                            Just w → do
                                Lua.newtable
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwBladeLength w)))
                                Lua.setfield (-2) "bladeLength"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwBaseSharpness w)))
                                Lua.setfield (-2) "baseSharpness"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwStabEff w)))
                                Lua.setfield (-2) "stabEffectiveness"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwSlashEff w)))
                                Lua.setfield (-2) "slashEffectiveness"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwBluntEff w)))
                                Lua.setfield (-2) "bluntEffectiveness"
                                Lua.setfield (-2) "weapon"
                            Nothing → pure ()
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1

-- | unit.getVisibleTiles(uid) → array of {x, y} tables, or nil if the
--   unit doesn't exist. Includes the unit's own tile.
unitGetVisibleTilesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetVisibleTilesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            tiles ← Lua.liftIO $ unitVisibleTiles env uid
            -- Return nil specifically when the unit is missing
            -- (distinct from "exists but sees nothing", which is []).
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            if not (HM.member uid (umInstances um))
                then do
                    Lua.pushnil
                    return 1
                else do
                    Lua.newtable
                    forM_ (zip [1 ..] tiles) $ \(i, (gx, gy)) → do
                        Lua.newtable
                        Lua.pushinteger (fromIntegral gx)
                        Lua.setfield (-2) "x"
                        Lua.pushinteger (fromIntegral gy)
                        Lua.setfield (-2) "y"
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1

-- | unit.getFrameTexture(uid) → texture handle integer (0 if missing).
--   Returns the texture for the unit's current animation frame at the
--   active camera facing — re-query each tick to follow the animation.
--   Used by the v2 info pane to mirror the unit's sprite as a portrait.
unitGetFrameTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetFrameTextureFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mTex ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                cam ← readIORef (cameraRef env)
                now ← readIORef (gameTimeRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case HM.lookup (uiDefName inst) (umDefs um) of
                            Nothing  → Nothing
                            -- Lua only needs the texture handle; the flipX
                            -- flag from `pickFrame` is consumed by the
                            -- renderer at draw time, not here.
                            Just def → Just (fst (pickFrame now (camFacing cam) inst def))
            case mTex of
                Just (TextureHandle k) → Lua.pushinteger (fromIntegral k)
                Nothing → Lua.pushinteger 0
            return 1

-- | unit.getPortraitTexture(uid) → texture handle integer (0 if the
--   unit is missing or its def declares no authored `portrait:`).
--   The info pane prefers this static authored portrait and falls back
--   to `getFrameTexture` (the live animation frame) when it returns 0.
unitGetPortraitTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPortraitTextureFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mTex ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case HM.lookup (uiDefName inst) (umDefs um) of
                            Nothing  → Nothing
                            Just def → udPortrait def
            case mTex of
                Just (TextureHandle k) → Lua.pushinteger (fromIntegral k)
                Nothing → Lua.pushinteger 0
            return 1
