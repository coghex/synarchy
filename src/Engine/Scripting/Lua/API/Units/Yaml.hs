{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Yaml
  ( loadUnitYamlFn
  , surfaceZInWorld
  )
    where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlUnits (UnitYamlDef(..), UnitYamlAnim(..), UnitYamlStat(..), UnitYamlSkill(..), UnitYamlBody(..), UnitYamlBodyAttr(..), UnitYamlInventoryEntry(..), UnitYamlModifier(..), UnitYamlBodyPart(..), UnitYamlLayer(..), UnitYamlNaturalWeapon(..), UnitYamlStrike(..), UnitYamlNaturalResistance(..), loadUnitYaml)
import Engine.Asset.YamlNames (loadNamePool)
import System.FilePath (takeDirectory, (</>), (<.>))
import Unit.Types
import Unit.Direction (Direction(..))
import World.Types (WorldState(..), LoadedChunk(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)
import Engine.Scripting.Lua.API.Units.List (unknownUnitTexture, unknownUnitAnimFrame)

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
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadUnitYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name      = uydName def
                        spritePath = T.unpack (uydSprite def)

                    resolvedSprite ← resolveTexturePath env "Unit sprite"
                                         (unknownUnitTexture DirS) spritePath
                    handle ← loadAndRegister env backendState lteq
                                 ("unit_" <> name) resolvedSprite

                    -- Load the optional authored portrait (info panel).
                    -- Nothing → the UI mirrors the live animation frame.
                    portraitH ← case uydPortrait def of
                        Nothing → return Nothing
                        Just p  → do
                            resolvedP ← resolveTexturePath env "Unit portrait"
                                            (unknownUnitTexture DirS) (T.unpack p)
                            Just <$> loadAndRegister env backendState lteq
                                         ("unit_" <> name <> "_portrait")
                                         resolvedP

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
                                resolved ← resolveTexturePath env "Unit directional sprite"
                                               (unknownUnitTexture dir) (T.unpack texPath)
                                h ← loadAndRegister env backendState lteq
                                        ("unit_" <> name <> "_" <> dirKey)
                                        resolved
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
                                    handles ← mapM (\(i, p) → do
                                        resolved ← resolveTexturePath env "Unit animation frame"
                                                       (unknownUnitAnimFrame animName dir i) (T.unpack p)
                                        loadAndRegister env backendState lteq
                                            ("unit_" <> name
                                             <> "_" <> animName
                                             <> "_" <> dirKey
                                             <> "_" <> T.pack (show i))
                                            resolved
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
                                , bpAffectsLocomotion = uybpAffectsLocomotion p
                                , bpAffectsBalance     = uybpAffectsBalance p
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
