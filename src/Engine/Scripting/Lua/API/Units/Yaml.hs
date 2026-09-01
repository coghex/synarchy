{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Units.Yaml
  ( loadUnitYamlFn
  , AtlasResolver
  , registerUnitDefs
  , resolveUnitAtlases
  , surfaceZInWorld
  )
    where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Log (LogCategory(..), logDebug, logWarn, logError)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg)
import Engine.Asset.Types (AssetPool)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegisterWithPool
                                             , loadAndRegisterAtlasWithPool
                                             , resolveTexturePath)
import Unit.Atlas.Index (AtlasLoadError(..), atlasTextureRequests
                        , renderAtlasLoadError)
import Unit.Atlas.Yaml (resolveUnitAtlases)
import Engine.Asset.YamlUnits (UnitYamlDef(..), UnitYamlAnim, UnitYamlStat(..), UnitYamlSkill(..), UnitYamlBody(..), UnitYamlBodyAttr(..), UnitYamlInventoryEntry(..), UnitYamlModifier(..), UnitYamlNaturalWeapon(..), UnitYamlStrike(..), UnitYamlNaturalResistance(..), loadUnitYaml, unitYamlBodyPartToBodyPart)
import Engine.Asset.YamlNames (loadNamePool)
import System.FilePath (takeDirectory, (</>), (<.>))
import Unit.Types
import Unit.Direction (Direction(..), parseDirectionName)
import World.Types (WorldState(..), LoadedChunk(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)
import Engine.Scripting.Lua.API.Units.List (unknownUnitTexture)

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
                (lteq, _) = lbsMsgQueues backendState
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadUnitYaml logger filePath
                total ← registerUnitDefs env (lbsAssetPool backendState) lteq
                            resolveUnitAtlases filePath defs
                logDebug logger CatAsset $
                    "loadUnitYaml: loaded " <> tshow total
                    <> " unit definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | How a unit's atlas-backed animations are resolved. Production
--   passes 'resolveUnitAtlases', which reads the compiled index off
--   disk; a test passes a canned selection so the REGISTRATION path —
--   handles, queued upload requests, published definitions — can be
--   driven without a compiled asset tree under the resource root.
type AtlasResolver =
    Text → Map.Map Text UnitYamlAnim
         → IO (Either AtlasLoadError (HM.HashMap Text AtlasAnimation))

-- | Register every unit definition in a parsed YAML file, returning how
--   many were published.
--
--   Split out of 'loadUnitYamlFn' (#1259) so the whole registration
--   boundary is reachable without a Lua state: it needs the engine
--   environment, an asset pool, the Lua→engine queue, and a way to
--   resolve atlases, and nothing else. Everything the Lua entry point
--   does beyond this is argument marshalling.
registerUnitDefs
    ∷ EngineEnv
    → IORef AssetPool
    → Q.Queue LuaToEngineMsg
    → AtlasResolver
    → FilePath                    -- ^ the YAML's own path (name pools sit beside it)
    → [UnitYamlDef]
    → IO Int
registerUnitDefs env poolRef lteq resolveAtlases filePath defs = do
    logger ← readIORef (loggerRef env)
    foldM (\acc def → do

        let name      = uydName def
            spritePath = T.unpack (uydSprite def)

        -- ATLAS RESOLUTION (#1259, total since #1261). The
        -- unit's compiled index describes every animation
        -- this YAML declares, and it is resolved and fully
        -- validated HERE, before a single handle is allocated
        -- or a single upload queued. A unit whose index is
        -- missing, incomplete, stale, unsupported, or
        -- malformed is NOT registered at all — there is no
        -- per-frame path left to fall back to, and there was
        -- never one to fall back to silently.
        eAtlas ← resolveAtlases name (uydAnimations def)
        case eAtlas of
          Left err → do
            logError logger CatAsset $
                renderAtlasLoadError err
                <> " — unit definition '" <> name
                <> "' was NOT registered"
            return acc
          Right atlasByName → do
            resolvedSprite ← resolveTexturePath env "Unit sprite"
                                 (unknownUnitTexture DirS) spritePath
            handle ← loadAndRegisterWithPool env poolRef lteq
                         UploadGlobalSampler
                         ("unit_" <> name) resolvedSprite

            -- Load the optional authored portrait (info panel).
            -- Nothing → the UI mirrors the live animation frame.
            --
            -- The one unit texture whose ONLY consumer is a UI panel,
            -- so it declares the UI policy (#2075) and keeps nearest
            -- across a filter toggle. The sprite, directional sprites,
            -- and animation atlases are world-drawn scene art and all
            -- follow the player-selected sampler (#2085).
            portraitH ← case uydPortrait def of
                Nothing → return Nothing
                Just p  → do
                    resolvedP ← resolveTexturePath env "Unit portrait"
                                    (unknownUnitTexture DirS) (T.unpack p)
                    Just <$> loadAndRegisterWithPool env poolRef lteq
                                 UploadPinnedNearest
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
                        h ← loadAndRegisterWithPool env poolRef lteq
                                UploadGlobalSampler
                                ("unit_" <> name <> "_" <> dirKey)
                                resolved
                        return (Map.insert dir h acc)
                ) Map.empty (Map.toList (uydDirectionalSprites def))

            -- ONE upload, handle, and bindless slot per animation
            -- (D-2/D-10), and ONE published `Animation` per upload —
            -- both issued from `atlasTextureRequests`, which IS the
            -- upload set rather than a description of it, so neither
            -- the count nor the published library can drift from what
            -- the selection says. `planUnitAtlasStorage` has already
            -- proved that selection covers exactly the animations this
            -- YAML declares, so there is no per-frame branch left and
            -- no animation this loop can miss.
            animMap ← foldM (\accA (animName, regName, aa) → do
                h ← loadAndRegisterAtlasWithPool env poolRef lteq
                        regName (aaPath aa)
                let anim = atlasAnimation (aaFps aa) (aaLoop aa) (aaFlip aa)
                               (ResidentAtlas aa h)
                return (HM.insert animName anim accA)
                ) HM.empty (atlasTextureRequests name atlasByName)

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
            -- becomes a distinct ItemInstance (the quality and
            -- weight rolls fire per copy; condition starts full
            -- on every one, #1421); the drop priority rides
            -- along for the spawn-time capacity check.
            let startingInv =
                    [ (uyieItem e, uyieFill e, uyieDropPriority e)
                    | e ← uydStartingInventory def
                    , _ ← [1 .. max 1 (uyieCount e)]
                    ]
                bodyParts =
                    [ unitYamlBodyPartToBodyPart p
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
            atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                (um { umDefs = HM.insert name unitDef (umDefs um) }, ())

            logDebug logger CatAsset $
                "Registered unit def: " <> name
                <> " (handle " <> tshow handle <> ")"
                <> " (" <> tshow (Map.size dirMap)
                <> " directional sprites, "
                <> tshow (HM.size animMap)
                <> " animations)"

            return (acc + 1)
        ) (0 ∷ Int) defs

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
--
--   The table itself is 'Unit.Direction.parseDirectionName' — shared
--   with the preview's folder-name parser and with the YAML→facts
--   projection the atlas selection is validated against, so the three
--   cannot drift into accepting different spellings.
parseDirKey ∷ Text → Maybe Direction
parseDirKey = parseDirectionName
