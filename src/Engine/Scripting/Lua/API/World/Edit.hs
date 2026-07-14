{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World.Edit
    ( worldAddTileFn
    , worldDigTileFn
    , worldDeleteTileFn
    , worldSetFluidTileFn
    , worldSetSlopeFn
    , worldSetVegFn
    , worldPlantRowCropAtFn
    , worldSetCellFn
    , worldMarkLocationContentsSpawnedFn
    , worldMarkLocationStampedFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Types hiding (activeWorldPage)
import World.Material (MaterialId(..), materialIdByName)

-- | world.markLocationContentsSpawned(gx, gy [, pageId]) — one-time
--   content-spawn flag (#90). An explicit pageId targets that live
--   page (even hidden); omitted defaults to the active world. No-op
--   (queues nothing) when neither resolves to a live page.
worldMarkLocationContentsSpawnedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldMarkLocationContentsSpawnedFn env = do
    gxArg   ← Lua.tointeger 1
    gyArg   ← Lua.tointeger 2
    pageArg ← Lua.tostring 3
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            Lua.liftIO $ do
                mPid ← case pageArg of
                    Just pidBS → pure (Just (WorldPageId (TE.decodeUtf8Lenient pidBS)))
                    Nothing    → (fmap fst) <$> activeWorldPage env
                case mPid of
                    Just pid → Q.writeQueue (worldQueue env) $
                        WorldMarkLocationContentsSpawned pid
                            (fromIntegral gx) (fromIntegral gy)
                    Nothing  → pure ()
            return 0
        _ → return 0

-- | world.markLocationStamped(gx, gy [, pageId]) — one-time geometry-stamp
--   flag (#424). An explicit pageId targets that live page (even hidden);
--   omitted defaults to the active world. No-op (queues nothing) when
--   neither resolves to a live page.
worldMarkLocationStampedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldMarkLocationStampedFn env = do
    gxArg   ← Lua.tointeger 1
    gyArg   ← Lua.tointeger 2
    pageArg ← Lua.tostring 3
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            Lua.liftIO $ do
                mPid ← case pageArg of
                    Just pidBS → pure (Just (WorldPageId (TE.decodeUtf8Lenient pidBS)))
                    Nothing    → (fmap fst) <$> activeWorldPage env
                case mPid of
                    Just pid → Q.writeQueue (worldQueue env) $
                        WorldMarkLocationStamped pid
                            (fromIntegral gx) (fromIntegral gy)
                    Nothing  → pure ()
            return 0
        _ → return 0

-- | world.addTile(pageId, gx, gy, material) → bool
--   Raise the column at (gx, gy) one z of the named material (string
--   name or numeric id). Queued onto the world thread; lands in the
--   edit log via WeAddTile, so it persists like any player edit.
--   Debug terrain placement. Returns false when the material can't
--   be resolved.
worldAddTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldAddTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    -- 4: material name (string) or id (number).
    matName ← Lua.tostring 4
    matNum  ← Lua.tonumber 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
            let mMat = case (matNum, matName) of
                    (Just (Lua.Number n), _) | n ≥ 1 ∧ n ≤ 255 →
                        Just (MaterialId (round n))
                    (_, Just nameBS) →
                        materialIdByName registry
                            (TE.decodeUtf8Lenient nameBS)
                    _ → Nothing
            case mMat of
                Nothing → Lua.pushboolean False >> return 1
                Just mat → do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Lua.liftIO $ Q.writeQueue (worldQueue env) $
                        WorldAddTile pageId (round gx) (round gy) mat
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | world.digTile(pageId, gx, gy, ux, uy, amount, minerSkill,
--   perception) —
--   apply dig progress to the designated tile. (ux, uy) is the
--   digger's tile-space position (drain order); amount is pre-scaled
--   by tool × material speed (see getDigInfoAt). minerSkill (the
--   current digger's mining skill; optional, defaults 0) scales the
--   per-tick chunk-yield fill — pass it every tick so a mid-dig
--   handoff uses the new digger's rate.
worldDigTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDigTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    uxArg ← Lua.tonumber 4
    uyArg ← Lua.tonumber 5
    amtArg ← Lua.tonumber 6
    skillArg ← Lua.tonumber 7
    percepArg ← Lua.tonumber 8
    case (pageIdArg, gxArg, gyArg, uxArg, uyArg, amtArg) of
        (Just pageIdBS, Just gx, Just gy, Just ux, Just uy, Just amt) →
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    skill = case skillArg of
                        Just (Lua.Number s) → realToFrac s
                        _                   → 0
                    percep = case percepArg of
                        Just (Lua.Number s) → realToFrac s
                        _                   → 1.0
                Q.writeQueue (worldQueue env) $
                    WorldDigTile pageId (round gx) (round gy)
                                 (realToFrac ux) (realToFrac uy)
                                 (realToFrac amt) skill percep
        _ → pure ()
    return 0

-- | world.deleteTile(pageId, gx, gy) → bool
-- Enqueues a dig-1-Z-down edit at the given tile. The actual mutation
-- happens on the next world-thread tick, so this returns true once
-- enqueued (not once applied).
worldDeleteTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDeleteTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (worldQueue env)
                    (WorldDeleteTile pageId (fromIntegral gx) (fromIntegral gy))
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setFluidTile(pageId, gx, gy, kind) → bool
-- Places one tile of fluid on top of the column at (gx, gy). `kind` is
-- one of "water" (Lake) / "lava" (Lava) / "river" (River) / "ocean"
-- (Ocean); unknown values fall back to "water". Debug-tool affordance:
-- lets the arena have water sources without waiting for procedural
-- generation.
worldSetFluidTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetFluidTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    kindArg   ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            let fluidType = case kindArg of
                    Just kBS → case TE.decodeUtf8Lenient kBS of
                        "lava"  → Lava
                        "river" → River
                        "ocean" → Ocean
                        _       → Lake     -- "water" / default
                    Nothing → Lake
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (worldQueue env) $
                    WorldSetFluidTile pageId
                        (fromIntegral gx) (fromIntegral gy) fluidType
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setCell(pageId, gx, gy, z, material) → bool
--   Set the single 3D cell at (gx,gy,z) to a material — the locations
--   primitive for carving interior air, walls, ceilings, staircases.
--   `material` is a string name, a numeric id, or "air"/0 to clear the
--   cell. Queued onto the world thread; lands in the edit log via
--   WeSetCell so it persists like any player edit. Grows the column up
--   to reach z; a z below the column floor is dropped (warns). Returns
--   false when the material can't be resolved.
worldSetCellFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCellFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    -- 5: material name (string), numeric id, or "air"/0 for air.
    matName ← Lua.tostring 5
    matNum  ← Lua.tonumber 5
    case (pageIdArg, gxArg, gyArg, zArg) of
        (Just pageIdBS, Just gx, Just gy, Just z) → do
            registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
            let mMat = case (matNum, matName) of
                    (Just (Lua.Number n), _) | n ≥ 0 ∧ n ≤ 255 →
                        Just (MaterialId (round n))
                    (_, Just nameBS) → case TE.decodeUtf8Lenient nameBS of
                        "air" → Just (MaterialId 0)
                        name  → materialIdByName registry name
                    _ → Nothing
            case mMat of
                Nothing → Lua.pushboolean False >> return 1
                Just mat → do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Lua.liftIO $ Q.writeQueue (worldQueue env) $
                        WorldSetCell pageId
                            (fromIntegral gx) (fromIntegral gy)
                            (fromIntegral z) mat
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | world.setSlope(pageId, gx, gy, z, bits) → bool
--   Set the walkable-ramp slope bitmask of the tile at (gx,gy,z).
--   Bits: 0=N 1=E 2=S 3=W; a set bit marks that cardinal neighbour as a
--   1-z ramp down (so a unit can walk up it instead of climbing). addTile
--   only ever makes flat tops (slope 0 = cliff), so this is the only way
--   to author a walkable ramp — exists for the movement test harness.
worldSetSlopeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSlopeFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    bitsArg   ← Lua.tointeger 5
    case (pageIdArg, gxArg, gyArg, zArg, bitsArg) of
        (Just pageIdBS, Just gx, Just gy, Just z, Just bits) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (worldQueue env) $
                    WorldSetSlope pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (fromIntegral z)
                        (fromIntegral bits)  -- → Word8 truncates to low 8 bits
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.plantRowCropAt(pageId, gx, gy, cropName) → bool
--   Plant a single row-crop FloraInstance at (gx,gy) via the WePlaceFlora
--   edit path (queued, same fire-and-forget shape as world.setVegAt) —
--   the farm AI's (#336) row-crop planting completion, the FloraInstance
--   counterpart to world.plantCropAt's CropPlot for groundcover crops.
--   Refused world-thread-side unless the tile is tilled soil and
--   cropName names a registered row_crop species; poll
--   world.getFloraGrowthAt afterward to confirm it landed.
worldPlantRowCropAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldPlantRowCropAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    cropArg   ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg, cropArg) of
        (Just pageIdBS, Just gx, Just gy, Just cropBS) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (worldQueue env) $
                    WorldPlantRowCropAt pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (TE.decodeUtf8Lenient cropBS)
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setVegAt(pageId, gx, gy, z, vegId) → bool
--   Set the vegetation id of the tile at (gx,gy,z). Mirrors
--   world.setSlope's shape and edit-log routing — the till AI's (#333)
--   completion primitive: flips a tilled tile's ground cover to
--   'World.Vegetation.vegTilledSoil' so it survives chunk eviction +
--   save/load like every other edit. No generator path emits arbitrary
--   ids here (computeChunkVegetation owns natural placement).
worldSetVegFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetVegFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    vegIdArg  ← Lua.tointeger 5
    case (pageIdArg, gxArg, gyArg, zArg, vegIdArg) of
        (Just pageIdBS, Just gx, Just gy, Just z, Just vegId) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (worldQueue env) $
                    WorldSetVeg pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (fromIntegral z)
                        (fromIntegral vegId)  -- → Word8 truncates to low 8 bits
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1
