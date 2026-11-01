{-# LANGUAGE Strict #-}
-- | The Lua surface of the flora condition seam (#2555, EFM-7): two
--   enqueue-only verbs for scripts and probes, the additive condition
--   fields every flora query reports, and the diagnostic fallback trace.
--
--   Lua owns NO state here. @world.setFloraCondition@ and
--   @world.clearFloraCondition@ only validate their arguments' SHAPES and
--   enqueue a command; the world thread
--   ("World.Thread.Command.Flora.Condition") validates the occurrence
--   against live state and writes. Their boolean is therefore QUEUE
--   ADMISSION, never proof of mutation: poll @world.getFloraGrowthAt@ /
--   @world.getCropPlotAt@, or drain the @flora.condition.*@ action
--   outcomes, to learn what the world thread did.
--
--   Player-facing UI is out of scope; nothing under @scripts/@ calls
--   these.
module Engine.Scripting.Lua.API.Forage.Condition
    ( worldSetFloraConditionFn
    , worldClearFloraConditionFn
    , worldGetFloraVisualAtFn
    , pushConditionFields
    ) where

import UPrelude
import Gameplay.Tags.Page (CropPlotStore(..))
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlFlora
    (conditionText, contextText, variantSelectorText)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv, activeWorldPageFrom, activeWorldStateFrom)
import World.Types
import World.Flora.Clock (growthCalendar)
import World.Flora.Condition
    (ConditionRetention(..), FloraConditionRecordOf(..), RetentionSuccessor(..))
import World.Flora.ConditionSeam (deathCauseText)
import World.Flora.Growth (FloraGrowth(..), annualStageText, lifePhaseText)
import World.Flora.Render
    (OccurrenceAppearance(..), occurrenceAppearance, plotAppearance)
import World.Flora.Visual
    (FloraTextureCandidate(..), FloraTextureResolution(..))
import World.Generate.Coordinates (canonicalTileFrame)
import Engine.Scripting.Lua.API.Forage.Lookup (canonicalPageTile)

-- | How a verb's third argument addresses the occurrence.
data Addressing
    = AddressPlot                    -- ^ @nil@ / absent: the tile's plot
    | AddressInstance !FloraInstanceId
    | AddressMalformed               -- ^ not an id: a shape error
    deriving (Eq)

-- | Read argument @n@ as @instanceId | nil@. A number must be a valid
--   instance id ('floraInstanceIdFromLua' refuses the reserved zero and
--   anything in neither namespace); a numeric STRING is not a number
--   (@Lua.tointeger@ would coerce it, so the stack type is checked
--   first); any other type is malformed.
readAddressing ∷ Lua.StackIndex → Lua.LuaE Lua.Exception Addressing
readAddressing ix = do
    ty ← Lua.ltype ix
    case ty of
        Lua.TypeNil  → pure AddressPlot
        Lua.TypeNone → pure AddressPlot
        Lua.TypeNumber → do
            mN ← Lua.tointeger ix
            -- The Lua id space is the POSITIVE Int64 range
            -- ('floraInstanceIdToLua'); a negative number names nothing
            -- and is refused here rather than trusted to the namespace
            -- predicates, which test a single bit each.
            pure $ case mN of
                Just n | n > 0
                       , Just iid ← floraInstanceIdFromLua (fromIntegral n)
                       → AddressInstance iid
                _ → AddressMalformed
        _ → pure AddressMalformed

-- | A real number argument as a whole tile coordinate.
readCoord ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Int)
readCoord ix = do
    ty ← Lua.ltype ix
    if ty ≢ Lua.TypeNumber then pure Nothing
    else fmap fromIntegral <$> Lua.tointeger ix

-- | world.setFloraCondition(gx, gy, instanceId | nil, cause) → bool
--
--   Enqueue marking the occurrence at (gx, gy) dead of @cause@: a wild or
--   row-flora occurrence by @instanceId@, or — @nil@ — the groundcover
--   plot planted on the tile. @cause@ is one of @natural@, @drought@,
--   @frost@, @fire@, @disease@, @damage@, @unknown@.
--
--   Returns true when the request was ADMITTED to the world queue, false
--   when an argument is malformed (a non-number coordinate, an id that
--   names no possible plant — including the reserved zero — a non-string
--   cause) or no world is active. The page is captured here; the
--   occurrence, an unrecognized cause token and an already-conditioned
--   occurrence are semantic refusals the world thread records as
--   @flora.condition.set@ action outcomes, changing nothing. Accepts any
--   u-alias of the tile.
worldSetFloraConditionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetFloraConditionFn env = do
    mGx ← readCoord 1
    mGy ← readCoord 2
    addr ← readAddressing 3
    causeTy ← Lua.ltype 4
    mCause ← if causeTy ≡ Lua.TypeString then Lua.tostring 4
             else pure Nothing
    admitted ← case (mGx, mGy, mCause) of
        (Just gx, Just gy, Just causeBS) | addr ≢ AddressMalformed →
            Lua.liftIO $ enqueueFor env $ \pid → WorldSetFloraCondition pid
                gx gy (addressedId addr) (TE.decodeUtf8Lenient causeBS)
        _ → pure False
    Lua.pushboolean admitted
    pure 1

-- | world.clearFloraCondition(gx, gy, instanceId | nil) → bool
--
--   The symmetric clear, with the same admission semantics as
--   world.setFloraCondition. Removes ONLY the addressed explicit
--   condition: planting time, generation and harvest timers are left
--   alone, and the occurrence returns to its clock-derived behaviour
--   (which can itself be a natural dead window).
worldClearFloraConditionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearFloraConditionFn env = do
    mGx ← readCoord 1
    mGy ← readCoord 2
    addr ← readAddressing 3
    admitted ← case (mGx, mGy) of
        (Just gx, Just gy) | addr ≢ AddressMalformed →
            Lua.liftIO $ enqueueFor env $ \pid →
                WorldClearFloraCondition pid gx gy (addressedId addr)
        _ → pure False
    Lua.pushboolean admitted
    pure 1

addressedId ∷ Addressing → Maybe FloraInstanceId
addressedId (AddressInstance iid) = Just iid
addressedId _                     = Nothing

-- | Capture the ACTIVE page and enqueue the command built for it.
enqueueFor ∷ EngineEnv → (WorldPageId → WorldCommand) → IO Bool
enqueueFor env mk = do
    let wsc = toWorldSimCapability env
    mPage ← activeWorldPageFrom (wsWorldManagerRef wsc)
    case mPage of
        Nothing → pure False
        Just (pid, _) → Q.writeQueue (wsWorldQueue wsc) (mk pid) ≫ pure True

-- | The ADDITIVE condition fields every flora query reports (#2555):
--
--   * @condition@ — the EFFECTIVE condition, @alive@ or @dead@ (an
--     explicit record, or the natural dead window);
--   * @context@ — @wild@ or @cultivated@;
--   * @cause@ — the record's cause, or @natural@ inside the natural dead
--     window (what the resolver is asked, D-21); absent while alive;
--   * @phaseAtDeath@ / @stageAtDeath@ — the record's frozen snapshot,
--     absent when there is no record or the species freezes none;
--   * @retention@ — @{visibility, expiryDay, successor}@, the outcome the
--     record SNAPSHOTTED at death, reported as stored and never
--     recomputed from the species; absent without a record.
--
--   A natural dead window creates no record, so its death metadata is
--   deliberately absent.
pushConditionFields ∷ FloraContext → FloraGrowth → Lua.LuaE Lua.Exception ()
pushConditionFields ctx g = do
    setText "condition" (conditionText (if fgDead g then ConditionDead
                                        else ConditionAlive))
    setText "context" (contextText ctx)
    case fgCondition g of
        Nothing → when (fgDead g) $ setText "cause" (deathCauseText CauseNatural)
        Just r → do
            setText "cause" (deathCauseText (fcrCause r))
            forM_ (fcrPhase r) $ setText "phaseAtDeath" ∘ lifePhaseText
            forM_ (fcrStage r) $ setText "stageAtDeath" ∘ annualStageText
            pushRetention (fcrRetention r)
            Lua.setfield (-2) "retention"
  where
    setText key v = do
        Lua.pushstring (TE.encodeUtf8 v)
        Lua.setfield (-2) key

pushRetention ∷ ConditionRetention → Lua.LuaE Lua.Exception ()
pushRetention ret = do
    Lua.newtable
    case ret of
        RetentionPersistent → text "visibility" "persistent"
        RetentionTransient expiry successor → do
            text "visibility" "transient"
            Lua.pushinteger (fromIntegral expiry)
            Lua.setfield (-2) "expiryDay"
            text "successor" $ case successor of
                RetainReseed          → "reseed"
                RetainAbsent          → "absent"
                RetainAwaitReplanting → "await_replanting"
  where
    text key v = do
        Lua.pushstring (TE.encodeUtf8 v)
        Lua.setfield (-2) key

-- | The readable name of a candidate in the trace.
candidateText ∷ FloraTextureCandidate → Text
candidateText c = case c of
    VariantCandidate sel → "variant:" <> variantSelectorText sel
    LegacyOverride (AnnualCycleKey p s) →
        "override:phase=" <> lifePhaseText p <> ",stage=" <> annualStageText s
    LegacyStage t → "stage:" <> annualStageText t
    LegacyPhase t → "phase:" <> lifePhaseText t
    SpeciesBase   → "base"

-- | world.getFloraVisualAt(gx, gy, instanceId | nil)
--   → {texture, winner, tried} | nil
--
--   The diagnostic fallback trace (#2555): what the occurrence DRAWS, and
--   why. @texture@ is the registered texture name of the resolved handle
--   (@flora_*@; nil when the species has no art for the state, which the
--   renderer drops), @winner@ the candidate that won, @tried@ the ordered
--   candidates the ladder walked, the winner last. Harvest depletion of a
--   LIVING occurrence is reported as winner @harvested@ — the same
--   decision the draw makes, because both ask
--   'World.Flora.Render.occurrenceAppearance'. @instanceId@ addresses a
--   wild or row-flora occurrence; @nil@ the tile's groundcover plot. nil
--   for an unknown target (or a malformed id). Accepts any u-alias.
worldGetFloraVisualAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFloraVisualAtFn env = do
    mGx ← readCoord 1
    mGy ← readCoord 2
    addr ← readAddressing 3
    case (mGx, mGy) of
        (Just rawGX, Just rawGY) | addr ≢ AddressMalformed → do
            mTrace ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure Nothing
                    Just ws → traceFor env ws rawGX rawGY addr
            case mTrace of
                Nothing → Lua.pushnil
                Just (mName, winner, tried) → do
                    Lua.newtable
                    forM_ mName $ \name → do
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.setfield (-2) "texture"
                    Lua.pushstring (TE.encodeUtf8 winner)
                    Lua.setfield (-2) "winner"
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] tried) $ \(i, t) → do
                        Lua.pushstring (TE.encodeUtf8 t)
                        Lua.rawseti (-2) (fromIntegral i)
                    Lua.setfield (-2) "tried"
        _ → Lua.pushnil
    pure 1

traceFor ∷ EngineEnv → WorldState → Int → Int → Addressing
         → IO (Maybe (Maybe Text, Text, [Text]))
traceFor env ws rawGX rawGY addr = do
    cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    (daysPerYear, _, absDay) ← growthCalendar ws
    (gx, gy) ← canonicalPageTile ws rawGX rawGY
    registry ← readIORef (rvTextureNameRegistryRef (toRenderViewCapability env))
    let describe handle = textureName registry handle
    case addr of
        AddressMalformed → pure Nothing
        AddressPlot → do
            plots ← cpsPlots <$> readIORef (wsCropPlotsRef ws)
            pure $ do
                cp ← HM.lookup (gx, gy) plots
                r ← plotAppearance cat daysPerYear absDay cp
                Just (describe (ftrTexture r), candidateText (ftrWinner r)
                     , map candidateText (ftrTried r))
        AddressInstance iid → do
            td ← readIORef (wsTilesRef ws)
            conditions ← readIORef (wsFloraConditionsRef ws)
            harvests ← readIORef (wsFloraHarvestsRef ws)
            worldSize ← pageWrapWorldSize ws
            let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
            pure $ do
                lc ← lookupChunk coord td
                inst ← listToMaybe
                    [ i | i ← fcdInstances (lcFlora lc)
                        , fiInstanceId i ≡ iid
                        , fromIntegral (fiTileX i) ≡ lx
                        , fromIntegral (fiTileY i) ≡ ly ]
                app ← occurrenceAppearance cat daysPerYear absDay
                          conditions harvests inst
                Just $ case app of
                    AppearanceHarvested h → (describe h, "harvested", ["harvested"])
                    AppearanceResolved r →
                        ( describe (ftrTexture r), candidateText (ftrWinner r)
                        , map candidateText (ftrTried r) )

-- | The registered name of a texture handle, preferring the flora
--   family's names; nil for handle 0 (no art) or an unregistered handle.
textureName ∷ HM.HashMap Text TextureHandle → TextureHandle → Maybe Text
textureName registry handle
    | handle ≡ TextureHandle 0 = Nothing
    | otherwise = snd <$> listToMaybe (sort
        [ (not (T.isPrefixOf "flora_" n), n)
        | (n, h) ← HM.toList registry, h ≡ handle ])
