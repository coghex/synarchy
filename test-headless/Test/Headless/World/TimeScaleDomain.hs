{-# LANGUAGE ScopedTypeVariables #-}
-- | @world.setTimeScale@'s accepted domain and return contract (#2280).
--
--   The verb used to take @Lua.tonumber@ and queue @realToFrac s@
--   unconditionally, so NaN, either infinity, a negative scale, a finite
--   Lua number that becomes infinite in the clock's 'Float' storage, and
--   a coercible numeric STRING all reached 'wsTimeScaleRef' — and from
--   there 'advanceWorldClock', whose @floor ∷ Int@ they corrupt or pin.
--   It also returned no results at all, so a caller could not tell a
--   refusal from an acceptance.
--
--   These examples drive the REAL registered Lua binding against a live
--   'EngineEnv'. Nothing here is a stub: the page is a real
--   'WorldState' in the engine's own world manager, the observables are
--   the engine's own refs and queue, and the accepted paths are finished
--   by the production handler
--   ('World.Thread.Command.Time.handleWorldSetTimeScaleCommand') so the
--   running and paused halves of #1599's storage split are both proven
--   end to end.
--
--   __Why this spec owns its engine, world-thread-free.__ Requirement 3
--   is about what a refused call does NOT enqueue, which is only
--   observable while nothing is draining 'worldQueue'. With no world
--   worker running, the queue after a call IS the evidence: empty for
--   every refusal, exactly one 'WorldSetTimeScale' for every
--   acceptance.
module Test.Headless.World.TimeScaleDomain (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (readMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Thread.Command.Time (handleWorldSetTimeScaleCommand)
import World.Types

-- * Fixture

-- | The one page every example drives.
page ∷ WorldPageId
page = WorldPageId "timescale_domain"

-- | The page's Lua-side name, spliced into every chunk.
pageLua ∷ BS.ByteString
pageLua = "'timescale_domain'"

-- | The scale a refused call must leave in place. Distinct from every
--   value any example sets, so \"unchanged\" cannot pass by coincidence.
sentinelScale ∷ Float
sentinelScale = 7

-- | The resume scale a refused call must leave in place while paused.
sentinelResume ∷ Float
sentinelResume = 11

-- | Install a one-page session in the given pause state and hand back
--   the page's live 'WorldState', with the world queue drained so the
--   queue observation below starts from empty.
installPage ∷ EngineEnv → Bool → IO WorldState
installPage env paused = do
    ws ← emptyWorldState
    writeIORef (wsTimeScaleRef ws) (if paused then 0 else sentinelScale)
    writeIORef (wsResumeScaleRef ws)
        (if paused then Just sentinelResume else Nothing)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (enginePausedRef env) paused
    _ ← Q.flushQueue (worldQueue env)
    pure ws

-- | A Lua state carrying the full production API — the same
--   registration the real Lua thread performs, so @world.setTimeScale@
--   and @world.getTimeScale@ under test are the shipped bindings.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- * Calling the verb

-- | Everything the return contract pins about one @world.setTimeScale@
--   call: whether the chunk raised at all, how many results came back,
--   and the first two of them.
data ScaleCall = ScaleCall
    { scRaised  ∷ Bool
    , scArity   ∷ Int
    , scFirst   ∷ Maybe Bool   -- ^ 'Nothing' unless result 1 is a boolean.
    , scSecond  ∷ Maybe Text   -- ^ 'Nothing' unless result 2 is a string.
    } deriving (Eq, Show)

-- | Call the verb with the given Lua argument list and report the whole
--   contract. @table.pack@ rather than a bare @return@ so the ARITY is
--   observed directly instead of being inferred from what happens to be
--   on the stack.
callScale ∷ LuaBackendState → BS.ByteString → IO ScaleCall
callScale ls args = Lua.runWith (lbsLuaState ls) $ do
    status ← Lua.dostring
        ("local r = table.pack(world.setTimeScale(" <> args <> "))\n\
         \__ts_n, __ts_a, __ts_b = r.n, r[1], r[2]")
    case status of
        Lua.OK → do
            arity ← globalInt "__ts_n"
            first ← globalBool "__ts_a"
            second ← globalString "__ts_b"
            pure (ScaleCall False (maybe (-1) fromIntegral arity) first second)
        _ → do
            Lua.pop 1
            pure (ScaleCall True (-1) Nothing Nothing)

globalInt ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Lua.Integer)
globalInt name = do
    ty ← Lua.getglobal (Lua.Name name)
    v ← if ty ≡ Lua.TypeNumber then Lua.tointeger (-1) else pure Nothing
    Lua.pop 1
    pure v

globalBool ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Bool)
globalBool name = do
    ty ← Lua.getglobal (Lua.Name name)
    v ← if ty ≡ Lua.TypeBoolean then Just ⊚ Lua.toboolean (-1) else pure Nothing
    Lua.pop 1
    pure v

globalString ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Text)
globalString name = do
    ty ← Lua.getglobal (Lua.Name name)
    v ← if ty ≡ Lua.TypeString
            then fmap TE.decodeUtf8Lenient ⊚ Lua.tostring (-1)
            else pure Nothing
    Lua.pop 1
    pure v

-- | @world.getTimeScale@ read back through Lua, i.e. the getter the
--   contract says a refused call must not move.
getterScale ∷ LuaBackendState → IO (Maybe Double)
getterScale ls = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.dostring ("return world.getTimeScale(" <> pageLua <> ")")
    n ← Lua.tonumber (-1)
    Lua.pop 1
    pure ((\(Lua.Number d) → d) ⊚ n)

-- * Assertions

-- | Assert the complete refusal shape: no Lua error, exactly two
--   results, @false@ first, and a diagnostic naming @expected@.
expectRefusal ∷ ScaleCall → Text → Expectation
expectRefusal call expected = do
    scRaised call `shouldBe` False
    scArity call `shouldBe` 2
    scFirst call `shouldBe` Just False
    case scSecond call of
        Nothing → expectationFailure
            ("expected a diagnostic string, got " ⧺ show call)
        Just diagnostic →
            unless (expected `T.isInfixOf` diagnostic) $ expectationFailure
                ("diagnostic " ⧺ show diagnostic ⧺ " does not mention "
                 ⧺ show expected)

-- | Assert every side effect requirement 3 forbids is absent: nothing
--   queued, the live scale, the pause epoch's resume scale and the
--   getter all where they were, and the player-intent generation not
--   advanced.
expectNoSideEffects ∷ EngineEnv → LuaBackendState → WorldState → Word64
                    → Float → Maybe Float → Expectation
expectNoSideEffects env ls ws gen0 liveBefore resumeBefore = do
    queued ← Q.flushQueue (worldQueue env)
    length queued `shouldBe` 0
    readIORef (wsTimeScaleRef ws) `shouldReturn` liveBefore
    readIORef (wsResumeScaleRef ws) `shouldReturn` resumeBefore
    getterScale ls `shouldReturn` Just (realToFrac liveBefore)
    readMVar (playerIntentGenRef env) `shouldReturn` gen0

-- | Drive one refusal end to end in the given pause state.
refusalCase ∷ EngineEnv → Bool → BS.ByteString → Text → Expectation
refusalCase env paused args expected = do
    ws ← installPage env paused
    ls ← newBackend env
    liveBefore ← readIORef (wsTimeScaleRef ws)
    resumeBefore ← readIORef (wsResumeScaleRef ws)
    gen0 ← readMVar (playerIntentGenRef env)
    call ← callScale ls args
    expectRefusal call expected
    expectNoSideEffects env ls ws gen0 liveBefore resumeBefore

-- | Drive one acceptance end to end: exactly one @true@ result, exactly
--   one queued command carrying the expected scale, the player-intent
--   generation advanced once, and the production handler landing the
--   value where the pause state says it belongs.
acceptanceCase ∷ EngineEnv → Bool → BS.ByteString → Float → Expectation
acceptanceCase env paused args expected = do
    ws ← installPage env paused
    ls ← newBackend env
    gen0 ← readMVar (playerIntentGenRef env)
    call ← callScale ls args
    scRaised call `shouldBe` False
    scArity call `shouldBe` 1
    scFirst call `shouldBe` Just True
    scSecond call `shouldBe` Nothing
    readMVar (playerIntentGenRef env) `shouldReturn` (gen0 + 1)
    queued ← Q.flushQueue (worldQueue env)
    case queued of
        [WorldSetTimeScale pid scale] → do
            pid `shouldBe` page
            scale `shouldBe` expected
        other → expectationFailure
            ("expected one WorldSetTimeScale, got " ⧺ show other)
    -- Finish the accepted path through the production handler, so the
    -- running/paused storage split (#1599) is proven rather than assumed.
    logger ← readIORef (loggerRef env)
    handleWorldSetTimeScaleCommand (toWorldSimCapability env) logger page expected
    if paused
        then do
            readIORef (wsTimeScaleRef ws) `shouldReturn` 0
            readIORef (wsResumeScaleRef ws) `shouldReturn` Just expected
        else do
            readIORef (wsTimeScaleRef ws) `shouldReturn` expected
            readIORef (wsResumeScaleRef ws) `shouldReturn` Nothing

-- | Rendered exactly as the boundary renders a 'Float', so the expected
--   substring is derived from the shared domain rather than retyped.
rendered ∷ Float → Text
rendered = tshow

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "world.setTimeScale domain" $ do

    describe "accepted scales keep working" $ do
        it "0 is accepted and pauses the page clock" $ \env →
            acceptanceCase env False (pageLua <> ", 0") 0
        it "-0.0 is accepted as zero" $ \env →
            acceptanceCase env False (pageLua <> ", -0.0") 0
        it "an ordinary positive scale is accepted" $ \env →
            acceptanceCase env False (pageLua <> ", 2.5") 2.5
        it "the 50000 probe scale is accepted while running" $ \env →
            acceptanceCase env False (pageLua <> ", 50000") 50000
        it "the 50000 probe scale is accepted while paused" $ \env →
            acceptanceCase env True (pageLua <> ", 50000") 50000
        it "the derived ceiling itself is accepted" $ \env →
            acceptanceCase env False
                (pageLua <> ", " <> BS.pack (show maxTimeScale)) maxTimeScale

    describe "refused scales change nothing, while running" $ do
        it "refuses NaN" $ \env →
            refusalCase env False (pageLua <> ", 0/0") "NaN"
        it "refuses +infinity" $ \env →
            refusalCase env False (pageLua <> ", math.huge") "Infinity"
        it "refuses -infinity" $ \env →
            refusalCase env False (pageLua <> ", -math.huge") "-Infinity"
        it "refuses a negative finite scale" $ \env →
            refusalCase env False (pageLua <> ", -1.5") "-1.5"
        it "refuses a finite Lua number that overflows Float" $ \env →
            -- 1e300 is an ordinary finite Double; it is only unsafe once
            -- narrowed to the clock's authoritative Float storage, which
            -- is why classification happens AFTER that conversion.
            refusalCase env False (pageLua <> ", 1e300") "Infinity"
        it "refuses a finite Float above the derived ceiling" $ \env →
            refusalCase env False
                (pageLua <> ", " <> BS.pack (show (maxTimeScale * 2)))
                (rendered (maxTimeScale * 2))
        it "refuses the numeric string \"1\", proving the type check \
           \precedes the numeric conversion" $ \env →
            -- Lua.tonumber would coerce this; Lua.ltype does not.
            refusalCase env False (pageLua <> ", '1'") "string"
        it "refuses a boolean" $ \env →
            refusalCase env False (pageLua <> ", true") "boolean"
        it "refuses a missing scale argument without raising" $ \env →
            -- tools/structure_rotation_probe.py issues exactly this
            -- (`world.setTimeScale(0)`), as a bare statement.
            refusalCase env False pageLua "number"

    describe "refused scales change nothing, while paused" $ do
        it "refuses NaN and leaves the pause epoch's resume scale alone" $ \env →
            refusalCase env True (pageLua <> ", 0/0") "NaN"
        it "refuses +infinity and leaves the pause epoch's resume scale \
           \alone" $ \env →
            refusalCase env True (pageLua <> ", math.huge") "Infinity"
        it "refuses a negative finite scale and leaves the pause epoch's \
           \resume scale alone" $ \env →
            refusalCase env True (pageLua <> ", -1.5") "-1.5"
        it "refuses a scale above the derived ceiling and leaves the pause \
           \epoch's resume scale alone" $ \env →
            refusalCase env True
                (pageLua <> ", " <> BS.pack (show (maxTimeScale * 2)))
                (rendered (maxTimeScale * 2))

    describe "the page argument" $
        it "refuses an accepted scale with a non-string page id, in the \
           \refusal shape and with no side effect" $ \env → do
            -- A table cannot be coerced to a string, so the pageId is
            -- unreadable; nothing may be queued and the result must still
            -- be the two-value refusal rather than silence.
            ws ← installPage env False
            ls ← newBackend env
            gen0 ← readMVar (playerIntentGenRef env)
            call ← callScale ls "{}, 1.0"
            expectRefusal call "page id"
            expectNoSideEffects env ls ws gen0 sentinelScale Nothing
