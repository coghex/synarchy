{-# LANGUAGE ScopedTypeVariables #-}
-- | Bulk chunk work is admitted to — and waited on — the page the
--   CALLER chose, not whichever page happens to be applied (#2310).
--
--   @world.show@ is queued on the world thread, so it moves @wmVisible@
--   a tick later than it is issued. Both chunk verbs used to read that
--   applied list, and independently: 'worldLoadChunksInRegionFn' through
--   @activeWorldPageFrom@ once, 'worldWaitForChunksFn' through
--   @activeWorldStateFrom@ on every 250 ms poll. So the probe suite's
--   standard opening
--
--   > world.show('B'); world.loadChunksInRegion(...); world.waitForChunks(...)
--
--   queued its region on the OUTGOING page A, then — once the show
--   landed — polled the INCOMING page B's empty queue and reported
--   completion while A was still generating. Nothing cancels the
--   misdirected work either
--   ('World.Thread.ChunkLoading.drainInitQueues' serves every page, by
--   design), so the observable was a wait that succeeded against work
--   that had not happened: exactly the shape of an unexplained probe
--   flake.
--
--   __Why this spec owns a world-thread-free engine.__ The whole defect
--   lives in the window between a @WorldShow@ being enqueued and being
--   applied. With no world worker draining 'worldQueue' that window is
--   a stable fixture rather than a race: the show sits unapplied for as
--   long as the example needs, @wmVisible@ stays on A, and
--   @wmProjectedVisible@ (#1602, published by the real
--   @enqueueSelectionChange@ the real @world.show@ binding calls) stays
--   on B. It also means a page's init queue is exactly what a producer
--   left there, so \"which page did the work land on\" is answered by
--   reading the two queues rather than inferred.
--
--   Every example drives a REAL binding: the registered Lua
--   @world.loadChunksInRegion@ / @world.waitForChunks@, and the debug
--   console's own off-Lua-thread fast path
--   ('Engine.Scripting.Lua.Thread.Console.debugBuiltin'), which
--   recognises the probes' exact @world.waitForChunks(...)@ commands and
--   used to re-resolve the active page on every poll of its own.
module Test.Headless.World.ChunkPageBinding (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), chunkTargetWorld)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (debugBuiltin)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Types

-- * Fixture

-- | The APPLIED page: the head of @wmVisible@, and so the page every
--   pre-#2310 resolution answers with.
pageA ∷ WorldPageId
pageA = WorldPageId "chunk_page_applied"

-- | The INCOMING page: registered and live, but only ever the head of
--   @wmProjectedVisible@ in these examples — the show that would make it
--   applied is still sitting in 'worldQueue'.
pageB ∷ WorldPageId
pageB = WorldPageId "chunk_page_projected"

-- | A page id nothing registers. An explicit request naming it must
--   queue nothing and report so, NEVER fall back to a live page.
pageMissing ∷ WorldPageId
pageMissing = WorldPageId "chunk_page_absent"

-- | The Lua string literals for the three ids above, spliced into the
--   chunks below and into the console commands, so a rename cannot
--   leave the two halves naming different pages.
luaId ∷ WorldPageId → BS.ByteString
luaId pid = "'" <> TE.encodeUtf8 (unWorldPageId pid) <> "'"

-- | Big enough that nothing in these small regions aliases across the
--   u-seam, so a queued count is just the rectangle's area (#1723 is
--   'Test.Headless.World.ChunkQueueFrame''s subject, not this one).
pageParams ∷ WorldGenParams
pageParams = defaultWorldGenParams { wgpWorldSize = 64 }

-- | How many chunks each page's queue is SEEDED with, before any
--   example asks for more. Distinct, and distinct from every region
--   area below, so \"which page answered\" is legible from the number
--   alone: a wait that reports 'seededA' watched A, one that reports
--   'seededB' watched B.
seededA, seededB ∷ Int
seededA = 3
seededB = 7

-- | Coordinates far from every requested region, so seeding a queue
--   cannot make a later request dedup against it.
seedCoords ∷ Int → [ChunkCoord]
seedCoords n = [ ChunkCoord 900 y | y ← [1 .. n] ]

-- | The rectangle the examples request, and its physical area.
regionArgs ∷ BS.ByteString
regionArgs = "0, 0, 2, 2"

regionArea ∷ Int
regionArea = 9

-- | A live page with generation params and a seeded init queue. Its
--   state belongs to no thread here, so the queue stays exactly as a
--   producer leaves it.
livePage ∷ Int → IO WorldState
livePage seeded = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just pageParams)
    writeIORef (wsInitQueueRef ws) (seedCoords seeded)
    pure ws

-- | The two-page session every example starts from: A applied, B
--   registered and live but hidden, the projection SETTLED (nothing
--   outstanding) and the world queue empty.
--
--   Settled rather than hand-poked: an example that wants a selection
--   change in flight gets one by calling the real @world.show@, so the
--   projection under test is the one 'enqueueSelectionChange' publishes.
installPages ∷ EngineEnv → IO (WorldState, WorldState)
installPages env = do
    wsA ← livePage seededA
    wsB ← livePage seededB
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds           = [(pageA, wsA), (pageB, wsB)]
        , wmVisible          = [pageA]
        , wmProjectedWorlds  = [pageA, pageB]
        , wmProjectedVisible = [pageA]
        }
    _ ← Q.flushQueue (worldQueue env)
    pure (wsA, wsB)

-- | A Lua state carrying the full production API — the same
--   registration the real Lua thread performs, so the verbs under test
--   are the shipped bindings.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- * Driving the verbs

-- | Run a chunk of Lua and read back its single numeric result.
--   'Nothing' means the call returned nil (the malformed-argument path)
--   or raised.
callNumber ∷ LuaBackendState → BS.ByteString → IO (Maybe Int)
callNumber ls code = Lua.runWith (lbsLuaState ls) $ do
    status ← Lua.dostring ("return " <> code)
    case status of
        Lua.OK → do
            ty ← Lua.ltype (-1)
            v ← if ty ≡ Lua.TypeNumber
                    then fmap fromIntegral ⊚ Lua.tointeger (-1)
                    else pure Nothing
            Lua.pop 1
            pure v
        _ → Lua.pop 1 ≫ pure Nothing

-- | @world.loadChunksInRegion@ over the fixture's rectangle, with the
--   given trailing argument text (empty for the omitted-page form).
loadRegion ∷ LuaBackendState → BS.ByteString → IO (Maybe Int)
loadRegion ls extra =
    callNumber ls ("world.loadChunksInRegion(" <> regionArgs <> extra <> ")")

-- | @world.waitForChunks@ through the registered Lua binding.
waitLua ∷ LuaBackendState → BS.ByteString → IO (Maybe Int)
waitLua ls args = callNumber ls ("world.waitForChunks(" <> args <> ")")

-- | @world.waitForChunks@ through the DEBUG CONSOLE's fast path. The
--   builtin must RECOGNISE the command (a 'Nothing' here would mean it
--   fell through to the Lua thread, which is the thing that path exists
--   to avoid), and its answer is the remaining count as text.
waitConsole ∷ EngineEnv → Text → IO (Maybe Int)
waitConsole env args = do
    answer ← debugBuiltin env ("return world.waitForChunks(" <> args <> ")")
    pure (answer ⌦ readMaybeInt)
  where
    readMaybeInt t = case TR.decimal (T.strip t) of
        Right (n, rm) | T.null (T.strip rm) → Just n
        _                                   → Nothing

-- | Queue the real @world.show@ for B and leave it unapplied — the one
--   fixture every projected-page example is about.
showBUnapplied ∷ EngineEnv → LuaBackendState → Expectation
showBUnapplied env ls = do
    Lua.runWith (lbsLuaState ls) $
        Lua.dostring ("world.show(" <> luaId pageB <> ")") ⌦ \status →
            when (status ≢ Lua.OK) (Lua.pop 1)
    -- Preconditions, or every assertion below could hold vacuously: the
    -- show really is outstanding, the APPLIED head really is still A,
    -- and the PROJECTED head really is B.
    mgr ← readIORef (worldManagerRef env)
    wmSelectionPending mgr `shouldSatisfy` (> 0)
    wmVisible mgr `shouldBe` [pageA]
    take 1 (wmProjectedVisible mgr) `shouldBe` [pageB]
    -- And it is still QUEUED: nothing has applied it. Draining it here
    -- is safe and deliberate — no worker exists to apply it, and the
    -- projection it published is what the examples are about.
    queued ← Q.flushQueue (worldQueue env)
    case queued of
        [WorldShow pid] → pid `shouldBe` pageB
        other → expectationFailure
            ("expected one queued WorldShow, got " ⧺ show other)

-- | The two pages' init-queue lengths, the observable that says which
--   page a request landed on.
queueLengths ∷ WorldState → WorldState → IO (Int, Int)
queueLengths wsA wsB = do
    a ← length ⊚ readIORef (wsInitQueueRef wsA)
    b ← length ⊚ readIORef (wsInitQueueRef wsB)
    pure (a, b)

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "chunk work binds to its own page" $ do

    describe "world.loadChunksInRegion admission" $ do
        it "admits to the projected page while a show is unapplied" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            showBUnapplied env ls

            queued ← loadRegion ls ""
            queued `shouldBe` Just regionArea
            -- The regression: pre-#2310 this whole region landed on A.
            queueLengths wsA wsB
                `shouldReturn` (seededA, seededB + regionArea)

        it "admits to the applied page when nothing is outstanding" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            -- Requirement 4: the ~47 existing probe call sites, which
            -- pass no page and run with a settled selection, behave
            -- exactly as they always have.
            queued ← loadRegion ls ""
            queued `shouldBe` Just regionArea
            queueLengths wsA wsB
                `shouldReturn` (seededA + regionArea, seededB)

        it "admits to an explicitly named page even while it is hidden" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            -- B is live but NOT visible and NOT projected: naming it is
            -- the only way to reach it, which is the point of the
            -- argument.
            queued ← loadRegion ls (", " <> luaId pageB)
            queued `shouldBe` Just regionArea
            queueLengths wsA wsB
                `shouldReturn` (seededA, seededB + regionArea)

        it "queues nothing for an explicit page that is not registered" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            queued ← loadRegion ls (", " <> luaId pageMissing)
            -- Requirement 2: says so, and never falls back.
            queued `shouldBe` Just 0
            queueLengths wsA wsB `shouldReturn` (seededA, seededB)

        it "queues nothing when the projected page is not live yet" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            -- The construction-probe shape exactly: a page CREATED and
            -- SHOWN in one queued burst. #1602 projects the
            -- registration too, so the show is predicted effective and
            -- the projected head is the new page — but its WorldState
            -- does not exist until the world thread runs the init, so
            -- there is nothing to admit to yet. Falling back to the
            -- applied page here would be the defect, not the fix, which
            -- is why tools/construction_probe.py still waits for its
            -- arena to be APPLIED before designating.
            _ ← Lua.runWith (lbsLuaState ls) $ Lua.dostring
                ("world.initArena(" <> luaId pageMissing <> ") "
                 <> "world.show(" <> luaId pageMissing <> ")")
            mgr ← readIORef (worldManagerRef env)
            wmSelectionPending mgr `shouldSatisfy` (> 0)
            take 1 (wmProjectedVisible mgr) `shouldBe` [pageMissing]
            -- Projected as REGISTERED, but not live: that gap is the
            -- whole point of the example.
            wmProjectedWorlds mgr `shouldContain` [pageMissing]
            map fst (wmWorlds mgr) `shouldBe` [pageA, pageB]

            queued ← loadRegion ls ""
            queued `shouldBe` Just 0
            queueLengths wsA wsB `shouldReturn` (seededA, seededB)

        it "refuses a non-string page argument instead of coercing it" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            -- Lua.tostring coerces a number, so without a type check
            -- this would have become a request against a page named
            -- "7" — silently zero work, reported as if the caller had
            -- named a page.
            queued ← loadRegion ls ", 7"
            queued `shouldBe` Nothing
            queueLengths wsA wsB `shouldReturn` (seededA, seededB)

    describe "world.waitForChunks target" $ do
        it "watches the projected page while a show is unapplied" $ \env → do
            (_, _) ← installPages env
            ls ← newBackend env
            showBUnapplied env ls
            -- seededB, not seededA: the wait followed the page the
            -- admission above would have used, not the applied one.
            waitLua ls "1" `shouldReturn` Just seededB

        it "watches the applied page when nothing is outstanding" $ \env → do
            _ ← installPages env
            ls ← newBackend env
            waitLua ls "1" `shouldReturn` Just seededA

        it "watches an explicitly named hidden page" $ \env → do
            _ ← installPages env
            ls ← newBackend env
            waitLua ls ("1, " <> luaId pageB) `shouldReturn` Just seededB

        it "reports nothing remaining for an unregistered explicit page" $ \env → do
            _ ← installPages env
            ls ← newBackend env
            waitLua ls ("1, " <> luaId pageMissing) `shouldReturn` Just 0

        it "keeps watching the page it started on across a visibility change" $ \env → do
            (_, wsB) ← installPages env
            ls ← newBackend env
            captureOnce env wsB (waitLua ls ("2, " <> luaId pageB))

        it "reads a zero timeout as the default, not as 'answer now'" $ \env → do
            (_, wsB) ← installPages env
            ls ← newBackend env
            drainsWhileWaiting wsB (waitLua ls ("0, " <> luaId pageB))

    describe "the debug console's own fast path" $ do
        it "watches the projected page while a show is unapplied" $ \env → do
            _ ← installPages env
            ls ← newBackend env
            showBUnapplied env ls
            waitConsole env "1" `shouldReturn` Just seededB

        it "watches the applied page when nothing is outstanding" $ \env → do
            _ ← installPages env
            _ ← newBackend env
            waitConsole env "1" `shouldReturn` Just seededA

        it "accepts an explicit page, quoted either way" $ \env → do
            _ ← installPages env
            _ ← newBackend env
            waitConsole env "1, 'chunk_page_projected'" `shouldReturn` Just seededB
            waitConsole env "1, \"chunk_page_projected\"" `shouldReturn` Just seededB
            -- The timeout may be left to the default while still naming
            -- a page, and that spelling stays on the fast path.
            waitConsole env "nil, 'chunk_page_projected'" `shouldReturn` Just seededB

        it "reports nothing remaining for an unregistered explicit page" $ \env → do
            _ ← installPages env
            _ ← newBackend env
            waitConsole env "1, 'chunk_page_absent'" `shouldReturn` Just 0

        it "keeps watching the page it started on across a visibility change" $ \env → do
            (_, wsB) ← installPages env
            _ ← newBackend env
            captureOnce env wsB (waitConsole env "2, 'chunk_page_projected'")

        it "reads a zero timeout as the default, not as 'answer now'" $ \env → do
            (_, wsB) ← installPages env
            _ ← newBackend env
            -- The regression this guards: before the explicit-page forms
            -- were recognised here, world.waitForChunks(0, 'p') fell
            -- through to the Lua binding, where a non-positive timeout
            -- has always meant the 120-second default. Serving it on the
            -- fast path with a literal 0 would answer before the target
            -- queue drained -- the exact failure mode #2310 is about.
            drainsWhileWaiting wsB
                (waitConsole env "0, 'chunk_page_projected'")

        it "reads a zero timeout as the default with no page named either" $ \env → do
            (wsA, _) ← installPages env
            _ ← newBackend env
            -- Nothing outstanding, so the target is the applied page.
            drainsWhileWaiting wsA (waitConsole env "0")

        it "leaves an argument list it cannot parse to the Lua thread" $ \env → do
            _ ← installPages env
            _ ← newBackend env
            -- Not a decimal, not a plain quoted literal: the console
            -- must not guess. 'Nothing' means "fell through", which is
            -- where Lua's own parser handles it correctly.
            debugBuiltin env "return world.waitForChunks(t)" `shouldReturn` Nothing
            debugBuiltin env "return world.waitForChunks(1, pid)"
                `shouldReturn` Nothing

    describe "the shared resolution rule" $
        it "prefers an explicit page over both applied and projected" $ \env → do
            (wsA, wsB) ← installPages env
            ls ← newBackend env
            showBUnapplied env ls
            -- With B projected, A is reachable only by naming it — the
            -- mirror of the hidden-B case, proving the explicit branch
            -- overrides the projection rather than merely agreeing with
            -- it.
            explicitA ← chunkTargetWorld (Just pageA) env
            fst ⊚ explicitA `shouldBe` Just pageA
            queued ← loadRegion ls (", " <> luaId pageA)
            queued `shouldBe` Just regionArea
            queueLengths wsA wsB
                `shouldReturn` (seededA + regionArea, seededB)

-- | Drive one zero-timeout example: empty the target page's queue while
--   the wait is running, and require the wait to have OBSERVED that.
--
--   A wait that took the 0 literally returns the queue's current length
--   the moment it starts — 'seededA' or 'seededB', never 0 — because
--   nothing has drained yet. Returning 0 means it kept polling past the
--   drain, which is what the shared 'chunkWaitTimeoutSec' rule buys.
--
--   The drain lands at 300 ms, one poll interval in, and the default
--   timeout is 120 seconds, so the example finishes in about a third of
--   a second rather than sitting out a real timeout.
drainsWhileWaiting ∷ WorldState → IO (Maybe Int) → Expectation
drainsWhileWaiting ws runWait = do
    before ← length ⊚ readIORef (wsInitQueueRef ws)
    before `shouldSatisfy` (> 0)
    _ ← forkIO $ threadDelay 300000 ≫ writeIORef (wsInitQueueRef ws) []
    runWait `shouldReturn` Just 0

-- | Drive one capture-once example: start a wait against B, flip the
--   whole session onto a DIFFERENT live page while it is running, and
--   require the answer to still be B's.
--
--   The replacement page's queue is EMPTY, so a wait that re-resolved
--   per poll — what both paths did before #2310 — would see nothing
--   remaining and return 0 early. Returning 'seededB' means the wait ran
--   its full timeout against the state it captured at the start.
--
--   The swap lands well inside the wait: the wait cannot finish early
--   (B's queue never drains without a world worker), so it runs the full
--   two seconds, and the swap happens after 300 ms — a poll boundary is
--   250 ms.
captureOnce ∷ EngineEnv → WorldState → IO (Maybe Int) → Expectation
captureOnce env wsB runWait = do
    wsC ← emptyWorldState
    writeIORef (wsGenParamsRef wsC) (Just pageParams)
    _ ← forkIO $ do
        threadDelay 300000
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds           = [(pageA, wsC)]
            , wmVisible          = [pageA]
            , wmProjectedWorlds  = [pageA]
            , wmProjectedVisible = [pageA]
            }
    result ← runWait
    result `shouldBe` Just seededB
    -- The swap really did happen, so the assertion above is not passing
    -- because nothing moved.
    mgr ← readIORef (worldManagerRef env)
    map fst (wmWorlds mgr) `shouldBe` [pageA]
    length ⊚ readIORef (wsInitQueueRef wsC) `shouldReturn` (0 ∷ Int)
    -- B is untouched: the wait watched it, it never drains.
    length ⊚ readIORef (wsInitQueueRef wsB) `shouldReturn` seededB
