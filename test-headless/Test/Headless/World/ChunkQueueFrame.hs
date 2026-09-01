{-# LANGUAGE Strict #-}
-- | Chunks reach 'wsTilesRef' under CANONICAL (u-wrapped) keys, whichever
--   loader put them there — and the init QUEUE names each of them once,
--   whichever producer appended it.
--
--   That first invariant is load-bearing well outside this module —
--   'World.Render.ChunkLookup' and
--   'World.Generate.Coordinates.canonicalTileFrame' both state it, and
--   every canonicalising lookup built on them (the render passes, the
--   tile hit-test, building placement) silently resolves to the wrong
--   place without it. The camera-driven loader
--   ('World.Thread.ChunkLoading.updateChunkLoading') has always wrapped;
--   the INIT QUEUE did not, and it has four producers that can name a
--   coord outside the canonical range:
--
--     * @world.loadChunksInRegion@ — an arbitrary caller-supplied region;
--     * the dump path's @--region@ fill;
--     * "World.Load.Stage" — a radius around the SAVED camera chunk, so a
--       session saved near the seam restores one;
--     * world init — a radius around the origin.
--
--   A seam-crossing coord from any of them used to be GENERATED and
--   INSERTED raw, leaving the map holding two independently generated
--   chunks for one physical place. The observable symptom was a
--   canonicalising reader resolving a tile the raw loader had populated
--   and finding nothing there: @world.getSurfaceAt@ answered at the raw
--   alias and @null@ at its canonical twin. Wrapping in the CONSUMER
--   closed that.
--
--   What it did not close (#1723) is the queue's own bookkeeping: the
--   producers still measured in raw coordinate space, so two aliases of
--   one physical chunk were counted twice, appended twice, and each
--   consumed a batch slot ahead of real work — inflating
--   @world.loadChunksInRegion@'s return, @world.waitForChunks@'s
--   remaining, 'LoadPhase2' progress and the dump's
--   @dump: queued N chunks@ line. Every producer now measures through
--   the ONE identity in "World.Chunk.Queue", and the consumer
--   canonicalises through that same identity.
module Test.Headless.World.ChunkQueueFrame (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Engine.Core.State (EngineEnv)
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Chunk.Queue
    (canonicalChunkCoord, dedupChunkQueue, enqueueChunkRequest, initialChunkQueue)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Command.Types (WorldCommand(..))
import World.State.Types (WorldState(..), emptyWorldState)
import World.Tile.Types (WorldTileData(..))
import Test.Headless.Harness
    (getWorldTileData, queueChunks, sendWorldCommand, waitForChunksAt,
     waitForWorldInit)

-- | A deliberately small world so this spec's own generation is cheap:
--   8 chunks around u means the canonical range is u ∈ [-4, 4), and
--   chunk (4, 0) — u = 4 — is the first coord past it.
seamWorldSize ∷ Int
seamWorldSize = 8

-- | A world big enough that nothing in a load-radius box aliases:
--   the u-shift is ±32 and the box spans u ∈ [-4, 4].
wideWorldSize ∷ Int
wideWorldSize = 64

page, dedupPage, detachedPageId ∷ WorldPageId
page      = WorldPageId "chunk_queue_frame"
dedupPage = WorldPageId "chunk_queue_dedup"
-- | The id the DETACHED-page examples request under. A chunk key is
--   page-qualified (#2001), so even a page with no engine behind it has
--   to name one; these pages are never registered, so the id is theirs
--   alone.
detachedPageId = WorldPageId "chunk_queue_detached"

-- | The alias a caller might hand the queue, and the key the chunk must
--   actually land under. (4,0): u = 4, v = 4 → wraps to u = -4 → (0,4).
aliasCoord, canonCoord ∷ ChunkCoord
aliasCoord = ChunkCoord 4 0
canonCoord = ChunkCoord 0 4

-- | Params for a page of the given size that is NOT an arena
--   ('World.Generate.Types.isArenaParams' keys off seed 0 + an empty
--   timeline, and 'defaultWorldGenParams' seeds 42).
sizedParams ∷ Int → WorldGenParams
sizedParams size = defaultWorldGenParams { wgpWorldSize = size }

-- | A detached page: a 'WorldState' that no thread owns, so its queue
--   stays exactly as a producer left it. The live engine's world thread
--   drains any page it can see within a tick, which is the whole reason
--   the pending-alias cases below cannot be observed on a real page.
detachedPage ∷ Maybe WorldGenParams → IO WorldState
detachedPage mParams = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) mParams
    pure ws

-- | The load-radius box a producer starts from, before any dedup.
rawInitialBox ∷ ChunkCoord → [ChunkCoord]
rawInitialBox (ChunkCoord ccx ccy) =
    [ ChunkCoord cx cy
    | cx ← [ccx - chunkLoadRadius .. ccx + chunkLoadRadius]
    , cy ← [ccy - chunkLoadRadius .. ccy + chunkLoadRadius]
    ]

spec ∷ SpecWith EngineEnv
spec = describe "init-queue chunks land under canonical keys" $ do
    it "wraps a seam-crossing queued coord instead of storing it raw" $ \env → do
        -- A PRIVATE page: this mutates its own tile map, so it must not
        -- share the suite's read-only world.
        sendWorldCommand env (WorldInit page 42 seamWorldSize 3 Nothing)
        ws ← waitForWorldInit env page 300

        -- Precondition: the fixture really names an out-of-range coord,
        -- or every assertion below would hold vacuously.
        wrapChunkCoordU seamWorldSize aliasCoord `shouldBe` canonCoord
        aliasCoord `shouldNotBe` canonCoord

        -- queueChunks is the harness analogue of the unwrapped append
        -- world.loadChunksInRegion performs.
        queueChunks page ws [aliasCoord]
        loaded ← waitForChunksAt ws canonCoord 120
        loaded `shouldBe` True

        td ← getWorldTileData ws
        -- The regression: the chunk used to appear under the raw alias,
        -- as a SECOND chunk for the same physical place.
        HM.member aliasCoord (wtdChunks td) `shouldBe` False

        -- ...and the general invariant every canonicalising lookup
        -- relies on, checked across whatever else this page has loaded.
        let nonCanonical =
                [ c | c ← HM.keys (wtdChunks td)
                    , wrapChunkCoordU seamWorldSize c ≢ c ]
        nonCanonical `shouldBe` []

    it "reads a page's identity from its OWN size, and only where there is a seam" $ \_ → do
        -- Two live pages can have different sizes at once, so the
        -- identity is per-page, never a global.
        let narrow = canonicalChunkCoord (sizedParams seamWorldSize)
            wide   = canonicalChunkCoord (sizedParams wideWorldSize)
        narrow aliasCoord `shouldBe` canonCoord
        wide aliasCoord `shouldBe` aliasCoord

        -- An arena's wgpWorldSize is a 100000 sentinel, not an extent,
        -- so it is recognised by isArenaParams rather than handed to
        -- wrapChunkCoordU — which would wrap a coord past u = ±50000.
        let arena = canonicalChunkCoord
                (defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 100000 })
            far = ChunkCoord 60000 0
        wrapChunkCoordU 100000 far `shouldNotBe` far
        arena far `shouldBe` far
        arena (ChunkCoord 2 (-2)) `shouldBe` ChunkCoord 2 (-2)

        -- A sizeless page has no seam either.
        canonicalChunkCoord (sizedParams 0) aliasCoord `shouldBe` aliasCoord

    it "keeps the first spelling and the queue order when deduping" $ \_ → do
        let canon = canonicalChunkCoord (sizedParams seamWorldSize)
            twins = [ChunkCoord 4 0, ChunkCoord 1 1, ChunkCoord 0 4]
        -- drainInitQueues takes the FRONT of the queue and drops a
        -- drained batch BY COORD, so the surviving spelling must be the
        -- first one seen, in place.
        dedupChunkQueue canon twins `shouldBe` [ChunkCoord 4 0, ChunkCoord 1 1]

        -- A request with no aliases is returned untouched — order and
        -- spellings both.
        let straight = [ChunkCoord 1 1, ChunkCoord 0 0, ChunkCoord (-1) 2]
        dedupChunkQueue canon straight `shouldBe` straight
        dedupChunkQueue (canonicalChunkCoord (sizedParams wideWorldSize)) twins
            `shouldBe` twins

    it "seeds fresh init and saved-page restore with unique physical chunks" $ \_ → do
        -- Both producers go through initialChunkQueue, so this covers
        -- world init (centre at the origin) and a save restored near
        -- the seam (centre at the saved camera chunk) alike.
        let narrow = canonicalChunkCoord (sizedParams seamWorldSize)
            wide   = canonicalChunkCoord (sizedParams wideWorldSize)
            box    = rawInitialBox (ChunkCoord 0 0)
            (narrowQueue, narrowTotal) = initialChunkQueue narrow (ChunkCoord 0 0)
            (wideQueue, wideTotal)     = initialChunkQueue wide (ChunkCoord 0 0)

        -- On a size-8 world the box aliases against itself: u = 4 and
        -- u = -4 are one physical column, so (2,-2) and (-2,2) are one
        -- chunk and the raw (2r+1)^2 total over-counts.
        narrow (ChunkCoord 2 (-2)) `shouldBe` ChunkCoord (-2) 2
        length box `shouldBe` 25
        narrowTotal `shouldBe` 24
        length narrowQueue `shouldBe` 23

        -- Where nothing aliases the queue is the box minus the centre,
        -- unchanged and in order — the pre-#1723 behaviour exactly.
        wideTotal `shouldBe` 25
        wideQueue `shouldBe` filter (≢ ChunkCoord 0 0) box

        -- The queue is the total minus the synchronously generated
        -- centre, counted exactly once, and holds no two aliases.
        narrowTotal `shouldBe` length narrowQueue + 1
        HS.size (HS.fromList (map narrow narrowQueue))
            `shouldBe` length narrowQueue
        map narrow narrowQueue `shouldNotContain` [narrow (ChunkCoord 0 0)]

        -- A restore centred off the origin near the seam: same shape,
        -- same aliasing, centre still counted once.
        let restored = ChunkCoord 3 (-1)
            (restoredQueue, restoredTotal) = initialChunkQueue narrow restored
        restoredTotal `shouldBe` length restoredQueue + 1
        HS.size (HS.fromList (map narrow restoredQueue))
            `shouldBe` length restoredQueue
        map narrow restoredQueue `shouldNotContain` [narrow restored]
        restoredTotal `shouldSatisfy` (< 25)

    it "counts a request in physical chunks, not coordinate spellings" $ \_ → do
        -- A detached page, so an appended coord really does stay pending
        -- for the duration of the example.
        ws ← detachedPage (Just (sizedParams seamWorldSize))

        -- Two aliases in ONE request are one chunk: counted once,
        -- appended once, under the spelling the caller asked with.
        queued ← enqueueChunkRequest detachedPageId ws [aliasCoord, canonCoord]
        queued `shouldBe` 1
        readIORef (wsInitQueueRef ws) `shouldReturn` [aliasCoord]

        -- A later request naming the OTHER alias of a pending chunk is
        -- already-queued work: nothing to add, nothing to report.
        again ← enqueueChunkRequest detachedPageId ws [canonCoord]
        again `shouldBe` 0
        readIORef (wsInitQueueRef ws) `shouldReturn` [aliasCoord]

        -- A genuinely new chunk still appends, at the back.
        more ← enqueueChunkRequest detachedPageId ws [ChunkCoord 1 1, aliasCoord]
        more `shouldBe` 1
        readIORef (wsInitQueueRef ws)
            `shouldReturn` [aliasCoord, ChunkCoord 1 1]

        -- The same two coords on a page WITHOUT a seam between them are
        -- two chunks — the identity is the page's own.
        wide ← detachedPage (Just (sizedParams wideWorldSize))
        wideQueued ← enqueueChunkRequest detachedPageId wide [aliasCoord, canonCoord]
        wideQueued `shouldBe` 2
        readIORef (wsInitQueueRef wide)
            `shouldReturn` [aliasCoord, canonCoord]

    it "queues nothing for a page whose generation params are missing" $ \_ → do
        -- No params means no physical identity, so no alias could be
        -- recognised — and drainInitQueues refuses to drain such a page
        -- anyway, so an append would sit there inflating every
        -- remaining count that reads the queue.
        ws ← detachedPage Nothing
        enqueueChunkRequest detachedPageId ws [aliasCoord, canonCoord] `shouldReturn` 0
        readIORef (wsInitQueueRef ws) `shouldReturn` []

    it "reports a live seam-crossing request as the work it actually queues" $ \env → do
        -- A second private page, driven through the real engine: the
        -- queue is empty and this page is not visible, so nothing else
        -- loads chunks for it and every count below is deterministic.
        sendWorldCommand env (WorldInit dedupPage 42 seamWorldSize 3 Nothing)
        ws ← waitForWorldInit env dedupPage 300
        let canon = canonicalChunkCoord (sizedParams seamWorldSize)

        -- Init loaded the unique PHYSICAL chunks of its box — the total
        -- LoadPhase2 was progressing towards — not 25 raw coords.
        td ← getWorldTileData ws
        HM.size (wtdChunks td)
            `shouldBe` snd (initialChunkQueue canon (ChunkCoord 0 0))
        readIORef (wsInitQueueRef ws) `shouldReturn` []

        -- An alias of an already-LOADED chunk is not work.
        let loadedKey    = ChunkCoord (-2) 2
            aliasOfLoaded = ChunkCoord 2 (-2)
        canon aliasOfLoaded `shouldBe` loadedKey
        HM.member loadedKey (wtdChunks td) `shouldBe` True
        HM.member aliasOfLoaded (wtdChunks td) `shouldBe` False
        enqueueChunkRequest dedupPage ws [aliasOfLoaded] `shouldReturn` 0
        readIORef (wsInitQueueRef ws) `shouldReturn` []

        -- The dump path's --region fill goes through this same call, so
        -- this is its "dump: queued N chunks" line: a seam-crossing
        -- rectangle, x-major exactly as App.Cli.chunkRegionCoords walks
        -- it. u spans [0..8] here, so the rectangle aliases against
        -- itself as well as against the loaded centre.
        let region = [ ChunkCoord cx cy | cx ← [0 .. 4], cy ← [-4 .. 0] ]
            loadedNow = wtdChunks td
            rawCount = length
                [ c | c ← region, not (HM.member c loadedNow) ]
            physicalCount = length
                [ c | c ← dedupChunkQueue canon region
                    , not (HM.member (canon c) loadedNow) ]
        -- Non-vacuity: the raw walk really would over-report here.
        physicalCount `shouldSatisfy` (< rawCount)
        enqueueChunkRequest dedupPage ws region `shouldReturn` physicalCount
