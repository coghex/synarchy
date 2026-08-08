{-# LANGUAGE Strict #-}
-- | Chunks reach 'wsTilesRef' under CANONICAL (u-wrapped) keys, whichever
--   loader put them there.
--
--   That invariant is load-bearing well outside this module —
--   'World.Render.ChunkLookup' and
--   'World.Generate.Coordinates.canonicalTileFrame' both state it, and
--   every canonicalising lookup built on them (the render passes, the
--   tile hit-test, building placement) silently resolves to the wrong
--   place without it. The camera-driven loader
--   ('World.Thread.ChunkLoading.updateChunkLoading') has always wrapped;
--   the INIT QUEUE did not, and it has three producers that can name a
--   coord outside the canonical range:
--
--     * @world.loadChunksInRegion@ — an arbitrary caller-supplied region;
--     * "World.Load.Stage" — a radius around the SAVED camera chunk, so a
--       session saved near the seam restores one;
--     * world init — a radius around the origin (interior in practice,
--       but unwrapped all the same).
--
--   A seam-crossing coord from any of them used to be GENERATED and
--   INSERTED raw, leaving the map holding two independently generated
--   chunks for one physical place. The observable symptom was a
--   canonicalising reader resolving a tile the raw loader had populated
--   and finding nothing there: @world.getSurfaceAt@ answered at the raw
--   alias and @null@ at its canonical twin.
module Test.Headless.World.ChunkQueueFrame (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Engine.Core.State (EngineEnv)
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Page.Types (WorldPageId(..))
import World.Command.Types (WorldCommand(..))
import World.Tile.Types (WorldTileData(..))
import Test.Headless.Harness
    (getWorldTileData, queueChunks, sendWorldCommand, waitForChunksAt,
     waitForWorldInit)

-- | A deliberately small world so this spec's own generation is cheap:
--   8 chunks around u means the canonical range is u ∈ [-4, 4), and
--   chunk (4, 0) — u = 4 — is the first coord past it.
seamWorldSize ∷ Int
seamWorldSize = 8

page ∷ WorldPageId
page = WorldPageId "chunk_queue_frame"

-- | The alias a caller might hand the queue, and the key the chunk must
--   actually land under. (4,0): u = 4, v = 4 → wraps to u = -4 → (0,4).
aliasCoord, canonCoord ∷ ChunkCoord
aliasCoord = ChunkCoord 4 0
canonCoord = ChunkCoord 0 4

spec ∷ SpecWith EngineEnv
spec = describe "init-queue chunks land under canonical keys" $
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
        queueChunks ws [aliasCoord]
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
                    , wrapChunkCoordU seamWorldSize c ≠ c ]
        nonCanonical `shouldBe` []
