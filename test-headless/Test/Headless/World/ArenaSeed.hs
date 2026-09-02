-- | Arena base seeding (#1718).
--
--   An arena page's base tile grid is never persisted: the save stores
--   generation parameters plus the edit overlay, so every surface tile
--   an edit never touched is RECONSTRUCTED on load from the page's
--   recorded seed. That only works if the base a fresh arena is built
--   from is the base its recorded seed produces, and if the load path
--   reads that seed rather than a constant of its own.
--
--   That reconstruction claim belongs to 'engineSpec' and to the
--   end-to-end probe below, and to nothing else here. 'pureSpec' covers
--   a different axis: it says nothing about which seed either caller
--   passes, and nothing about a seed rebuilding its own base.
--
--   * 'engineSpec' pins the fresh-creation WIRING against a live
--     headless engine: the chunk map 'handleWorldInitArenaCommand'
--     actually wrote must equal the one rebuilt from the @wgpSeed@ that
--     same command recorded — exactly the reconstruction the load path
--     performs, and the one an ambient 'newStdGen' fails.
--   * 'pureSpec' pins what a seed is ALLOWED to vary: two different
--     pinned seeds must disagree about surface vegetation, and about
--     nothing else — coordinates, surface maps, column starts,
--     materials and slopes are seed-blind, and vegetation reaches only
--     the top tile.
--
--   'pureSpec' pins no vegetation VALUES, by policy. Grass-variant
--   placement is not a cross-revision compatibility surface: a later
--   build may map the same arena seed to different variants, so an
--   arena autosave loaded by that build may render untouched tiles
--   differently. Only within one build must a seed reconstruct its own
--   base. That declines to freeze the generated vector and nothing
--   more — save decoding, the persisted edit overlay, arena topology
--   and material compatibility are all unaffected.
--
--   The end-to-end half (a real save, a fresh process, a real load)
--   lives in @tools/multiworld_save_probe.py --arena@, which compares an
--   untouched chunk's whole 256-position vegetation vector across the
--   round trip.
module Test.Headless.World.ArenaSeed (pureSpec, engineSpec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import Engine.Core.State (EngineEnv)
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Types
import World.Generate.Arena (arenaGenForSeed, generateArenaChunks)

-- | Every column's TOP tile vegetation id, in column order — the 256
--   values one arena chunk's surface actually renders, and the whole of
--   what the arena's generator varies.
surfaceVeg ∷ LoadedChunk → [Word8]
surfaceVeg lc = [ VU.last (ctVeg col) | col ← V.toList (lcTiles lc) ]

-- | The canonical arena seed. 'isArenaParams' recognises an arena by an
--   empty geological timeline and @wgpSeed ≡ 0@, so this is not a free
--   choice: it is the value both construction paths must build from.
arenaSeed ∷ Word64
arenaSeed = 0

-- | A second, deliberately different pinned seed. Nothing in the engine
--   uses it; it exists so the different-seeds assertion is a real
--   comparison rather than a tautology.
otherSeed ∷ Word64
otherSeed = 20260827

pureSpec ∷ Spec
pureSpec =
    describe "arena seed sensitivity and seed-blind topology" $ do
        it "two different pinned seeds disagree about surface vegetation" $
            -- Not merely "the bases differ": the difference must be in
            -- the surface vegetation, which is the only thing the seed
            -- is allowed to vary.
            map surfaceVeg (generateArenaChunks (arenaGenForSeed arenaSeed))
                `shouldNotBe`
                map surfaceVeg (generateArenaChunks (arenaGenForSeed otherSeed))

        it "and disagree about nothing else — the topology is seed-blind" $ do
            let a = generateArenaChunks (arenaGenForSeed arenaSeed)
                b = generateArenaChunks (arenaGenForSeed otherSeed)
                columns cs = [ col | c ← cs, col ← V.toList (lcTiles c) ]
            map lcCoord a `shouldBe` map lcCoord b
            map lcSurfaceMap a `shouldBe` map lcSurfaceMap b
            map lcTerrainSurfaceMap a `shouldBe` map lcTerrainSurfaceMap b
            map ctStartZ (columns a) `shouldBe` map ctStartZ (columns b)
            map ctMats   (columns a) `shouldBe` map ctMats   (columns b)
            map ctSlopes (columns a) `shouldBe` map ctSlopes (columns b)

        it "vegetation sits on the top tile only, 256 columns per chunk" $ do
            let chunks = generateArenaChunks (arenaGenForSeed arenaSeed)
                columns = [ col | c ← chunks, col ← V.toList (lcTiles c) ]
            map (length . surfaceVeg) chunks
                `shouldBe` replicate (length chunks) 256
            all (VU.all (≡ 0) . VU.init . ctVeg) columns `shouldBe` True

engineSpec ∷ SpecWith EngineEnv
engineSpec =
    describe "a freshly created arena page is generated from the seed it \
             \records" $
        it "the chunk map world init wrote equals the one rebuilt from \
           \that page's own wgpSeed" $ \env → do
            let pid = WorldPageId "id_arena_seed_1718"
            sendWorldCommand env (WorldInitArena pid)
            ws ← waitForWorldInit env pid 60

            mParams ← readIORef (wsGenParamsRef ws)
            params ← case mParams of
                Nothing → expectationFailure
                    "a fresh arena page recorded no WorldGenParams"
                    ≫ error "unreachable"
                Just p  → pure p

            -- The persisted arena seed stays zero, because that is half
            -- of how a loaded page is RECOGNISED as an arena at all.
            wgpSeed params `shouldBe` arenaSeed
            isArenaParams params `shouldBe` True

            tiles ← readIORef (wsTilesRef ws)
            let rebuilt = HM.fromList
                    [ (lcCoord c, c)
                    | c ← generateArenaChunks
                              (arenaGenForSeed (wgpSeed params)) ]

            HM.keys (wtdChunks tiles) `shouldMatchList` HM.keys rebuilt
            -- The whole base, not a sampled tile: with four vegetation
            -- variants a single coordinate agrees by chance 1 time in 4.
            wtdChunks tiles `shouldBe` rebuilt
