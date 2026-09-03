{-# LANGUAGE OverloadedStrings #-}
-- | Flora registration and placement are independent of enumeration
--   order (#2241, FSI-1 of epic #2236).
--
--   Four things are pinned here, because four different mutations can
--   put OS-dependent enumeration back into a shipped world:
--
--   * two OPPOSING registration orders over the same shipped species
--     produce name-normalized-identical generated instances — the whole
--     record, not just which species appeared;
--   * an IMPOSSIBLE-FIT species that sorts lexically first, shifts every
--     other species' catalog position and never occupies a tile changes
--     nothing at all. This is the case a same-species-set order swap
--     cannot see: it is what fails when a roll or an instance salt goes
--     back to being indexed off a list position;
--   * a checked-in GOLDEN layout, generated from the real shipped
--     catalog at seed 42 / world size 64, which must be byte-identical
--     on macOS and Linux; and
--   * a pre-change save's numeric 'FloraId' references, which this
--     change deliberately reinterprets ONCE, resolving to explicitly
--     named post-change species rather than merely to \"some species
--     that exists\".
--
--   What is deliberately NOT asserted is final-layout invariance when a
--   species that ACTUALLY PLACES is added or removed: flora share one
--   occupancy map and 'World.Flora.Placement' lets an earlier placement
--   suppress a later candidate. That ecological competition is intended
--   (#2241 requirement 2).
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "World.FloraOrder"'@
--
--   == Re-capturing the checked-in fixtures
--
--   Both fixtures are mechanically re-capturable, so a DELIBERATE
--   future change to flora content or placement is a re-run rather than
--   a hand transcription:
--
--   > SYNARCHY_FLORA_GOLDEN_CAPTURE=1 cabal test synarchy-test-headless \
--   >     --test-options='--match "World.FloraOrder"'
--
--   rewrites 'goldenPath' from the live engine and then asserts against
--   what it wrote; @SYNARCHY_FLORA_LEGACY_CAPTURE=1@ does the same for
--   'legacyPath'. Neither variable is set in CI, and with neither set
--   both files are read-only inputs. Re-capturing the GOLDEN is a
--   worldgen-output change; re-capturing the LEGACY fixture is not, and
--   should essentially never happen — its numbers are a historical
--   record (see 'preChangeCatalog').
module Test.Headless.World.FloraOrder (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Char (digitToInt, intToDigit, isHexDigit)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory, takeExtension)

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)

import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Flora.Placement (computeChunkFlora)
import World.Flora.Types
import World.Fluid.Types (FluidCell)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Save.Component.Page
import World.Weather.Types
    ( ClimateCoord(..), ClimateGrid(..), ClimateState(..)
    , RegionClimate(..), SeasonalClimate(..), defaultRegionClimate )

-- * Fixture geography

goldenSeed ∷ Word64
goldenSeed = 42

goldenWorldSize ∷ Int
goldenWorldSize = 64

goldenPageKey ∷ Text
goldenPageKey = "flora_order_golden"

-- | The chunks the golden covers. Four, spread far apart across the
--   64-chunk page, rather than one corner of it: a climate region is
--   four chunks on a side and 'World.Weather.Lookup.lookupLocalClimate'
--   interpolates between region centres, so adjacent chunks would
--   sample one climate and place essentially one species.
goldenChunks ∷ [ChunkCoord]
goldenChunks =
    [ ChunkCoord 0 0, ChunkCoord 7 11, ChunkCoord 19 5, ChunkCoord 31 27 ]

goldenPath ∷ FilePath
goldenPath = "test-headless" </> "data" </> "flora-order"
                             </> "seed42-w64-golden.txt"

legacyPath ∷ FilePath
legacyPath = "test-headless" </> "data" </> "flora-order"
                             </> "pre-canonical-refs.txt"

-- | Flat ground, no slope, no water — the terrain is held constant on
--   purpose so every difference the examples below can see comes from
--   the CATALOG.
flatSurface ∷ VU.Vector Int
flatSurface = VU.replicate (chunkSize * chunkSize) 8

-- | Loam (@data\/materials\/soils_mineral.yaml@ id 56). A real SOIL,
--   not an arbitrary number: 'World.Vegetation.isBarrenMaterial' rejects
--   every rock id outright, and the four soil-constrained shipped
--   species name loam among the soils they accept, so this is a
--   material on which the shipped catalog genuinely competes.
flatMats ∷ VU.Vector Word8
flatMats = VU.replicate (chunkSize * chunkSize) 56

flatSlopes ∷ VU.Vector Word8
flatSlopes = VU.replicate (chunkSize * chunkSize) 0

noFluid ∷ V.Vector (Maybe FluidCell)
noFluid = V.replicate (chunkSize * chunkSize) Nothing

-- | A synthetic but VARYING climate grid, built here rather than taken
--   from a generated world.
--
--   'defaultWorldGenParams' carries an EMPTY grid, which interpolates to
--   0°C / 0 precipitation everywhere and hard-kills every shipped
--   species before a single roll is consulted — a fixture on which any
--   placement bug at all is invisible. Running real worldgen instead
--   would cost the fast headless tier a ~10 s w64 generation and drag
--   the whole terrain pipeline's floating-point surface into a golden
--   that is only about flora.
--
--   So: 16×16 regions (@worldSize \`div\` climateRegionSize@), each given
--   a temperature, precipitation and humidity that vary with its
--   position. Every value is a dyadic fraction, exactly representable
--   in 'Float', so the bilinear interpolation feeding
--   'World.Flora.Placement.speciesFitness' introduces no rounding of its
--   own.
goldenClimate ∷ ClimateState
goldenClimate = base
    { csClimate = ClimateGrid
        { cgSize = regions
        , cgRegions = HM.fromList
            [ (ClimateCoord ru rv, regionAt ru rv)
            | ru ← [0 .. regions - 1], rv ← [0 .. regions - 1] ] } }
  where
    base = wgpClimateState defaultWorldGenParams
    regions = goldenWorldSize `div` 4
    regionAt ru rv = defaultRegionClimate
        { rcAirTemp = SeasonalClimate t t
        , rcPrecipitation = SeasonalClimate pr pr
        , rcHumidity = hu
        }
      where
        t  = 2.0 + fromIntegral (ru `mod` 8) * 3.0          -- 2 .. 23 °C
        pr = 0.25 + fromIntegral (rv `mod` 4) * 0.125       -- 0.25 .. 0.625
        hu = 0.375 + fromIntegral ((ru + rv) `mod` 3) * 0.125

-- * Catalog plumbing

-- | One catalog entry as the loader saw it, keyed by the authored name
--   that #2241 makes the stable species key.
data Entry = Entry
    { enName    ∷ !Text
    , enSpecies ∷ !FloraSpecies
    , enWorldGen ∷ !(Maybe FloraWorldGen)
    } deriving (Show, Eq)

-- | Every species in a catalog with its world-gen entry, in authored
--   name order.
catalogEntries ∷ FloraCatalog → [Entry]
catalogEntries cat = L.sortOn enName
    [ Entry (fsName sp) sp (HM.lookup k (fcWorldGen cat))
    | (k, sp) ← HM.toList (fcSpecies cat) ]

-- | Rebuild a catalog by registering @entries@ in exactly the order
--   given, through the SAME 'nextFloraId' allocator the YAML loader
--   uses. Two different orders therefore hand the same species
--   different numeric ids — which is the whole point.
reindexCatalog ∷ [Entry] → FloraCatalog
reindexCatalog = foldl' step emptyFloraCatalog
  where
    step cat e =
        let (fid, cat') = nextFloraId cat
            cat'' = insertSpecies fid (enSpecies e) cat'
        in maybe cat'' (\wg → insertWorldGen fid wg cat'') (enWorldGen e)

-- | A generated instance with its session-local 'fiSpecies' resolved to
--   the authored NAME, and every other observable kept verbatim.
--
--   Requirement 3 wants the COMPLETE record compared, not just the
--   species: a residual positional salt moves offsets, variant and age
--   long before it moves which species placed where.
data NamedInstance = NamedInstance
    { niName     ∷ !Text
    , niInstance ∷ !FloraInstance
    } deriving (Show, Eq)

nameInstances ∷ FloraCatalog → [FloraInstance] → [NamedInstance]
nameInstances cat = map $ \fi → NamedInstance
    { niName = maybe "<unregistered>" fsName (lookupSpecies (fiSpecies fi) cat)
      -- fiSpecies is the one field that legitimately differs between
      -- two registration orders; blanked so the rest of the record is
      -- compared as-is.
    , niInstance = fi { fiSpecies = FloraId 0 }
    }

placeChunk ∷ FloraCatalog → ChunkCoord → [NamedInstance]
placeChunk cat coord = nameInstances cat ∘ fcdInstances $
    computeChunkFlora goldenPageKey goldenSeed goldenWorldSize coord
        flatSurface flatMats flatSlopes noFluid goldenClimate cat

placeAll ∷ FloraCatalog → [(ChunkCoord, [NamedInstance])]
placeAll cat = [ (c, placeChunk cat c) | c ← goldenChunks ]

-- | A species nothing can ever grow: its temperature window sits far
--   outside any climate the generator produces, so 'speciesFitness'
--   hard-kills it before any roll is consulted. Named to sort FIRST in
--   authored-name order, so registering it moves every other species'
--   catalog position AND its position in 'worldGenSpecies'.
impossibleEntry ∷ Entry
impossibleEntry = Entry name species (Just wg)
  where
    name = "aaaa_impossible_fit_probe"
    species = newFloraSpecies name (TextureHandle 0)
    wg = FloraWorldGen
        { fwCategory      = "groundcover"
        , fwMinTemp       = 9000.0
        , fwMaxTemp       = 9500.0
        , fwIdealTemp     = 9250.0
        , fwMinPrecip     = 0.0
        , fwMaxPrecip     = 1.0
        , fwIdealPrecip   = 0.5
        , fwMinAlt        = -100
        , fwMaxAlt        = 800
        , fwIdealAlt      = 350
        , fwMinHumidity   = 0.0
        , fwMaxHumidity   = 1.0
        , fwIdealHumidity = 0.5
        , fwMaxSlope      = 15
        , fwDensity       = 1.0
        , fwSoils         = []
        , fwFootprint     = 0.0
        }

-- * The shipped catalog

-- | Load @data\/materials@ and then @data\/flora@ into a PRIVATE
--   headless engine through the real @engine.load*Yaml@ bindings, in
--   the byte order @scripts\/startup_loader.lua@ now uses, and hand back
--   the catalog that produced.
--
--   Materials first, exactly as @queueNormalProfile@ orders them: a
--   flora species' @soils@ list is resolved against the material
--   registry at registration time, and an unpopulated registry would
--   silently drop every soil constraint — which would make the golden a
--   layout production never generates.
--
--   PRIVATE because @engine.loadFloraYaml@ mutates shared engine state
--   (the catalog, the asset pool, the texture-name registry) in ways no
--   @finally@ can cleanly undo; the same reasoning
--   "Test.Headless.Asset.TextureFallback" records.
loadShippedCatalog ∷ IO FloraCatalog
loadShippedCatalog = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loadFamily ls "data/materials" "loadMaterialYaml"
    loadFamily ls "data/flora"     "loadFloraYaml"
    readIORef (floraCatalogRef env)
  where
    loadFamily ls dir verb = do
        files ← L.sort ∘ filter ((≡ ".yaml") ∘ takeExtension)
                    <$> listDirectory dir
        forM_ files $ \f → do
            _ ← executeDebugLua (lbsLuaState ls)
                    ("return engine." <> verb <> "('"
                     <> T.pack dir <> "/" <> T.pack f <> "')")
            pure ()

-- * The golden

-- | One line per placed plant: chunk, local tile, species name. Sorted,
--   so the file is a stable rendering of a set rather than of a walk
--   order — the walk order is 'World.Flora.Placement''s own business
--   and is pinned by the order-equivalence examples above.
renderGolden ∷ [(ChunkCoord, [NamedInstance])] → Text
renderGolden chunks = T.unlines $ header ⧺ map render (L.sort rows)
  where
    rows = [ (cx, cy, fiTileX (niInstance ni), fiTileY (niInstance ni)
             , niName ni)
           | (ChunkCoord cx cy, nis) ← chunks, ni ← nis ]
    render (cx, cy, tx, ty, name) =
        T.unwords [tshow cx, tshow cy, tshow tx, tshow ty, name]
    header =
        [ "# Flora placement golden (#2241 requirement 5)."
        , "# seed=42 worldSize=64 page=" <> goldenPageKey
        , "# chunks=" <> T.intercalate ","
            [ tshow cx <> "/" <> tshow cy | ChunkCoord cx cy ← goldenChunks ]
        , "# Columns: chunkX chunkY tileX tileY speciesName"
        , "# Re-capture: SYNARCHY_FLORA_GOLDEN_CAPTURE=1 cabal test \
          \synarchy-test-headless --test-options='--match \"World.FloraOrder\"'"
        ]

-- * The pre-change legacy references

-- | What each numeric 'FloraId' in 'legacyPath' MEANT in the catalog
--   that produced it: a macOS build before #2241, whose @data\/flora@
--   enumeration began with @temperate_deciduous.yaml@ rather than with
--   @boreal_evergreen.yaml@.
--
--   Recorded here as prose-with-teeth. The point of the example below
--   is precisely that these are NOT what the same numbers mean now:
--   canonical registration reinterprets them once, deliberately
--   (#2241 requirement 6), and #2243 is what stops new saves carrying
--   numbers at all.
preChangeCatalog ∷ [(Word16, Text)]
preChangeCatalog =
    [ (1,  "white_oak")       -- temperate_deciduous.yaml, first enumerated
    , (3,  "weeping_willow")
    , (14, "tomato_plant")    -- crops.yaml, sixth file enumerated
    ]

-- | The post-change meaning of those same three numbers under the
--   canonical byte-ordered catalog: @boreal_evergreen@, then @crops@,
--   then @saguaro@, then @temperate_deciduous@ …
postChangeExpectation ∷ [(Word16, Text)]
postChangeExpectation =
    [ (1,  "scots_pine")
    , (3,  "tomato_plant")
    , (14, "coconut_palm")
    ]

-- | The three persisted references the fixture carries, at the numbers
--   a pre-change save recorded.
legacyEdit ∷ WorldEditDTO
legacyEdit = WePlaceFloraD 12 34 (FloraId 14) 8 1.0

legacyPlot ∷ CropPlotDTO
legacyPlot = CropPlotDTO (FloraId 3) 120 0.75

legacyDesignation ∷ PlantDesignationDTO
legacyDesignation = PlantDesignationDTO 8 (FloraId 1)

renderLegacy ∷ Text
renderLegacy = T.unlines
    [ "# A pre-#2241 save's three persisted numeric FloraId references."
    , "# Minted from a macOS build whose data/flora enumeration began"
    , "# with temperate_deciduous.yaml, where:"
    , "#   1 = white_oak, 3 = weeping_willow, 14 = tomato_plant"
    , "# Decoded, never re-encoded, by Test.Headless.World.FloraOrder."
    , "edit " <> hex legacyEdit
    , "plot " <> hex legacyPlot
    , "designation " <> hex legacyDesignation
    ]
  where hex ∷ S.Serialize α ⇒ α → Text
        hex = toHex ∘ S.encode

-- | Hex written out rather than pulled from a codec package: the
--   fixture is a tracked, human-readable file and this is the whole
--   dependency it needs.
toHex ∷ BS.ByteString → Text
toHex = T.pack ∘ concatMap byte ∘ BS.unpack
  where byte b = [ intToDigit (fromIntegral b `div` 16)
                 , intToDigit (fromIntegral b `mod` 16) ]

fromHex ∷ Text → Either String BS.ByteString
fromHex t
    | odd (T.length t)          = Left "hex payload has an odd length"
    | not (T.all isHexDigit t)  = Left "hex payload holds a non-hex digit"
    | otherwise                 = Right (BS.pack (go (T.unpack t)))
  where
    go (a : b : rest) =
        fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = []

-- | Read one labelled hex payload back out of the fixture file.
decodeLegacy ∷ S.Serialize α ⇒ Text → Text → Either String α
decodeLegacy body label =
    case [ v | l ← T.lines body
             , (k : v : _) ← [T.words l], k ≡ label ] of
        []      → Left ("fixture carries no '" <> T.unpack label <> "' row")
        (v : _) → fromHex v ⌦ S.decode

-- * Fixture IO

-- | Read a checked-in fixture, first rewriting it when its capture
--   variable is set. Capture is opt-in and off in CI, so the default
--   path reads a tracked file and can only ever fail an assertion.
withFixture ∷ String → FilePath → IO Text → IO Text
withFixture var path produce = do
    capture ← lookupEnv var
    case capture of
        Just v | not (null v) → do
            createDirectoryIfMissing True (takeDirectory path)
            body ← produce
            TIO.writeFile path body
            pure body
        _ → TIO.readFile path

-- * The spec

spec ∷ Spec
spec = beforeAll loadShippedCatalog $ do

  describe "registration order" $ do

    it "the shipped catalog holds no duplicate authored names, which is \
       \what makes the name a usable species key" $ \shipped → do
      let names = map enName (catalogEntries shipped)
      length names `shouldSatisfy` (> 8)
      L.nub names `shouldBe` names

    it "generates name-normalized-identical instances from two OPPOSING \
       \registration orders" $ \shipped → do
      let entries = catalogEntries shipped
          forward = reindexCatalog entries
          backward = reindexCatalog (reverse entries)
      -- The premise: the two catalogs really do number the same species
      -- differently, so this is not comparing a catalog with itself.
      map fiSpecies (concatMap (map niInstance ∘ snd) (placeAll forward))
        `shouldNotBe` []
      case entries of
          []      → expectationFailure "the shipped catalog is empty"
          (e : _) →
              (fst <$> findSpeciesByName (enName e) forward)
                `shouldNotBe` (fst <$> findSpeciesByName (enName e) backward)
      placeAll backward `shouldBe` placeAll forward

    it "places something on every sampled chunk, so the equivalence \
       \above is not an equality between two empty layouts" $ \shipped → do
      let placed = placeAll (reindexCatalog (catalogEntries shipped))
      forM_ placed $ \(coord, nis) →
          (coord, length nis) `shouldSatisfy` ((> 0) ∘ snd)
      L.nub (L.sort (map niName (concatMap snd placed)))
        `shouldSatisfy` ((> 1) ∘ length)

    it "is unmoved by an IMPOSSIBLE-FIT species that sorts first and \
       \shifts every catalog position" $ \shipped → do
      let entries  = catalogEntries shipped
          without  = reindexCatalog entries
          with     = reindexCatalog (impossibleEntry : entries)
      -- It really does shift positions: every shipped species' numeric
      -- id moves by one, and the intruder sorts ahead of all of them.
      map enName (take 1 entries)
        `shouldSatisfy` all (> enName impossibleEntry)
      fcNextId with `shouldBe` fcNextId without + 1
      -- and it really never places.
      concatMap snd (placeAll with)
        `shouldSatisfy` all ((≢ enName impossibleEntry) ∘ niName)
      -- so nothing else moves either.
      placeAll with `shouldBe` placeAll without

  describe "the seed-42 golden layout" $

    it "matches the checked-in golden, byte for byte" $ \shipped → do
      let live = renderGolden (placeAll shipped)
      checked ← withFixture "SYNARCHY_FLORA_GOLDEN_CAPTURE" goldenPath
                    (pure live)
      -- Compared line by line: a whole-file `shouldBe` on a few hundred
      -- lines reports an unreadable blob.
      T.lines live `shouldBe` T.lines checked
      length (filter (not ∘ T.isPrefixOf "#") (T.lines checked))
        `shouldSatisfy` (> 20)

  describe "a pre-change save's numeric flora references" $

    it "resolve through the canonical catalog to the explicitly expected \
       \post-change species — a reinterpretation this slice accepts" $
        \shipped → do
      body ← withFixture "SYNARCHY_FLORA_LEGACY_CAPTURE" legacyPath
                 (pure renderLegacy)
      let named fid = fsName <$> lookupSpecies fid shipped
      -- The fixture decodes to the numbers the pre-change build wrote.
      decodeLegacy body "edit" `shouldBe` Right legacyEdit
      decodeLegacy body "plot" `shouldBe` Right legacyPlot
      decodeLegacy body "designation" `shouldBe` Right legacyDesignation
      -- Each number now names a DIFFERENT species than it did, and the
      -- expectation is the specific new one, never "some species".
      forM_ postChangeExpectation $ \(n, expected) →
          (n, named (FloraId n)) `shouldBe` (n, Just expected)
      forM_ preChangeCatalog $ \(n, was) →
          (n, named (FloraId n)) `shouldNotBe` (n, Just was)
      -- and the three references are read from the DECODED fixture, so
      -- a fixture that stopped carrying them cannot pass vacuously.
      case ( decodeLegacy body "edit", decodeLegacy body "plot"
           , decodeLegacy body "designation" ) of
          (Right (WePlaceFloraD _ _ f1 _ _), Right p, Right d) → do
              named f1 `shouldBe` Just "coconut_palm"
              named (cpiSpecies p) `shouldBe` Just "tomato_plant"
              named (ptiCrop d) `shouldBe` Just "scots_pine"
          other → expectationFailure
              ("fixture rows did not decode to the three references: "
               ⧺ show other)
