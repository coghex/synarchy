-- | Exact-level fluid diagnostics (#2535, DFL-6).
--
--   Three maintainer-facing readers of one 'FluidCell' must agree with
--   its authoritative exact surface ('fcExactSurface', eighths of a z)
--   while keeping the integer ceiling every older reader knows:
--
--   * the @--dump@ fluid layer, through the PRODUCTION serializer
--     'App.Dump.dumpTilesJSON' — @fluidSurfaceUnits@ / @fluidLevel@
--     beside the retained @fluidType@ / @fluidSurf@;
--   * the HUD cursor line, through the production
--     'World.Thread.Cursor.fluidCursorText' that @sendTileInfo@ sends;
--   * @world.getAreaFluid@'s additive @surfaceUnits@ / @level@ fields,
--     through the real registered closure.
--
--   The expectations are computed independently of the production
--   helpers: the level from the issue's formula
--   @1 + ((units - 1) mod 8)@ and the surface from a 'Rational'
--   mathematical ceiling, over every exact surface from -40 to 40 — so
--   negative, zero, full level-8 and every partial remainder are all
--   covered, not a hand-picked few.
--
--   Run:
--   @cabal test synarchy-test-headless --test-options='--match "Fluid exact diagnostics"'@
module Test.Headless.World.FluidDiagnostics (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, writeIORef)
import Data.Ratio ((%))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua

import App.Cli (ChunkRegion(..), DumpLayers(..))
import App.Dump (dumpTilesJSON)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.WorldQuery.Fluid (worldGetAreaFluidFn)
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Material (emptyMaterialRegistry)
import World.Thread.Cursor (fluidCursorText)
import World.Types
import World.Weather.Types (initClimateState)

-- * Fixture
--
--   One chunk at (0, 0), so a tile's global coordinate is its local
--   one. Column @i@ for @i@ in @0 .. 80@ holds exact surface @i - 40@,
--   cycling through every 'FluidType'; every other column is dry.

sweepUnits ∷ [Int]
sweepUnits = [-40 .. 40]

unitsAtIndex ∷ Int → Maybe Int
unitsAtIndex i
    | i < length sweepUnits = Just (i - 40)
    | otherwise             = Nothing

typeAtIndex ∷ Int → FluidType
typeAtIndex i = case i `mod` 4 of
    0 → River
    1 → Lake
    2 → Ocean
    _ → Lava

fixtureCell ∷ Int → Maybe FluidCell
fixtureCell i = FluidCell (typeAtIndex i) <$> unitsAtIndex i

fixtureChunk ∷ LoadedChunk
fixtureChunk =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = -60
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0
                 }
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area (-41)
        , lcTerrainSurfaceMap = VU.replicate area (-41)
        , lcFluidMap = V.generate area fixtureCell
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.replicate area 0
        , lcWaterTableMap = VU.replicate area (-50)
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) fixtureChunk
    , wtdMaxChunks = 4
    }

-- * Independent expectations

expectedLevel ∷ Int → Int
expectedLevel u = 1 + ((u - 1) `mod` 8)

expectedCeil ∷ Int → Int
expectedCeil u = ceiling (toInteger u % 8)

typeLabel ∷ FluidType → Text
typeLabel Ocean = "ocean"
typeLabel Lake  = "lake"
typeLabel River = "river"
typeLabel Lava  = "lava"

-- * Dump

noLayers ∷ DumpLayers
noLayers = DumpLayers
    { dlTerrain = False, dlMaterial = False, dlFluid = False
    , dlIce = False, dlOre = False, dlSlope = False }

dumpTiles ∷ DumpLayers → [KM.KeyMap Value]
dumpTiles layers =
    let bytes = dumpTilesJSON layers emptyMaterialRegistry 0
                              (initClimateState 0) fixtureTiles
                              (ChunkRegion 0 0 0 0)
    in fromMaybe (error "dump output is not a JSON array of objects")
                 (decode bytes)

tileIndex ∷ KM.KeyMap Value → Int
tileIndex t = case (KM.lookup "x" t, KM.lookup "y" t) of
    (Just (Number x), Just (Number y)) →
        round y * chunkSize + round x
    _ → error ("tile without integer x/y: " ⧺ show t)

fluidKeys ∷ [K.Key]
fluidKeys = ["fluidType", "fluidSurf", "fluidSurfaceUnits", "fluidLevel"]

num ∷ Int → Value
num = Number . fromIntegral

-- * Lua

-- | Every entry @world.getAreaFluid(cx, cy, r)@ returns, as
--   @x,y,type,surface,surfaceUnits,level@ lines in (y, x) order — the
--   verb's own scan order.
areaFluidLines ∷ WorldSimCapability → Int → Int → Int → IO [Text]
areaFluidLines wsc cx cy r = Lua.run @Lua.Exception $ do
    Lua.openlibs
    callStats ← Lua.liftIO newLuaCallStats
    Lua.newtable
    registerLuaFunction callStats "world" "getAreaFluid" (worldGetAreaFluidFn wsc)
    Lua.setglobal "world"
    let chunk = T.unlines
            [ "local out = {}"
            , "for _, c in ipairs(world.getAreaFluid(" <> tshow cx <> ","
                <> tshow cy <> "," <> tshow r <> ")) do"
            , "  out[#out+1] = table.concat({c.x, c.y, c.type, c.surface,"
            , "    tostring(c.surfaceUnits), tostring(c.level)}, ',')"
            , "end"
            , "return table.concat(out, '\\n')"
            ]
    st ← Lua.dostring (TE.encodeUtf8 chunk)
    case st of
        Lua.OK → do
            mOut ← Lua.tostring (-1)
            pure $ filter (not . T.null)
                 $ T.lines (maybe "" TE.decodeUtf8Lenient mOut)
        _ → do
            err ← Lua.tostring (-1)
            Lua.liftIO $ expectationFailure $ "Lua chunk failed: "
                ⧺ maybe "<no message>" (T.unpack . TE.decodeUtf8Lenient) err
            pure []

spec ∷ Spec
spec = describe "Fluid exact diagnostics" $ do

  describe "dump fluid layer" $ do
    let tiles = dumpTiles noLayers { dlFluid = True }

    it "serializes every wet sweep tile with exact units, level and its ceiling" $ do
      let wet = [ (i, t) | t ← tiles, let i = tileIndex t
                         , isJust (unitsAtIndex i) ]
      length wet `shouldBe` length sweepUnits
      forM_ wet $ \(i, t) → do
        let u = fromMaybe 0 (unitsAtIndex i)
        (i, [ KM.lookup k t | k ← fluidKeys ]) `shouldBe`
          ( i
          , [ Just (String (typeLabel (typeAtIndex i)))
            , Just (num (expectedCeil u))
            , Just (num u)
            , Just (num (expectedLevel u)) ] )

    it "pins the named edge cases: partial, full, zero and negative planes" $ do
      -- Hand-worked, so a shared mistake in the formulas above could
      -- not hide here: (units, ceiling, level).
      let view u = case [ t | t ← tiles, tileIndex t ≡ u + 40 ] of
            [t] → (KM.lookup "fluidSurf" t, KM.lookup "fluidLevel" t)
            ts  → error ("expected one tile for units " ⧺ show u
                          ⧺ ", got " ⧺ show (length ts))
      view 19    `shouldBe` (Just (num 3),    Just (num 3))
      view 24    `shouldBe` (Just (num 3),    Just (num 8))
      view 1     `shouldBe` (Just (num 1),    Just (num 1))
      view 0     `shouldBe` (Just (num 0),    Just (num 8))
      view (-5)  `shouldBe` (Just (num 0),    Just (num 3))
      view (-13) `shouldBe` (Just (num (-1)), Just (num 3))
      view (-16) `shouldBe` (Just (num (-2)), Just (num 8))

    it "emits explicit null for all four fields on a dry tile" $ do
      let dry = [ t | t ← tiles, isNothing (unitsAtIndex (tileIndex t)) ]
      dry `shouldNotBe` []
      forM_ dry $ \t →
        [ KM.lookup k t | k ← fluidKeys ] `shouldBe` replicate 4 (Just Null)

    it "omits every fluid field when the fluid layer is disabled" $ do
      let off = dumpTiles noLayers { dlMaterial = True }
      length off `shouldBe` chunkSize * chunkSize
      forM_ off $ \t →
        [ k | k ← fluidKeys, KM.member k t ] `shouldBe` []

  describe "cursor fluid line" $ do

    it "prints nothing for a dry column" $
      fluidCursorText Nothing `shouldBe` ""

    it "shows the ceiling surface beside the exact units and level" $ do
      fluidCursorText (Just (FluidCell River 19)) `shouldBe`
        "Fluid: River (surface z=3, exact 19/8, level 3/8)"
      fluidCursorText (Just (FluidCell Ocean (-16))) `shouldBe`
        "Fluid: Ocean (surface z=-2, exact -16/8, level 8/8)"

    it "agrees with the independent expectations across the sweep" $
      forM_ sweepUnits $ \u →
        fluidCursorText (Just (FluidCell Lake u)) `shouldBe`
          ("Fluid: Lake (surface z=" <> tshow (expectedCeil u)
             <> ", exact " <> tshow u <> "/8, level "
             <> tshow (expectedLevel u) <> "/8)")

  beforeAll activeFixture $
    describe "world.getAreaFluid structured fields" $ do

      it "adds exact units and level while surface stays the ceiling" $ \wsc → do
        -- Radius 8 around (8, 0) scans x 0..16 and rows -8..8. Only
        -- chunk (0, 0) is loaded, so that is every sweep column (rows
        -- 0..5, indices 0..80) and every dry column after them.
        rows ← areaFluidLines wsc 8 0 8
        let expected =
              [ T.intercalate ","
                  [ tshow x, tshow y, typeLabel (typeAtIndex i)
                  , tshow (expectedCeil u), tshow u, tshow (expectedLevel u) ]
              | y ← [0 .. 8], x ← [0 .. 16]
              , x < chunkSize
              , let i = y * chunkSize + x
              , Just u ← [unitsAtIndex i] ]
        rows `shouldBe` expected

      it "returns no entry for a dry tile, never a level-0 cell" $ \wsc → do
        -- Index 81 onward is dry: (1, 5) is index 81.
        rows ← areaFluidLines wsc 1 5 0
        rows `shouldBe` []
  where
    activeFixture = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ws ← emptyWorldState
        writeIORef (wsTilesRef ws) fixtureTiles
        let page = WorldPageId "fluid_diagnostics"
        ref ← newIORef emptyWorldManager
            { wmWorlds = [(page, ws)], wmVisible = [page] }
        pure (toWorldSimCapability env) { wsWorldManagerRef = ref }
