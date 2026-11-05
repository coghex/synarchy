{-# OPTIONS_GHC -Wno-orphans #-}
-- | Declared hand-written encoders reachable from a persisted wire root
--   (issue #2916). See "World.Save.Wire" for the classes of boundary.
--
--   Each instance below is the HUMAN declaration of a hand-written
--   'Data.Serialize.Serialize' instance: its owner module, a description
--   of the bytes it writes, the hspec example in the @current save
--   schema@ group that exercises it, and the wire-level types the
--   encoder writes (so the walk continues into them). None of it is
--   derived. A change to such an encoder, or to a type it writes that is
--   not named here, is outside mechanical detection: it still requires
--   every affected version to advance, current-only acceptance and a
--   distinguishing test (see @docs/current_save_schema_guard.md@).
module World.Save.Wire.Custom
    ( customBoundaries
    ) where

import UPrelude
import qualified Data.Serialize as S
import qualified Data.Vector.Unboxed as VU
import Data.Proxy (Proxy(..))
import World.Chunk.Types (ChunkCoord)
import World.Fluid.Types (IceLevelGrid(..))
import World.Fluid.Lake.Types
    (LakeChunkEntry, WorldLakes, emptyWorldLakes, LakeId, Lake)
import World.Fluid.River.Types
    (RiverChunkEntry, WorldRivers, emptyWorldRivers, RiverId, River)
import World.Fluid.OceanMask (WorldOceanMask, emptyWorldOceanMask)
import World.Fluid.Seabed.Types (SeabedTable, emptySeabedTable)
import World.Geology.Coastal.Types (CoastalTable, emptyCoastalTable)
import World.Geology.Ore.Types (WorldOreDeposits, emptyWorldOreDeposits)
import World.Page.Types (WorldPageId(..))
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Wire

decl ∷ Text → Text → CustomDecl
decl owner contract = CustomDecl
    { cdOwner = owner
    , cdContract = contract
    , cdTest = "current save schema: custom boundaries round-trip" }

kid ∷ ∀ b. WireSchema b ⇒ Proxy b → Child
kid = Child

coordRows ∷ ∀ b. (S.Serialize b, WireSchema b) ⇒ Proxy b → Child
coordRows _ = Child (Proxy ∷ Proxy [(ChunkCoord, b)])

instance WireSchema IceLevelGrid where
    wireNode = customNodeOpaque
        (decl "World.Fluid.Types"
            "grid width (Int), spacing (Int), then the level vector as a list of Int")
        [kid (Proxy ∷ Proxy Int), kid (Proxy ∷ Proxy [Int])]
    wireSamples _ = [IceLevelGrid 1 1 (VU.fromList [0]), IceLevelGrid 2 3 (VU.fromList [1, 2])]

instance WireSchema LakeChunkEntry where
    wireNode = customNodeOpaque
        (decl "World.Fluid.Lake.Types"
            "the lake id, then the 256-tile bitmask packed into a list of 32 Word8")
        [kid (Proxy ∷ Proxy LakeId), kid (Proxy ∷ Proxy [Word8])]

instance WireSchema WorldLakes where
    wireNode = customNodeOpaque
        (decl "World.Fluid.Lake.Types"
            "the lake list, the per-chunk entry rows, then the per-chunk carve-delta rows")
        [ kid (Proxy ∷ Proxy [Lake])
        , coordRows (Proxy ∷ Proxy [LakeChunkEntry])
        , coordRows (Proxy ∷ Proxy [Int]) ]
    wireSamples _ = [emptyWorldLakes]

instance WireSchema RiverChunkEntry where
    wireNode = customNodeOpaque
        (decl "World.Fluid.River.Types"
            "the river id, the packed 32-byte bitmask, the per-tile surface Z list, the width-radius list")
        [ kid (Proxy ∷ Proxy RiverId), kid (Proxy ∷ Proxy [Word8])
        , kid (Proxy ∷ Proxy [Int]) ]

instance WireSchema WorldRivers where
    wireNode = customNodeOpaque
        (decl "World.Fluid.River.Types"
            "the river list, the per-chunk entry rows, then the per-chunk carve-delta rows")
        [ kid (Proxy ∷ Proxy [River])
        , coordRows (Proxy ∷ Proxy [RiverChunkEntry])
        , coordRows (Proxy ∷ Proxy [Int]) ]
    wireSamples _ = [emptyWorldRivers]

instance WireSchema WorldOceanMask where
    wireNode = customNodeOpaque
        (decl "World.Fluid.OceanMask"
            "a list of (chunk coordinate, packed bitmask bytes) rows")
        [coordRows (Proxy ∷ Proxy [Word8])]
    wireSamples _ = [emptyWorldOceanMask]

instance WireSchema SeabedTable where
    wireNode = customNodeOpaque
        (decl "World.Fluid.Seabed.Types"
            "elevation-delta rows then material-override rows, each (chunk coordinate, list)")
        [coordRows (Proxy ∷ Proxy [Int]), coordRows (Proxy ∷ Proxy [Word8])]
    wireSamples _ = [emptySeabedTable]

instance WireSchema CoastalTable where
    wireNode = customNodeOpaque
        (decl "World.Geology.Coastal.Types"
            "elevation-delta rows then material-override rows, each (chunk coordinate, list)")
        [coordRows (Proxy ∷ Proxy [Int]), coordRows (Proxy ∷ Proxy [Word8])]
    wireSamples _ = [emptyCoastalTable]

instance WireSchema WorldOreDeposits where
    wireNode = customNodeOpaque
        (decl "World.Geology.Ore.Types"
            "the per-chunk deposit map as a list of (chunk coordinate, [(Word8, Int)]) rows")
        [kid (Proxy ∷ Proxy [(ChunkCoord, [(Word8, Int)])])]
    wireSamples _ = [emptyWorldOreDeposits]

instance WireSchema WorldPageId where
    wireNode = newtypeNode "WorldPageId" "unWorldPageId" WorldPageId
    wireSamples = newtypeSamples WorldPageId

instance WireSchema ComponentId where
    wireNode = newtypeNode "ComponentId" "" ComponentId
    wireSamples = newtypeSamples ComponentId

-- | Every declared hand-written boundary, as @(type key, declaration)@.
--   The guard requires each to be reachable from a persisted root and
--   every reachable 'WKCustom' node to be listed here.
customBoundaries ∷ [(Text, CustomDecl)]
customBoundaries =
    [ b (Proxy ∷ Proxy IceLevelGrid)
    , b (Proxy ∷ Proxy LakeChunkEntry), b (Proxy ∷ Proxy WorldLakes)
    , b (Proxy ∷ Proxy RiverChunkEntry), b (Proxy ∷ Proxy WorldRivers)
    , b (Proxy ∷ Proxy WorldOceanMask), b (Proxy ∷ Proxy SeabedTable)
    , b (Proxy ∷ Proxy CoastalTable), b (Proxy ∷ Proxy WorldOreDeposits) ]
  where
    b ∷ ∀ a. WireSchema a ⇒ Proxy a → (Text, CustomDecl)
    b p = case wnKind (wireNode p) of
        WKCustom d → (wnKey (wireNode p), d)
        _          → error "World.Save.Wire.Custom: not a custom boundary"
