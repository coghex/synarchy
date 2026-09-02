{-# LANGUAGE Strict #-}
-- | Screen-space flora selection (#1856) — the Chop tool's target
--   oracle.
--
-- Chop selects trees by where they are DRAWN, not by which tiles a
-- rectangle covers (D-9). A tile rectangle and the box the player
-- actually drew disagree around cliffs, because elevation, sub-tile
-- offsets and sprite geometry all move a tree's rendered position away
-- from its tile's — so the old rule designated trees outside the box
-- and missed trees inside it. Retaining it under a press-drag gesture
-- is explicitly rejected.
--
-- Everything here is derived from the SAME values the renderer draws
-- with, through the two shared boundaries
-- ("World.Render.FloraDraws" decides which sprites exist and what each
-- is showing; "World.Render.FloraProjection" places them):
--
--   * placement, quad bounds, ground-contact anchor and painter depth;
--   * the live terrain-derived surface z, never a stored one;
--   * the growth-stage or depleted texture and its real dimensions;
--   * the facing-aware wrap offset (#1176) and the chunk-visibility
--     test, so a seam-side tree is picked where it is painted;
--   * the z-slice cull, so a tree the player cannot see is never
--     selected.
--
-- The projection itself mirrors "World.Render.HitTest": FRAMEBUFFER
-- aspect with WINDOW-pixel normalization, and a degenerate viewport
-- selects nothing rather than unprojecting to a non-finite coordinate.
--
-- == Two eligibility sets, one oracle
--
-- Add and erase are symmetric gestures over the same geometry but not
-- over the same candidates (D-12):
--
--   * 'SelectChoppable' is the unchanged Chop predicate — a species
--     with a harvest block whose tags carry the requested one, and an
--     instance with no live regrowth timer. It deliberately does NOT
--     consult the forage API's growth-window @harvestable@ signal.
--   * 'SelectDesignated' is every currently designated live tree, so a
--     standing designation stays clearable by the player even after it
--     has stopped being add-eligible.
module World.Flora.HitTest
    ( FloraPick(..)
    , FloraSelectMode(..)
    , FloraHitView(..)
    , floraHitView
    , pickFloraAt
    , pickFloraInRect
    , floraSelectCandidates
    , floraPainterOrder
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Asset.Handle (TextureHandle, toInt)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Viewport (viewportDegenerate)
import World.Chop.Types (ChopDesignations)
import World.Chunk.Types (LoadedChunk(..))
import World.Flora.Harvest (FloraHarvests)
import World.Flora.Identity (FloraInstanceId)
import World.Flora.Types
import World.Generate (viewDepth)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.FloraDraws (FloraDraw(..), chunkFloraDraws)
import World.Render.FloraProjection
    (FloraGeom(..), floraGeom, floraVisibleInSlice)
import World.Render.SpriteDepth
    (FrontWallLift, frameFrontWallLift, liftSpriteSortKey)
import World.Render.Camera (placementCamera, quadCacheMargins)
import World.Render.Camera.Types (WorldCameraSnapshot(..))
import World.Render.ViewBounds
    (ViewBounds, expandViewBounds, viewBoundsAt)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types (WorldState(..), pageWrapWorldSize)
import World.Tile.Types (WorldTileData(..))
import World.Time.Types
    (calendarDaysPerYear, worldAbsoluteDay, defaultCalendarConfig)

-- | One selected plant: its stable identity plus the coords a
--   designation needs. The tile is in the CHUNK's own frame and the z
--   is the LIVE surface z, exactly what the sprite is drawn at.
data FloraPick = FloraPick
    { fpInstanceId ∷ !FloraInstanceId
    , fpGX         ∷ !Int
    , fpGY         ∷ !Int
    , fpZ          ∷ !Int
    } deriving (Show, Eq)

-- | Which plants a gesture may act on.
data FloraSelectMode
    = SelectChoppable !Text
      -- ^ Add: harvestable species carrying this tag, no live regrowth
      --   timer. Unchanged from the two-click rectangle.
    | SelectDesignated
      -- ^ Erase: whatever is designated right now.
    deriving (Show, Eq)

-- | Everything the oracle reads, snapshotted together. Assembled once
--   by 'floraHitView' so a gesture's click and box halves cannot see
--   two different frames, and so the whole rule stays a pure function
--   that a spec can drive with synthetic chunks.
data FloraHitView = FloraHitView
    -- The PLACEMENT half: the camera the flora currently on screen was
    -- built with ('World.Render.Camera.placementCamera'). A cached
    -- quad's wrap alias is baked into its world coordinates, so
    -- deriving placement from the LIVE camera puts a seam-side tree a
    -- whole world away from where it is drawn for as long as the cache
    -- is reused across the alias midpoint.
    { fhvFacing      ∷ !CameraFacing
    , fhvZSlice      ∷ !Int
    , fhvPlaceCamX   ∷ !Float
    , fhvPlaceCamY   ∷ !Float
    -- The VIEW half: the LIVE camera, which is what the pixel→world
    -- unprojection must use — cached world coordinates are viewed
    -- through the live camera every frame.
    , fhvZoom        ∷ !Float
    , fhvCamX        ∷ !Float
    , fhvCamY        ∷ !Float
    , fhvFbW         ∷ !Int
    , fhvFbH         ∷ !Int
    , fhvWinW        ∷ !Int
    , fhvWinH        ∷ !Int
    , fhvWorldSize   ∷ !Int
    , fhvEffDepth    ∷ !Int
    , fhvViewBounds  ∷ !ViewBounds
    , fhvTiles       ∷ !WorldTileData
    , fhvCatalog     ∷ !FloraCatalog
    , fhvHarvests    ∷ !FloraHarvests
    , fhvDesignated  ∷ !ChopDesignations
    , fhvTexSizes    ∷ !(HM.HashMap TextureHandle (Int, Int))
    , fhvDaysPerYear ∷ !Int
    , fhvAbsDay      ∷ !Int
    , fhvFrontWall   ∷ FrontWallLift
      -- ^ The frame's structure front-wall sprite lift, built by the
      --   SAME 'frameFrontWallLift' the render pass builds it with.
      --   Without it a tree lifted to clear a wall would be ranked at
      --   its unlifted depth and the picker would disagree with what
      --   was actually painted.
    }

-- | Snapshot the live engine + page state the oracle needs.
floraHitView ∷ EngineEnv → WorldState → IO FloraHitView
floraHitView env worldState = do
    let rv = toRenderViewCapability env
        wsc = toWorldSimCapability env
    camera       ← readIORef (rvCameraRef rv)
    cachedQuads  ← readIORef (wsQuadCacheRef worldState)
    (winW, winH) ← readIORef (rvWindowSizeRef rv)
    (fbW, fbH)   ← readIORef (rvFramebufferSizeRef rv)
    texSizes     ← readIORef (rvTextureSizeRef rv)
    tileData     ← readIORef (wsTilesRef worldState)
    catalog      ← readIORef (wsFloraCatalogRef wsc)
    harvests     ← readIORef (wsFloraHarvestsRef worldState)
    designated   ← readIORef (wsChopDesignationsRef worldState)
    paramsM      ← readIORef (wsGenParamsRef worldState)
    worldDate    ← readIORef (wsDateRef worldState)
    worldSize    ← pageWrapWorldSize worldState
    let calendar = maybe defaultCalendarConfig wgpCalender paramsM
        zoom = camZoom camera
        effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
        live = WorldCameraSnapshot
            { wcsPosition = camPosition camera
            , wcsZoom     = zoom
            , wcsZSlice   = camZSlice camera
            , wcsFbSize   = (fbW, fbH)
            , wcsFacing   = camFacing camera
            }
        placed = placementCamera cachedQuads live
        (placeX, placeY) = wcsPosition placed
    pure FloraHitView
        { fhvFacing      = wcsFacing placed
        , fhvZSlice      = wcsZSlice placed
        , fhvPlaceCamX   = placeX
        , fhvPlaceCamY   = placeY
        , fhvZoom        = zoom
        , fhvCamX        = fst (camPosition camera)
        , fhvCamY        = snd (camPosition camera)
        , fhvFbW         = fbW
        , fhvFbH         = fbH
        , fhvWinW        = winW
        , fhvWinH        = winH
        , fhvWorldSize   = worldSize
        , fhvEffDepth    = effDepth
        -- The cache's OWN coverage: its snapshot's bounds widened by
        -- the very margins 'World.Render.Quads' widens them with, so a
        -- chunk the cache built is still a candidate here.
        , fhvViewBounds  = expandViewBounds (quadCacheMargins placed)
                               (viewBoundsAt (wcsPosition placed)
                                   (wcsZoom placed) fbW fbH effDepth)
        , fhvTiles       = tileData
        , fhvCatalog     = catalog
        , fhvHarvests    = harvests
        , fhvDesignated  = designated
        , fhvTexSizes    = texSizes
        , fhvDaysPerYear = calendarDaysPerYear calendar
        , fhvAbsDay      = worldAbsoluteDay calendar worldDate
        , fhvFrontWall   = frameFrontWallLift (camFacing camera) worldSize
                               (camZSlice camera) effDepth
                               (wtdChunks tileData)
        }

-- | Every eligible, currently VISIBLE plant paired with the geometry it
--   is drawn with. The one enumeration both gestures share, so a click
--   and a box can never disagree about what exists.
floraSelectCandidates
    ∷ FloraHitView → FloraSelectMode → [(FloraPick, FloraGeom)]
floraSelectCandidates view mode =
    [ (pick, geom)
    | (coord, lc) ← HM.toList (wtdChunks (fhvTiles view))
    , Just wrapOff ← [isChunkVisibleWrapped (fhvFacing view)
                          (fhvWorldSize view) (fhvViewBounds view)
                          (fhvPlaceCamX view) (fhvPlaceCamY view) coord]
    , fd ← chunkFloraDraws (fhvCatalog view) (fhvDaysPerYear view)
               (fhvAbsDay view) (fhvHarvests view) (lcCoord lc) lc
    , let inst = fdInstance fd
    , floraVisibleInSlice (fhvZSlice view) (fhvEffDepth view) inst
    , eligible view mode inst
    , let base = floraGeom (fhvFacing view) (fdGX fd) (fdGY fd) inst
                     (fdTexture fd) (fhvTexSizes view)
                     (fhvZSlice view) wrapOff
          -- The FINAL painter depth, front-wall lift included (#418).
          geom = base { fgSortKey = liftSpriteSortKey (fhvFrontWall view)
                            (lcCoord lc) (fdGX fd) (fdGY fd) (fgSortKey base) }
          pick = FloraPick { fpInstanceId = fiInstanceId inst
                           , fpGX = fdGX fd
                           , fpGY = fdGY fd
                           , fpZ  = fiZ inst }
    ]

-- | Chop eligibility. Add filters by the species' harvest tags and the
--   instance's own regrowth timer — the exact predicate the two-click
--   rectangle applied, deliberately NOT the forage growth-window
--   @harvestable@ signal. Erase filters by what is designated NOW.
eligible ∷ FloraHitView → FloraSelectMode → FloraInstance → Bool
eligible view (SelectChoppable tag) inst = fromMaybe False $ do
    sp ← lookupSpecies (fiSpecies inst) (fhvCatalog view)
    fh ← fsHarvest sp
    pure $ tag `elem` fhTags fh
         ∧ HM.lookupDefault 0 (fiInstanceId inst) (fhvHarvests view) ≤ 0
eligible view SelectDesignated inst =
    HM.member (fiInstanceId inst) (fhvDesignated view)

-- | The topmost eligible tree under a pointer, in WINDOW pixels.
--
--   \"Under the pointer\" is the inclusive bounds of the currently
--   rendered sprite quad, the AABB precedent 'Unit.HitTest' sets.
--
--   \"Topmost\" is the candidate the renderer draws LAST, and it is
--   decided by the renderer's OWN comparison —
--   'Engine.Scene.Types.Batch.quadPainterOrder', reconstructed here
--   from the same values the quad carries. That order is
--   @(sqSortKey, v0.x, v0.y, v2.x, v2.y, texture)@:
--
--     * the FINAL painter depth, structure front-wall lift included
--       ('World.Render.SpriteDepth'), so a lifted tree ranks exactly
--       where it was painted; and
--     * the quad's own rect and texture, which break the depth ties
--       @sqSortKey@ alone leaves — two wood-tagged co-tenants on one
--       tile at one z with equal 'fiOffV' really do share a key, and
--       the scene sorter is an unstable introsort, so before #1856 they
--       were drawn in an order nothing could agree with.
--
--   Two candidates still equal after all six RENDER IDENTICALLY (see
--   'quadPainterOrder'): same depth, same rect, same texture, hence the
--   same bytes whichever the sorter placed last. The stable instance id
--   is the deterministic backstop there — a choice between two
--   candidates the frame itself cannot tell apart — and it is shared
--   with the marker so the two never disagree.
pickFloraAt
    ∷ FloraHitView → FloraSelectMode → Float → Float → Maybe FloraPick
pickFloraAt view mode pixX pixY
    | degenerate view = Nothing
    | otherwise =
        case sortOn rank hits of
            []      → Nothing
            (h : _) → Just (fst3 h)
  where
    (worldX, worldY) = windowToWorld view pixX pixY
    hits =
        [ (pick, floraPainterOrder geom, fpInstanceId pick)
        | (pick, geom) ← floraSelectCandidates view mode
        , worldX ≥ fgDrawX geom
        , worldX ≤ fgDrawX geom + fgQuadW geom
        , worldY ≥ fgDrawY geom
        , worldY ≤ fgDrawY geom + fgQuadH geom
        ]
    -- Descending renderer order, then descending id (see above).
    rank (_, order, iid) = (Down order, Down iid)
    fst3 (a, _, _) = a

-- | Every eligible tree whose rendered ground-contact anchor lies
--   inside a screen-space rectangle, in WINDOW pixels.
--
--   Either drag direction normalizes, and the bounds are CLOSED — the
--   inclusive convention 'Unit.HitTest.hitTestUnitsInRect' established,
--   so a tree exactly on the edge the player dragged to is inside it.
--   Results are ordered by instance id, never hash order.
pickFloraInRect
    ∷ FloraHitView → FloraSelectMode
    → Float → Float → Float → Float → [FloraPick]
pickFloraInRect view mode ax ay bx by
    | degenerate view = []
    | otherwise = sortOn fpInstanceId
        [ pick
        | (pick, geom) ← floraSelectCandidates view mode
        , let (px, py) = worldToWindow view (fgAnchorX geom) (fgAnchorY geom)
        , px ≥ x1, px ≤ x2, py ≥ y1, py ≤ y2
        ]
  where
    x1 = min ax bx
    x2 = max ax bx
    y1 = min ay by
    y2 = max ay by

-- | A zero-size window or framebuffer (a minimize, or a
--   minimize/restore mid-gesture) makes every projection below
--   non-finite. Select nothing rather than a garbage set.
degenerate ∷ FloraHitView → Bool
degenerate view =
    viewportDegenerate (fhvWinW view) (fhvWinH view)
                       (fhvFbW view) (fhvFbH view)

-- | Window pixel → world coordinate. Framebuffer aspect, window
--   normalization — the same pairing 'World.Render.HitTest.pickWorldTile'
--   uses, so a Chop pick and a tile pick agree about where a pixel is.
windowToWorld ∷ FloraHitView → Float → Float → (Float, Float)
windowToWorld view pixX pixY =
    let (vw, vh) = viewExtent view
        normX = pixX / fromIntegral (fhvWinW view)
        normY = pixY / fromIntegral (fhvWinH view)
    in ( (normX * 2.0 - 1.0) * vw + fhvCamX view
       , (normY * 2.0 - 1.0) * vh + fhvCamY view )

-- | The exact inverse of 'windowToWorld'.
worldToWindow ∷ FloraHitView → Float → Float → (Float, Float)
worldToWindow view wx wy =
    let (vw, vh) = viewExtent view
        normX = ((wx - fhvCamX view) / vw + 1.0) / 2.0
        normY = ((wy - fhvCamY view) / vh + 1.0) / 2.0
    in (normX * fromIntegral (fhvWinW view), normY * fromIntegral (fhvWinH view))

-- | 'Engine.Scene.Types.Batch.quadPainterOrder' for a flora sprite,
--   reconstructed from its projected geometry. 'World.Render.FloraQuads'
--   emits @sqV0@ at the quad's top-left and @sqV2@ at its bottom-right,
--   so these are the same six values the sorter compares — pinned by
--   a spec that sorts REAL quads through the real sorter and checks the
--   picker agrees with what came out last.
floraPainterOrder ∷ FloraGeom → (Float, Float, Float, Float, Float, Int)
floraPainterOrder g =
    ( fgSortKey g
    , fgDrawX g, fgDrawY g
    , fgDrawX g + fgQuadW g, fgDrawY g + fgQuadH g
    , toInt (fgTexture g) )

viewExtent ∷ FloraHitView → (Float, Float)
viewExtent view =
    let aspect = fromIntegral (fhvFbW view) / fromIntegral (fhvFbH view)
    in (fhvZoom view * aspect, fhvZoom view)
