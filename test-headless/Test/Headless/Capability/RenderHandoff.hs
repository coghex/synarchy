{-# LANGUAGE UnicodeSyntax #-}
-- | Projection-aliasing coverage for the @world-sim-render-handoff@
--   RENDER-HANDOFF capability record (issue #894, E5b of the @EngineEnv@
--   capability split #537) — the direct counterpart of
--   "Test.Headless.Capability.WorldSim", which covers E5a's other half,
--   for the same reason.
--
--   'Engine.Core.Capability.RenderHandoff.toRenderHandoffCapability' is
--   documented as returning the __identical live containers__ 'EngineEnv'
--   already carries, never a copy and never a snapshot. For this
--   particular record that property is not merely convenient, it is the
--   whole mechanism: every one of its fields is a CROSS-THREAD
--   handoff, so a projection that copied a container would leave the
--   world thread writing a preview/atlas/quad set the render thread can
--   never observe, draining a blood-disposal queue nobody enqueues onto —
--   a silent GPU-resource leak (#788) rather than a crash — or drawing
--   every rotated wall with its authored sprite because the wall-art
--   catalogue Lua registered into is not the one the renderer reads
--   (#1712). A
--   projection that bound 'rhWorldPreviewRef' to @zoomAtlasDataRef env@,
--   or minted a fresh ref with @newIORef =\<\< readIORef@, would still
--   typecheck, still pass the SS6 ratchet, and still look right in a
--   diff.
--
--   Every field of the record is either an 'Data.IORef.IORef' (pointer
--   'Eq') or an 'Engine.Core.Queue.Queue' (which derives 'Eq' through
--   its 'TQueue'), so "same live container" is directly assertable:
--   compare the projected field against the same 'EngineEnv' field. A
--   wrong-container binding fails the corresponding example — including
--   a SWAP between the two same-typed palette refs or between the two
--   same-shaped single-slot upload handoffs, since each field is checked
--   against its own named counterpart rather than merely "some field of
--   the env".
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so this
--   module imports @EngineEnv(..)@ directly — that is the point: it
--   compares the capability's view against the unrestricted one.
module Test.Headless.Capability.RenderHandoff (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.RenderHandoff
  (RenderHandoffCapability(..), toRenderHandoffCapability)

-- | Assert two live containers are the SAME one. Neither 'IORef' nor
--   'Engine.Core.Queue.Queue' has a 'Show' instance, so this is an
--   'Eq'-only assertion carrying its own failure message rather than
--   'shouldBe'\'s rendered-value diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.RenderHandoff's \
      \convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toRenderHandoffCapability (nine of the ten render-handoff \
           \fields)" $ do
    -- The record carries ten fields; `rhSceneStatsRef` was added
    -- later (#1921) and has no alias assertion here yet. Adding it
    -- is its own change; this heading states what is actually
    -- covered.
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toRenderHandoffCapability env)) (field env)

    aliases "rhWorldPreviewRef"           rhWorldPreviewRef
            worldPreviewRef
    aliases "rhWorldPreviewGenerationRef" rhWorldPreviewGenerationRef
            worldPreviewGenerationRef
    aliases "rhZoomAtlasDataRef"          rhZoomAtlasDataRef
            zoomAtlasDataRef
    aliases "rhWorldQuadsRef"             rhWorldQuadsRef
            worldQuadsRef
    aliases "rhBloodDisposeQueue"         rhBloodDisposeQueue
            bloodDisposeQueue
    aliases "rhTexPaletteRef"             rhTexPaletteRef
            texPaletteRef
    aliases "rhTexPaletteHandlesRef"      rhTexPaletteHandlesRef
            texPaletteHandlesRef
    aliases "rhStructureWallCatalogRef"   rhStructureWallCatalogRef
            structureWallCatalogRef
    aliases "rhStructureArtCatalogRef"    rhStructureArtCatalogRef
            structureArtCatalogRef

    it "keeps the two single-slot upload handoffs distinct" $ \env → do
      -- worldPreviewRef and zoomAtlasDataRef are the record's pair of
      -- `IORef (Maybe (Int, Int, ByteString, _))` single-slot staging
      -- handoffs, both written by World.Thread.Command.Init within a few
      -- lines of each other and both consumed-to-Nothing by their own
      -- Message.WorldTexture handler. Their payload tails differ
      -- (Word64 generation vs. [WorldState]), so a transposition is a
      -- type error TODAY -- but that is a property of the payloads, not
      -- of the projection, and the per-field examples above are what
      -- actually pin each to its own counterpart. State the risk
      -- explicitly and assert they really are two different containers.
      let cap = toRenderHandoffCapability env
      sameContainer (rhWorldPreviewRef cap)  (worldPreviewRef env)
      sameContainer (rhZoomAtlasDataRef cap) (zoomAtlasDataRef env)

    it "keeps the palette and its runtime handle table on their own refs" $ \env → do
      -- texPaletteRef (persisted path<->id) and texPaletteHandlesRef
      -- (session-local id->TextureHandle) are read together in the SAME
      -- expression by structure.unresolvedPaletteIds, and both are
      -- session-replaced by the same load publish. They have different
      -- element types, so a swap is not a type error at the field level
      -- but WOULD be at the use site -- the real hazard is a projection
      -- that bound one of them to the other's live ref, which typechecks
      -- nowhere else and is caught only here.
      let cap = toRenderHandoffCapability env
      sameContainer (rhTexPaletteRef cap)        (texPaletteRef env)
      sameContainer (rhTexPaletteHandlesRef cap) (texPaletteHandlesRef env)

    it "keeps both structure-art catalogues on their own refs, beside \
       \the palette" $ \env → do
      -- structureWallCatalogRef (#1712) and structureArtCatalogRef
      -- (#1842) are the third and fourth texture-identity refs on this
      -- record, and both are the odd ones out: boot-process rather than
      -- session-replaced, precisely BECAUSE neither is keyed by a
      -- palette id — one by texture PATH, one by PACK NAME — so both
      -- survive the palette replacement a load performs. They answer
      -- DIFFERENT questions about the same packs (how a PLACED wall
      -- rotates; what an UNPLACED piece would be built with). Binding
      -- either to a palette ref is a type error, but binding a palette
      -- ref to one of THEM, or binding these two to each other, is
      -- caught only here — and a projection that minted either fresh
      -- would leave Lua registering into one container while every
      -- reader consulted another: no rotation and no resolution at all,
      -- with no error anywhere.
      let cap = toRenderHandoffCapability env
      sameContainer (rhStructureWallCatalogRef cap) (structureWallCatalogRef env)
      sameContainer (rhStructureArtCatalogRef cap)  (structureArtCatalogRef env)
