-- | The versioned paged map artifact (issue #2693, world-map epic
--   #2017, design slice WML-7): the manifest and page-storage contract
--   for a world's mandatory root/coarse map coverage and its optional
--   fine pages.
--
--     * "World.ZoomMap.PagedArtifact.Types" — the compatibility
--       descriptor, format constants and the structured refusal type.
--     * "World.ZoomMap.PagedArtifact.Png" — the lossless RGBA8 PNG page
--       codec, gated before decode.
--     * "World.ZoomMap.PagedArtifact.Format" — the manifest and page-file
--       byte layouts, pure.
--     * "World.ZoomMap.PagedArtifact.Store" — publication and reading
--       through "World.GeneratedLibrary", and fine-page cache files.
--
--   Nothing here is activated: world creation, save loading and
--   rendering do not call it yet (WML-8, WML-9, WML-10), and the
--   separate monolithic reconstruction cache "World.ZoomMap.Artifact"
--   is unchanged. The contract is documented in
--   @docs/world_map_paged_artifact_format.md@.
module World.ZoomMap.PagedArtifact
    ( module World.ZoomMap.PagedArtifact.Types
    , module World.ZoomMap.PagedArtifact.Png
    , module World.ZoomMap.PagedArtifact.Format
    , module World.ZoomMap.PagedArtifact.Store
    ) where

import World.ZoomMap.PagedArtifact.Types
import World.ZoomMap.PagedArtifact.Png
import World.ZoomMap.PagedArtifact.Format
import World.ZoomMap.PagedArtifact.Store
