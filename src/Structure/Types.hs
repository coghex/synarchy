{-# LANGUAGE Strict #-}
-- | Structural pieces — walls, floors, ceilings — placed in the world by
--   the "structures" debug builder. A first, in-memory store (no save
--   support yet); the data model for the RCT-style edge-wall feature.
--
--   Walls sit on one of the 4 diamond edges of a tile, named by the wall
--   sprite suffix (ne/nw/se/sw). Floors and ceilings occupy the tile top.
--   Each piece is a full 96×64 sprite (the wall art is pre-positioned on
--   its edge within the canvas, transparent elsewhere) plus its own
--   facemap, drawn at the tile exactly like a terrain tile.
module Structure.Types
    ( StructureSlot(..)
    , slotFromText
    , StructurePiece(..)
    , StructurePieceData(..)
    , ChunkStructures
    , emptyChunkStructures
    , StructureStageToken(..)
    , StructureCommitWindow(..)
    , StagedStructurePiece(..)
    , StructureStage(..)
    , emptyStructureStage
    , stageStructurePlacement
    , dropStagedAttempt
    , recordDeclinedAttempt
    , takeDeclinedInWindow
    , clearStagedKey
    , clearStagedAll
    , stagedPieces
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Engine.Asset.Handle (TextureHandle)

-- | Where a piece sits within a tile.
data StructureSlot
    = SFloor | SCeiling
    | SWallNE | SWallNW | SWallSE | SWallSW
    -- ^ Corner posts — one per tile VERTEX (N/E/S/W of the diamond), placed
    --   per-tile (inset toward the tile centre) so an underground wall pressed
    --   against terrain keeps its post inside the tile, not on the shared node.
    | SPostN | SPostE | SPostS | SPostW
    | SWire
    -- ^ Power-grid wire (#359) — occupies the tile top like a floor. Its
    --   rendered sprite is one of the "wire" pack's 16 connection variants,
    --   picked by the placer (scripts/wire.lua) from which cardinal
    --   neighbours also carry wire; this slot itself carries no orientation.
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Parse the Lua-facing slot name. Matches the texture suffixes.
slotFromText ∷ Text → Maybe StructureSlot
slotFromText t = case T.toLower t of
    "floor"   → Just SFloor
    "ceiling" → Just SCeiling
    "wall_ne" → Just SWallNE
    "wall_nw" → Just SWallNW
    "wall_se" → Just SWallSE
    "wall_sw" → Just SWallSW
    "post_n"  → Just SPostN
    "post_e"  → Just SPostE
    "post_s"  → Just SPostS
    "post_w"  → Just SPostW
    "wire"    → Just SWire
    _         → Nothing

-- | A structure piece with RESOLVED runtime texture handles. Built by the
--   renderer from a 'StructurePieceData' (palette ids → handles) as the input
--   to the iso-quad builders; not stored anywhere.
data StructurePiece = StructurePiece
    { spTexture ∷ !TextureHandle   -- ^ the 96×64 sprite
    , spFaceMap ∷ !TextureHandle   -- ^ its facemap (sun shading)
    , spGridZ   ∷ !Int             -- ^ world z it sits at
    } deriving (Show, Eq)

-- | PURE, per-chunk structure piece — texture identity by PALETTE ID (not a
--   runtime handle), so it can be produced by the pure replay (`applyEdit`) and
--   live inside a 'LoadedChunk'. The renderer resolves the ids → handles via
--   the texture palette. (Contrast 'StructurePiece', which holds resolved
--   handles for rendering.)
data StructurePieceData = StructurePieceData
    { spdTexId  ∷ !Int   -- ^ palette id of the sprite texture
    , spdFaceId ∷ !Int   -- ^ palette id of the facemap
    , spdGridZ  ∷ !Int   -- ^ world z it sits at
    } deriving (Show, Eq)

instance NFData StructurePieceData where
    rnf (StructurePieceData a b c) = rnf a `seq` rnf b `seq` rnf c

-- | A chunk's structure overlay: piece keyed by (gx, gy, slot-tag = fromEnum
--   slot). Built by replaying the chunk's structure edits; lives in
--   'LoadedChunk' so it evicts and reloads with the chunk.
type ChunkStructures = HM.HashMap (Int, Int, Word8) StructurePieceData

emptyChunkStructures ∷ ChunkStructures
emptyChunkStructures = HM.empty

-- | Identity of ONE @structure.place@ attempt (#1674).
--
--   The staging cache below is written on the Lua thread and reconciled
--   on the WORLD thread, which can only name a placement by the payload
--   the queued 'World.Command.Types.WorldSetStructure' carries. Key or
--   value equality is not enough to name an attempt: a second
--   @structure.place@ at the same tile and slot with byte-identical
--   texture ids, facemap id and z is a DIFFERENT placement, and a
--   cleanup that matched on those would delete it on behalf of the first
--   one. So each attempt takes a token from its world's own monotonic
--   counter, the command carries it, and the cleanup matches on it.
newtype StructureStageToken = StructureStageToken Word64
    deriving (Show, Eq, Ord)

-- | The half-open span of placement attempts ONE stamp invocation made
--   on ONE page (#2051): @scwFrom@ is the page's next-token watermark
--   read immediately before the builder ran, @scwTo@ the watermark read
--   immediately after it returned.
--
--   That span names the invocation's attempts EXACTLY, and cheaply.
--   Tokens come from the target page's own monotonic counter
--   ('ssNextToken'), and every @structure.place@ that takes one runs on
--   the single Lua thread inside the synchronous builder call, so no
--   other attempt on that page can be allocated between the two reads.
--   A placement rejected BEFORE staging (unparseable slot, omitted
--   paths, unresolvable page, unloaded target) never takes a token at
--   all, so the span holds the ACCEPTED attempts and nothing else.
data StructureCommitWindow = StructureCommitWindow
    { scwFrom ∷ !StructureStageToken   -- ^ first token of the invocation
    , scwTo   ∷ !StructureStageToken   -- ^ one past its last token
    } deriving (Show, Eq)

-- | One staged placement: the piece the builder reads back, tagged with
--   the attempt that staged it.
data StagedStructurePiece = StagedStructurePiece
    { stgToken ∷ !StructureStageToken   -- ^ which attempt staged it
    , stgPiece ∷ !StructurePieceData    -- ^ what a query reports
    } deriving (Show, Eq)

-- | The Lua-thread write-ahead staging cache for one world's structure
--   placements, plus the counter the next attempt's token comes from.
--   The counter lives HERE rather than beside it so allocating a token
--   and staging its entry are one atomic step on one ref — two refs
--   could interleave and hand two attempts the same token.
data StructureStage = StructureStage
    { ssNextToken ∷ !StructureStageToken
      -- ^ Token the next staged attempt takes. Only ever advances:
      --   'clearStagedAll' deliberately keeps it, so a token retired by
      --   a wipe is never handed out again and a stale decline cannot
      --   match a later placement.
    , ssEntries   ∷ !(HM.HashMap (Int, Int, Word8) StagedStructurePiece)
      -- ^ Staged pieces, keyed exactly like 'ChunkStructures'.
    , ssDeclined  ∷ !(S.Set StructureStageToken)
      -- ^ Attempts whose commit the WORLD thread declined (#2051) — the
      --   target chunk evicted between @structure.place@'s residency
      --   check and the world thread's own, so no piece reached
      --   'ChunkStructures' and no edit reached the log.
      --
      --   This is the only record of that outcome: 'ssEntries' cannot
      --   carry it, because a decline RETRACTS the staged entry (#1674),
      --   and the overlay cannot carry it either, because a byte-identical
      --   placement by another attempt is indistinguishable there.
      --   'takeDeclinedInWindow' both reads and prunes it, so it holds
      --   only declines no completion decision has consumed yet: one
      --   'Word64' per eviction race since the last stamp on this page
      --   reached the world thread, and every one of them already logged
      --   a warning on its way in.
    } deriving (Show, Eq)

emptyStructureStage ∷ StructureStage
emptyStructureStage = StructureStage (StructureStageToken 0) HM.empty S.empty

-- | Stage one placement attempt, returning the token that names it. The
--   caller puts that token on the queued command so the world thread can
--   undo exactly this attempt if it declines the commit.
stageStructurePlacement ∷ (Int, Int, Word8) → StructurePieceData → StructureStage
                        → (StructureStage, StructureStageToken)
stageStructurePlacement key piece st =
    let tok@(StructureStageToken n) = ssNextToken st
    in ( st { ssNextToken = StructureStageToken (n + 1)
            , ssEntries   = HM.insert key (StagedStructurePiece tok piece)
                                          (ssEntries st) }
       , tok )

-- | Remove the staged entry for exactly the attempt named by @tok@.
--   A no-op when the key holds nothing, or holds a LATER attempt — that
--   newer placement is its own commit and survives this one's decline.
dropStagedAttempt ∷ (Int, Int, Word8) → StructureStageToken → StructureStage
                  → StructureStage
dropStagedAttempt key tok st = case HM.lookup key (ssEntries st) of
    Just staged | stgToken staged ≡ tok →
        st { ssEntries = HM.delete key (ssEntries st) }
    _ → st

-- | Record that the world thread DECLINED the commit of exactly this
--   attempt (#2051). Called from the same atomic update that retracts
--   the attempt's staged entry, so the two can never disagree.
recordDeclinedAttempt ∷ StructureStageToken → StructureStage → StructureStage
recordDeclinedAttempt tok st = st { ssDeclined = S.insert tok (ssDeclined st) }

-- | Answer one stamp invocation's completion question and retire the
--   declines it just consumed: @True@ iff any attempt in the half-open
--   window was declined.
--
--   Pruning is by WATERMARK, not by the window alone — every recorded
--   token below @scwTo@ is dropped, including ones outside the window.
--   Tokens are handed out in queue order on one thread, so a token below
--   this invocation's end belongs to an attempt whose own completion
--   decision (if it has one) was queued, and therefore dispatched,
--   earlier. Dropping them keeps the set bounded, and dropping THIS
--   window's own declines is what lets the next load's retry be judged
--   on its own attempts rather than inheriting this one's verdict.
takeDeclinedInWindow ∷ StructureCommitWindow → StructureStage
                     → (StructureStage, Bool)
takeDeclinedInWindow (StructureCommitWindow lo hi) st =
    let (consumed, kept) = S.spanAntitone (< hi) (ssDeclined st)
        inWindow         = S.dropWhileAntitone (< lo) consumed
    in (st { ssDeclined = kept }, not (S.null inWindow))

-- | Remove whatever is staged at one key, whichever attempt staged it
--   ('structure.clear' — the player asked for the tile to be empty).
clearStagedKey ∷ (Int, Int, Word8) → StructureStage → StructureStage
clearStagedKey key st = st { ssEntries = HM.delete key (ssEntries st) }

-- | Remove every staged entry ('structure.clearAll'), keeping the token
--   counter where it is — and keeping the recorded declines (#2051).
--   A wipe is the strongest possible reason to withhold an in-flight
--   stamp's completion marker: it removed the very pieces that stamp
--   was placing, so an attempt already known to have failed must not
--   become invisible because the overlay was cleared underneath it.
clearStagedAll ∷ StructureStage → StructureStage
clearStagedAll st = st { ssEntries = HM.empty }

-- | The staged pieces as a plain overlay, for the query paths that read
--   or union the stage against 'ChunkStructures'.
stagedPieces ∷ StructureStage → ChunkStructures
stagedPieces = HM.map stgPiece . ssEntries
