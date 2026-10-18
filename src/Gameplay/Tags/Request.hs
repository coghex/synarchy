-- | Asking the world thread for a tile or plant tag edit (#2705), from
--   any other thread.
--
--   The edit itself is 'Gameplay.Tags.PageLive.mutatePageTargetTags',
--   which must run on the world thread (see "Gameplay.Tags.Page" for
--   why). This resolves the target's page and incarnation, queues
--   'World.Command.Types.WorldEditPageTags' and waits for the answer.
--   The @gameplayTags@ Lua verbs (#2706) call it for every tile and plant
--   edit, and wait for the answer.
module Gameplay.Tags.Request
    ( requestPageTagEdit
    , requestPageTagEditAt
    ) where

import UPrelude
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Gameplay.Tags.Edit (TagEdit, TagEditResult(..))
import Gameplay.Tags.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration)
import World.Page.Types (WorldPageId)
import World.Command.Types (WorldCommand(..), TagEditReply(..))
import World.State.Types (WorldManager(..))

-- | Queue one edit on the world thread and wait for its result. A target
--   that is not a tile or a plant, or whose page is not live, answers
--   'TagEditNoTarget' without queueing anything. Must not be called ON
--   the world thread, which would wait for itself.
requestPageTagEdit ∷ WorldSimCapability → TagTarget → TagEdit → IO TagEditResult
requestPageTagEdit wsc target edit = case targetPage target of
    Nothing → pure TagEditNoTarget
    Just page → do
        mgr ← readIORef (wsWorldManagerRef wsc)
        case lookup page (wmWorlds mgr) of
            Nothing → pure TagEditNoTarget
            Just ws → do
                epoch ← pageIncarnation ws
                requestPageTagEditAt wsc page epoch target edit
  where
    targetPage = \case
        TargetTile t  → Just (tileTargetPage t)
        TargetPlant p → Just (plantTargetPage p)
        _             → Nothing

-- | 'requestPageTagEdit' for a caller that already resolved the page and
--   read its incarnation, BEFORE it derived anything else from that page
--   (#2706: the Lua verbs canonicalise coordinates against the page they
--   resolved). Carrying that epoch, rather than reading the page again
--   here, is what makes a replacement in between refuse the edit instead
--   of applying it to the replacement.
requestPageTagEditAt ∷ WorldSimCapability → WorldPageId → ChunkGeneration → TagTarget
                     → TagEdit → IO TagEditResult
requestPageTagEditAt wsc page epoch target edit = do
    reply ← newEmptyMVar
    Q.writeQueue (wsWorldQueue wsc)
        (WorldEditPageTags page epoch target edit (TagEditReply reply))
    takeMVar reply
