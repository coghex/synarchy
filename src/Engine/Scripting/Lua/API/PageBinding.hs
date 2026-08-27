{-# LANGUAGE Strict #-}
-- | The page-selection BINDING vocabulary every Lua verb that accepts a
--   @bindGen@ slot shares (#1602, #1686).
--
--   A binding is a page-selection generation a script captured earlier —
--   from @world.pickTile@ for a click, from @building.getActiveIds@ for a
--   per-tick scan — and hands back to the verb that commits the work it
--   started. The verb compares it against the live manager INSIDE the
--   same read that resolves the target page, so page selection cannot
--   move between the check and the resolution.
--
--   It lives here, in neither domain's module, because two unrelated
--   verbs now answer the same question and must answer it with the same
--   word: 'Engine.Scripting.Lua.API.Buildings.Spawn'\'s placement commit
--   and 'Engine.Scripting.Lua.API.Units.Spawn'\'s bound spawn. A second
--   spelling of the refusal would be a second contract for the scripts
--   that test it.
module Engine.Scripting.Lua.API.PageBinding
    ( pageBindingStaleReason
    , bindingStale
    ) where

import UPrelude
import qualified HsLua as Lua
import World.Types (WorldManager, selectionMovedSince)

-- | The rejection reason a stale page binding produces (#1602). ONE
--   spelling, shared by every verb that takes a binding, so the
--   validation and commit halves of a bound operation — and the scripts
--   that branch on the answer — cannot drift apart.
pageBindingStaleReason ∷ Text
pageBindingStaleReason = "page binding stale"

-- | Has page selection moved since the binding was captured, or is a
--   change already on its way (#1602)? 'Nothing' (no binding supplied)
--   is never stale — every unbound caller keeps its exact behaviour.
--
--   'selectionMovedSince' reads both halves from the manager snapshot the
--   CALLER already took, so the check and the page resolution it guards
--   share one read. Its projected half is what makes this answer honest
--   rather than merely optimistic: a @world.hide@ enqueued before this
--   call has not moved the applied generation yet, so comparing that
--   alone would report "fresh" for work the world thread is about to
--   invalidate — and the caller would have recorded an acceptance for
--   something that never landed. An INEFFECTIVE request (a redundant
--   @world.show@) moves neither, so ordinary traffic never costs a
--   click or a tick.
bindingStale ∷ Maybe Lua.Integer → WorldManager → Bool
bindingStale Nothing     _  = False
bindingStale (Just want) wm = selectionMovedSince (fromIntegral want) wm
