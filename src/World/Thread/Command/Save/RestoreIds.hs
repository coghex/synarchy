{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Save-load restore-id assignment: maps each saved page to a
--   collision-free id in the current session (the active page always
--   restores under the load target; a saved page whose own id collides
--   gets a "<id>#N" suffix). Split out of "World.Thread.Command.Save"
--   (issue #561).
module World.Thread.Command.Save.RestoreIds
    ( dedupPages
    , assignRestoreIds
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import World.Types
import World.Thread.Helpers (unWorldPageId)

-- | Order-preserving de-duplication of a page-id list (keeps the first
--   occurrence). Used to build the restored visibility list without dupes.
dedupPages ∷ [WorldPageId] → [WorldPageId]
dedupPages = go HS.empty
  where go _    []       = []
        go seen (p : ps)
            | p `HS.member` seen = go seen ps
            | otherwise          = p : go (HS.insert p seen) ps

-- | Map every saved page's id to a UNIQUE restore id. The active page (id ==
--   @activeId@) maps to the load target @target@ (always "main_world", the
--   documented convention Lua/headless code assumes); every other page keeps
--   its own id unless that id is already taken — e.g. a non-active page
--   literally named "main_world" would otherwise collide with the active
--   remap and silently overwrite it — in which case it gets a "<id>#N" suffix
--   so no page is dropped. The active page is reserved first so it owns
--   @target@.
--
--   A page that keeps its OWN id may legitimately shadow a live page of the
--   same id (a reload of that page). But a SYNTHETIC "<id>#N" must avoid the
--   current session's live page ids (@liveIds@) too — otherwise a collision
--   rename could land on an unrelated live page, whose WorldState the load then
--   replaces and whose entities it drops, violating the preserve-unrelated-
--   pages guarantee (#191).
assignRestoreIds ∷ WorldPageId → WorldPageId → HS.HashSet WorldPageId
                 → [WorldPageSave] → HM.HashMap WorldPageId WorldPageId
assignRestoreIds target activeId liveIds pages =
    let others = filter ((≢ activeId) . wpsPageId) pages
        go _    []       = []
        go used (w : ws) =
            let base = wpsPageId w
                rid | not (base `HS.member` used) = base   -- keep own id (reload)
                    | otherwise = uniquePageId (used `HS.union` liveIds) base
            in (base, rid) : go (HS.insert rid used) ws
    in HM.fromList ((activeId, target) : go (HS.singleton target) others)

-- | First id in the @base, base#2, base#3, …@ sequence not already in @used@.
--   Terminates because @used@ is finite.
uniquePageId ∷ HS.HashSet WorldPageId → WorldPageId → WorldPageId
uniquePageId used base
    | not (base `HS.member` used) = base
    | otherwise = pick (2 ∷ Int)
  where pick n
            | cand `HS.member` used = pick (n + 1)
            | otherwise             = cand
          where cand = WorldPageId
                    (unWorldPageId base <> "#" <> T.pack (show n))
