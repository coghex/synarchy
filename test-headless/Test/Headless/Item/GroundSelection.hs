-- | @item.select(gid)@ validates its argument against the ACTIVE page's
--   ground items and reports the outcome (#2300).
--
--   The registered Lua verb used to write whatever integer it was given
--   straight into 'World.Cursor.Types.selectedGroundItem' and push no
--   result at all, so every id was \"the selected ground item\" and no
--   caller could tell a real selection from a stale one. This gates the
--   boundary itself, one refusal shape per example; the deferred
--   context-menu path that the refusals exist for is
--   'Test.Headless.UI.ItemInfoRowSelection'.
--
--   Two live pages, active first, because ground items are PAGE-LOCAL
--   (#1208): a gid that exists only on another page is a different item
--   entirely and must be refused exactly like one that exists nowhere.
--   That case is unreachable with a single page, and it is the one a
--   \"look the id up somewhere\" implementation would get wrong.
--
--   Every example asserts the RESULT and the resulting selection: a
--   refusal that changed the selection anyway, and a refusal that
--   cleared it, both look identical from the boolean alone.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match \"ground item selection\"'@.
module Test.Headless.Item.GroundSelection (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sort)
import Control.Concurrent
    (forkIO, newEmptyMVar, putMVar, readMVar, takeMVar, withMVar)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import System.Timeout (timeout)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItems(..), spawnGroundItem)
import Item.Types (ItemInstance(..), emptyItemManager)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import World.Cursor.Types (CursorState(..))
import World.GroundItems (selectGroundItemOnPage, takeGroundItemOnPage)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager
    , emptyWorldState )

-- * Fixture identities

-- | The ACTIVE page: the only one @item.select@ may ever select from.
pageActive ∷ WorldPageId
pageActive = WorldPageId "ground_selection_active"

-- | A live but NOT active page. Its items are the off-page decoys.
pageOther ∷ WorldPageId
pageOther = WorldPageId "ground_selection_other"

-- | The active page's two items. The first is the standing selection
--   every refusal has to leave alone; the second proves a refusal does
--   not merely leave the LAST thing selected by accident.
liveGid, otherLiveGid ∷ Int
liveGid = 0
otherLiveGid = 1

-- | A gid no page holds.
missingGid ∷ Int
missingGid = 77

-- | The gid that exists ONLY on 'pageOther'. Its page holds one item
--   more than the active one, so this number is live somewhere and
--   absent from the page @item.select@ resolves against -- exactly the
--   shape a session-wide lookup would wrongly accept.
offPageGid ∷ Int
offPageGid = 2

spec ∷ Spec
spec = around (withIsolatedResourceRoot . withHeadlessEngineNoWorld) $ do

    describe "item.select" $ do

        it "selects a live item of the active page and reports true" $ \env → do
            (ls, sc) ← selectionBackend env
            select ls otherLiveGid `shouldReturn` "true"
            selectionOn (scActive sc) `shouldReturn` Just otherLiveGid

        it "refuses a gid the active page has no item for, leaving the \
           \previous selection" $ \env → do
            (ls, sc) ← selectionBackend env
            select ls liveGid `shouldReturn` "true"
            select ls missingGid `shouldReturn` "false"
            selectionOn (scActive sc) `shouldReturn` Just liveGid

        it "refuses a gid that exists only on another page, leaving the \
           \previous selection and that page untouched" $ \env → do
            (ls, sc) ← selectionBackend env
            select ls liveGid `shouldReturn` "true"
            select ls offPageGid `shouldReturn` "false"
            selectionOn (scActive sc) `shouldReturn` Just liveGid
            -- The other page is never written either: a selection is
            -- the ACTIVE page's, and a verb that resolved the gid
            -- globally would have had to record it somewhere.
            selectionOn (scOther sc) `shouldReturn` Nothing

        it "refuses an argument that is not a number, leaving the \
           \previous selection" $ \env → do
            (ls, sc) ← selectionBackend env
            select ls liveGid `shouldReturn` "true"
            evalOk ls "return item.select('not a gid')" `shouldReturn` "false"
            evalOk ls "return item.select({})" `shouldReturn` "false"
            evalOk ls "return item.select()" `shouldReturn` "false"
            selectionOn (scActive sc) `shouldReturn` Just liveGid

        it "refuses everything when no page is active, touching no \
           \page's selection" $ \env → do
            (ls, sc) ← selectionBackend env
            select ls liveGid `shouldReturn` "true"
            retireEveryPage env
            select ls liveGid `shouldReturn` "false"
            select ls missingGid `shouldReturn` "false"
            -- The pages are held directly, so their cursors are still
            -- readable after the manager stopped naming them.
            selectionOn (scActive sc) `shouldReturn` Just liveGid
            selectionOn (scOther sc) `shouldReturn` Nothing

    -- The atomicity claim, gated rather than asserted. A check of
    -- 'wsGroundItemsRef' followed by an independent write to
    -- 'wsCursorRef' is two steps on two IORefs, and a removal landing
    -- between them commits a selection for an item that is already
    -- gone. Both operations take the page's lock for the whole
    -- read-decide-write, so neither can run beside the other -- which
    -- is what these two observe: with the lock held, a lock-free
    -- implementation finishes immediately and a locked one cannot
    -- finish at all.
    describe "the page's ground-item lock" $ do

        it "makes selection wait for a removal that is in progress" $ \env → do
            (_, sc) ← selectionBackend env
            let ws = scActive sc
            committed ← runUnderHeldLock ws
                (selectGroundItemOnPage ws liveGid)
                (selectionOn ws `shouldReturn` Nothing)
            committed `shouldBe` True
            selectionOn ws `shouldReturn` Just liveGid

        it "makes removal wait for a selection that is in progress" $ \env → do
            (_, sc) ← selectionBackend env
            let ws = scActive sc
            removed ← runUnderHeldLock ws
                (takeGroundItemOnPage ws liveGid)
                (groundIds ws `shouldReturn` [liveGid, otherLiveGid])
            isJust removed `shouldBe` True
            groundIds ws `shouldReturn` [otherLiveGid]

-- * Driving the boundary

-- | Fork @act@ against @ws@ while THIS thread holds that page's
--   ground-item lock, assert @whileBlocked@ about the untouched state,
--   assert @act@ has not finished, then release and return its result.
--
--   The negative half is what a missing lock fails: the forked call
--   would complete in microseconds and @whileBlocked@ would already be
--   looking at its effect.
runUnderHeldLock ∷ Show α ⇒ WorldState → IO α → IO () → IO α
runUnderHeldLock ws act whileBlocked = do
    started ← newEmptyMVar
    result  ← newEmptyMVar
    withMVar (wsGroundItemLock ws) $ \_ → do
        _ ← forkIO $ do
                putMVar started ()
                putMVar result =≪ act
        -- The worker is running, so a completion from here on is the
        -- operation's own, not a thread that never got scheduled.
        takeMVar started
        early ← timeout blockedProbeMicros (readMVar result)
        isNothing early `shouldBe` True
        whileBlocked
    finished ← timeout releaseProbeMicros (takeMVar result)
    case finished of
        Just r  → pure r
        Nothing → do
            expectationFailure "the operation never completed after the \
                               \lock was released"
            error "unreachable"

-- | Long enough that a lock-free implementation has finished many
--   times over, short enough to keep this spec in the milliseconds the
--   rest of it runs in.
blockedProbeMicros ∷ Int
blockedProbeMicros = 200000

-- | Generous, because this one only bounds a hang: the operation is
--   unblocked by the time it is consulted.
releaseProbeMicros ∷ Int
releaseProbeMicros = 5000000

select ∷ LuaBackendState → Int → IO Text
select ls gid = evalOk ls ("return item.select(" <> tshow gid <> ")")

-- * Live-state readers and edits

selectionOn ∷ WorldState → IO (Maybe Int)
selectionOn ws = selectedGroundItem <$> readIORef (wsCursorRef ws)

groundIds ∷ WorldState → IO [Int]
groundIds ws = sort ∘ HM.keys ∘ gisItems <$> readIORef (wsGroundItemsRef ws)

-- | Empty the world manager entirely, which is the only state
--   'Engine.Core.State.resolveActiveWorld' answers @Nothing@ to: an
--   empty @wmVisible@ alone falls back to the first live page, so
--   hiding is NOT the same as having no active page.
retireEveryPage ∷ EngineEnv → IO ()
retireEveryPage env = writeIORef (worldManagerRef env) emptyWorldManager

-- * Fixture

-- | Two in-memory pages, active first. Each allocates ground-item ids
--   from its own zero, so the same-numbered gid on both is the
--   DEFAULT rather than something the fixture contrives.
data Scene = Scene { scActive ∷ WorldState, scOther ∷ WorldState }

installPages ∷ EngineEnv → IO Scene
installPages env = do
    wsA ← emptyWorldState
    wsO ← emptyWorldState
    a0 ← atomicModifyIORef' (wsGroundItemsRef wsA) $
             spawnGroundItem (mkItem 700) 1 1
    a1 ← atomicModifyIORef' (wsGroundItemsRef wsA) $
             spawnGroundItem (mkItem 701) 2 2
    -- pageOther holds one item MORE, so its last id is a number the
    -- active page has none of: the off-page-only gid.
    oIds ← forM [800, 801, 802] $ \iid →
        atomicModifyIORef' (wsGroundItemsRef wsO) $
            spawnGroundItem (mkItem iid) 3 3
    -- Pins what the identities above assume about a fresh page's
    -- allocator, so a change there fails here rather than silently
    -- seeding ids the assertions do not name.
    (a0, a1) `shouldBe` (liveGid, otherLiveGid)
    oIds `shouldBe` [0, 1, offPageGid]
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageActive, wsA), (pageOther, wsO)]
        , wmVisible = [pageActive] }
    writeIORef (itemManagerRef env) emptyItemManager
    pure (Scene wsA wsO)

selectionBackend ∷ EngineEnv → IO (LuaBackendState, Scene)
selectionBackend env = do
    sc ← installPages env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure (ls, sc)

-- | Nothing about the instance is read by @item.select@; only its
--   presence in @gisItems@ is.
mkItem ∷ Word64 → ItemInstance
mkItem iid = ItemInstance
    { iiDefName = "ground_selection_marker", iiCurrentFill = 0
    , iiQuality = 100, iiCondition = 100, iiWeight = 1.0
    , iiSharpness = 100, iiContents = [], iiInstanceId = iid
    , iiTemp = Nothing, iiBulk = Just 1.0, iiStorage = Nothing
    }

-- * Lua plumbing

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    got ← executeDebugLua (lbsLuaState ls) src
    when ("error:" `T.isPrefixOf` got ∨ "syntax error:" `T.isPrefixOf` got) $
        expectationFailure ("Lua error from " ⧺ show src ⧺ ": " ⧺ T.unpack got)
    pure got
