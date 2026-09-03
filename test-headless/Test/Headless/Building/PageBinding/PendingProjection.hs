{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | QUEUED-selection projection for the "Build placement page binding"
--   (#1602) gate: a selection change ENQUEUED before the click but
--   applied after it. Comparing generations alone reports "fresh"
--   there, so the projection predicts the queued verbs IN ORDER —
--   invalidating for the ones whose handler will really move the head,
--   leaving a binding alone for the ones that will not, staying stale
--   across a half-drained dependent sequence, and settling honestly
--   once the world thread applies or refuses each command.
--
--   'noOpSelectionRequests' — the ten-entry matrix pinning every
--   selection verb in a configuration its own handler turns into a
--   no-op — is this owner's, not generic support: nothing else
--   consumes it.
--
--   These are fixture-CONSUMING fragments: the engine, the Lua backend
--   and the isolated resource root are the façade's
--   ("Test.Headless.Building.PageBinding").
module Test.Headless.Building.PageBinding.PendingProjection
    ( pendingSpec
    ) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.Text as T

import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Building.PageBinding.Support
    ( aimAt, armBuildTool, canPlaceAt, clearStubs, clickAt
    , commitOutcomes, committedPlacements, evalDebug, expectStale, pageA
    , pageB, placeTile, portalName, resetScene, resetSceneBothVisible
    , runOneWorldCommand, runWorldQueue, selectionGen, shedName
    , terrainZA, terrainZB )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), projectedVisible, selectionChangeInFlight)
import World.Thread.Command.Init (handleWorldInitArenaCommand)

-- | The window round 3 named: a selection change ENQUEUED before the
--   click but applied after it. Comparing generations alone reports
--   "fresh" there — the world thread has not applied anything yet — so
--   the placement would be accepted synchronously and then correctly
--   dropped at the commit, leaving the build tool having recorded an
--   acceptance for a building that never landed. The pending count
--   closes it: the rejection is SYNCHRONOUS, which is what lets the tool
--   record the required outcome and stay armed.
pendingSpec ∷ SpecWith (EngineEnv, LuaBackendState)
pendingSpec =
  describe "a selection change ENQUEUED but not yet applied" $ do

    it "makes canPlaceAt report the binding stale before the world \
       \thread has touched anything" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.hide('", unWorldPageId pageA, "'); return 'queued'" ]
        -- Nothing has been applied: the page is still visible and the
        -- generation has not moved.
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldBe` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

    it "rejects the starting-building click, records the outcome and \
       \leaves placement armed" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.hide('", unWorldPageId pageA, "'); return 'queued'" ]
        (px, py) ← aimAt env placeTile terrainZA
        _ ← clickAt ls (px, py)
        expectStale env wsA wsB ls

    it "rejects the construction.designate click the same way" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← evalDebug ls $ T.concat
                [ "world.show('", unWorldPageId pageB, "'); return 'queued'" ]
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "counts world.initArenaDone too, whose handler also prepends to \
       \wmVisible" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        -- The one selection-changing verb that used to enqueue its
        -- command directly: an already-registered hidden arena's
        -- initArenaDone queued ahead of a click would make the visible
        -- page change under a placement the synchronous check had just
        -- called fresh.
        _ ← evalDebug ls $ T.concat
            [ "world.initArenaDone('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        (wmSelectionPending <$> readIORef (worldManagerRef env))
            `shouldReturn` 1
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

    it "rejects a click queued behind world.initArenaDone, recording the \
       \outcome and staying armed" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.initArenaDone('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        (px, py) ← aimAt env placeTile terrainZA
        _ ← clickAt ls (px, py)
        expectStale env wsA wsB ls

    it "discharges every selection-changing verb it counts" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            -- One of each, then one drain: a verb that incremented
            -- without a matching handler discharge would leave the
            -- count stuck above zero and wedge every later binding.
            _ ← evalDebug ls $ T.concat
                [ "world.hide('", unWorldPageId pageA, "'); "
                , "world.show('", unWorldPageId pageA, "'); "
                , "world.initArenaDone('", unWorldPageId pageB, "'); "
                , "world.destroy('", unWorldPageId pageB, "'); "
                , "return 'queued'" ]
            (wmSelectionPending <$> readIORef (worldManagerRef env))
                `shouldReturn` 4
            runWorldQueue env
            (wmSelectionPending <$> readIORef (worldManagerRef env))
                `shouldReturn` 0

    it "does NOT invalidate a binding for an INEFFECTIVE request" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            -- Showing the already-visible page, and hiding the already
            -- hidden one: ordinary traffic that moves no selection. A
            -- click must still be accepted (requirement 12's
            -- no-page-switch path), even though both are still queued.
            _ ← evalDebug ls $ T.concat
                [ "world.show('", unWorldPageId pageA, "'); "
                , "world.hide('", unWorldPageId pageB, "'); "
                , "return 'queued'" ]
            (wmSelectionPending <$> readIORef (worldManagerRef env))
                `shouldReturn` 2
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "true|nil|false"

    it "commits a click made while only INEFFECTIVE requests are in \
       \flight" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageA, "'); return 'queued'" ]
        (px, py) ← aimAt env placeTile terrainZA
        _ ← clickAt ls (px, py)
        outs ← commitOutcomes ls
        outs `shouldBe` ["accepted|nil"]
        committedPlacements env `shouldReturn`
            [(portalName, fst placeTile, snd placeTile, pageA)]

    it "judges a DEPENDENT sequence in queue order, not against the \
       \applied list" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        -- show(B) then hide(B). Against the APPLIED list the hide looks
        -- like a no-op — B is not visible yet — so judging there would
        -- call the pair harmless. Against the projection the show makes
        -- B visible first, so the hide is a real change.
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageB, "'); "
            , "world.hide('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        mgr0 ← readIORef (worldManagerRef env)
        -- The projection walked show-then-hide in order and came back
        -- to [A]; both were counted as real changes.
        snd (projectedVisible mgr0) `shouldBe` [pageA]
        selectionChangeInFlight mgr0 `shouldBe` True
        -- Drain HALFWAY: the show has landed, the hide has not. The
        -- projection must still report a change in flight — this is the
        -- exact window a placement would be accepted in and then
        -- dropped at the commit.
        runOneWorldCommand env
        mgr1 ← readIORef (worldManagerRef env)
        wmVisible mgr1 `shouldBe` [pageB, pageA]
        wmSelectionPending mgr1 `shouldBe` 1
        gen ← selectionGen env
        canPlaceAt ls shedName placeTile (Just (pageB, gen))
            `shouldReturn` "false|page binding stale|true"
        -- And it settles honestly once the hide lands.
        runWorldQueue env
        mgr2 ← readIORef (worldManagerRef env)
        wmVisible mgr2 `shouldBe` [pageA]
        wmProjectedGen mgr2 `shouldBe` wmSelectionGen mgr2

    it "rejects a click made in that half-drained window" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageB, "'); "
            , "world.hide('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        runOneWorldCommand env
        -- The show has landed, so page B is what a click now hit-tests;
        -- aim at ITS terrain so the pick resolves and the rejection is
        -- the binding's, not an off-world miss.
        (px, py) ← aimAt env placeTile terrainZB
        _ ← clickAt ls (px, py)
        expectStale env wsA wsB ls

    it "does NOT invalidate a binding for a destroy that touches no \
       \visible page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        -- A page that does not exist, and one that is registered but
        -- HIDDEN. Neither is what any binding names — a pick only ever
        -- resolves the visible head — so neither may cost a click.
        _ ← evalDebug ls $ T.concat
            [ "world.destroy('bind_page_missing'); "
            , "world.destroy('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"
        runWorldQueue env
        -- Still fresh once they have actually been applied.
        selectionGen env `shouldReturn` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "DOES invalidate for a destroy of the visible page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.destroy('", unWorldPageId pageA, "'); return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

    it "does NOT invalidate a binding when a HIDDEN page is \
       \re-initialised" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        -- The visible-page counterpart of this is asserted below; a
        -- hidden page's replacement leaves the binding alone.
        handleWorldInitArenaCommand env logger pageB
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldBe` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "ignores a hide/destroy/re-init of a visible page that is NOT \
       \the head" $ \(env, ls) → do
        _ ← resetSceneBothVisible env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        -- Page B is visible but sits BEHIND A, so nothing done to it can
        -- move the page a binding names. None of these may cost a click,
        -- queued or applied.
        _ ← evalDebug ls $ T.concat
            [ "world.hide('", unWorldPageId pageB, "'); return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"
        runWorldQueue env
        selectionGen env `shouldReturn` gen
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        -- Re-initialising a non-head visible page, likewise.
        _ ← resetSceneBothVisible env
        gen2 ← selectionGen env
        handleWorldInitArenaCommand env logger pageB
        selectionGen env `shouldReturn` gen2
        canPlaceAt ls shedName placeTile (Just (pageA, gen2))
            `shouldReturn` "true|nil|false"
        -- And destroying one.
        _ ← resetSceneBothVisible env
        gen3 ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.destroy('", unWorldPageId pageB, "'); return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen3))
            `shouldReturn` "true|nil|false"
        runWorldQueue env
        selectionGen env `shouldReturn` gen3

    it "DOES invalidate when the HEAD of a multi-visible list is hidden" $
        \(env, ls) → do
            _ ← resetSceneBothVisible env
            _ ← clearStubs ls
            gen ← selectionGen env
            -- Same list, but now the page removed IS the head, so the
            -- page a binding names really does change — to B.
            _ ← evalDebug ls $ T.concat
                [ "world.hide('", unWorldPageId pageA, "'); return 'queued'" ]
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "false|page binding stale|true"
            runWorldQueue env
            mgr ← readIORef (worldManagerRef env)
            wmVisible mgr `shouldBe` [pageB]
            selectionGen env `shouldNotReturn` gen

    -- One table rather than an example per verb: every selection verb,
    -- in a configuration where its OWN handler will change no selection,
    -- must leave a live binding alone — queued AND once applied. Adding
    -- a verb, or narrowing one's precondition, without extending the
    -- prediction fails here instead of in review.
    describe "no selection verb invalidates a binding when its handler \
             \will change nothing" $
      forM_ noOpSelectionRequests $ \(label, luaCall) →
        it label $ \(env, ls) → do
            _ ← resetSceneBothVisible env
            _ ← clearStubs ls
            gen ← selectionGen env
            _ ← evalDebug ls (luaCall <> " return 'queued'")
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "true|nil|false"
            runWorldQueue env
            selectionGen env `shouldReturn` gen
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "true|nil|false"

    it "still invalidates for a show that a queued init makes REAL" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            -- The show alone would be refused (the page is not
            -- registered), but the init ahead of it registers the page,
            -- so the show WILL prepend and move the head. Predicting
            -- from the applied registration set would miss this.
            _ ← evalDebug ls
                "world.initArena('bind_page_new'); \
                \world.show('bind_page_new'); return 'queued'"
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "false|page binding stale|true"

    it "heals after a request the handler REFUSES, so a later \
       \ineffective one still costs nothing" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        -- A show for a page that does not exist: predicted effective,
        -- refused by the handler. Without the settle the projection
        -- would stay ahead of the applied generation for good, and the
        -- redundant show below would then read as a change in flight.
        _ ← evalDebug ls "world.show('bind_page_missing'); return 'queued'"
        runWorldQueue env
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageA, "'); return 'queued'" ]
        mgr ← readIORef (worldManagerRef env)
        selectionChangeInFlight mgr `shouldBe` False
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "settles once the world thread applies the change" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageB, "'); return 'queued'" ]
        runWorldQueue env
        (wmSelectionPending <$> readIORef (worldManagerRef env))
            `shouldReturn` 0
        -- A binding taken AFTER the change settled is good again.
        gen ← selectionGen env
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageB, pageA]
        canPlaceAt ls shedName placeTile (Just (pageB, gen))
            `shouldReturn` "true|nil|false"

    it "invalidates a binding when a VISIBLE page is re-initialised \
       \under the same id" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        -- world.init/initArena REPLACE the page's WorldState while
        -- leaving wmVisible alone, so the page id still matches: only
        -- the generation can tell the binding apart from a live one.
        handleWorldInitArenaCommand env logger pageA
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldNotBe` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

-- | Every selection-changing verb, paired with a call its own handler
--   will turn into a no-op given 'resetSceneBothVisible' (pages A then
--   B visible, A the head, both registered). None may cost a click.
noOpSelectionRequests ∷ [(String, Text)]
noOpSelectionRequests =
    [ ( "world.show of the page that is already the head"
      , call "world.show" pageA )
    , ( "world.show of a page that is visible but NOT the head"
      , call "world.show" pageB )
    , ( "world.show of a page that is not registered at all"
      , "world.show('bind_page_missing');" )
    , ( "world.hide of a visible page that is not the head"
      , call "world.hide" pageB )
    , ( "world.hide of a page that is not visible"
      , "world.hide('bind_page_missing');" )
    , ( "world.destroy of a visible page that is not the head"
      , call "world.destroy" pageB )
    , ( "world.destroy of a page that does not exist"
      , "world.destroy('bind_page_missing');" )
    , ( "world.initArena replacing a visible page that is not the head"
      , call "world.initArena" pageB )
    , ( "world.initArena registering a brand new page"
      , "world.initArena('bind_page_new');" )
    , ( "world.initArenaDone for the page that is already the head"
      , call "world.initArenaDone" pageA )
    ]
  where
    call verb (WorldPageId pid) =
        T.concat [verb, "('", pid, "');"]
