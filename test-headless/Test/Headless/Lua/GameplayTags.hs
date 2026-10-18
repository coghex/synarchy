{-# LANGUAGE OverloadedStrings #-}
-- | "Lua gameplay tags" (#2706, SCN-06 of epic #2698): the registered
--   @gameplayTags@ namespace, through REAL Lua calls against the
--   production registration ('Engine.Scripting.Lua.API.registerLuaAPI')
--   and the live owners. Tile and plant edits travel through the real
--   world queue and dispatcher (see "Test.Headless.Lua.GameplayTags.Scene").
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua gameplay tags"'@.
module Test.Headless.Lua.GameplayTags (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified HsLua as Lua

import Building.Types (BuildingId(..))
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API.GameplayTags (TagMarshal(..), productionTagMarshal)
import Engine.Scripting.Lua.API.GameplayTags.Args (maxExpressionDepth)
import Engine.Scripting.Lua.API.Register.GameplayTags (registerGameplayTagAPIWith)
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Index (categoryTargets)
import Gameplay.Tags.Live
import Gameplay.Tags.Page (PageTags(..), emptyPageTags)
import Gameplay.Tags.Query (TagQuery(..), TagFilter(..), tagged, targetSetsToList)
import Gameplay.Tags.Registry (registryInvariantViolations)
import Gameplay.Tags.Types
import Location.Instance (LocationInstanceId(..))
import Test.Headless.Lua.GameplayTags.Scene
import Test.Headless.World.PageIncarnation (drainEntities)
import Unit.Command.Types (UnitCommand(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Edit.Types (WorldEdit(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldState)
import World.Thread.Command.Init (registerPageIncarnation)
import World.Tile.Types (lookupChunk)

spec ∷ Spec
spec = describe "Lua gameplay tags" $ aroundAll withFixture $ do
    operationSpec
    identitySpec
    querySpec
    scalarSpec
    residencySpec
    lifecycleSpec
    refusalSpec
    isolationSpec
    exampleSpec

tg ∷ Text → GameplayTag
tg t = fromMaybe (error "an empty fixture tag") (mkGameplayTag t)

tags ∷ [Text] → Set.Set GameplayTag
tags = Set.fromList . map tg

itemOf ∷ Word64 → TagTarget
itemOf i = TargetItem (fromMaybe (error "item 0") (mkItemTarget i))

-- | Every target form the API covers, by its Lua literal, its typed
--   target, its category name and its page.
forms ∷ [(String, Text, TagTarget, Text)]
forms =
    [ ("a unit", unitT 1, TargetUnit (UnitId 1), "unit")
    , ("a building", buildingT 1, TargetBuilding (BuildingId 1), "building")
    , ("a location", locationT pageP 1, TargetLocation (LocationTarget pageP (LocationInstanceId 1)), "location")
    , ("a carried item", itemT 1, itemOf satchel, "item")
    , ("an item nested one level", itemT 2, itemOf pouch, "item")
    , ("an item nested two levels", itemT 3, itemOf ration, "item")
    , ("an equipped item", itemT 4, itemOf dagger, "item")
    , ("a worn item", itemT 5, itemOf habit, "item")
    , ("a building-stored item", itemT 6, itemOf crate, "item")
    , ("an item nested in stored storage", itemT 7, itemOf crateBar, "item")
    , ("a delivered item", itemT 8, itemOf deliveredBar, "item")
    , ("a ground item above 2^53", itemT (fromIntegral bigItem), itemOf bigItem, "item")
    , ("a wild plant occurrence", plantT pageP oakOcc, occOf pageP oakOcc, "plant")
    , ("a planted row crop", plantT pageP rowCrop, occOf pageP rowCrop, "plant")
    , ("a groundcover plot", plotT pageP plotTile, plotOf pageP plotTile, "plant")
    , ("a surface cell", tileT pageP colTile 10, cell pageP colTile 10, "tile")
    , ("a loaded underground cell", tileT pageP colTile 5, cell pageP colTile 5, "tile")
    ]

-- | Requirements 1–3: every operation on every target form.
operationSpec ∷ SpecWith Fx
operationSpec = describe "every operation on every target form" $
    forM_ forms $ \(label, lt, target, cat) →
        it ("add, remove, replace, clear, tagsOf and has on " <> label) $ \fx → do
            resetScene fx
            let call verb rest = run fx ("gameplayTags." <> verb <> "(" <> lt <> rest <> ")")
                opts = "{ categories = '" <> cat <> "', page = '" <> pageText pageP <> "' }"
            call "add" ", { 'a', 'b', 'a' }" `shouldReturn` "true"
            call "tagsOf" "" `shouldReturn` "[a,b]"
            call "has" ", 'a'" `shouldReturn` "true"
            call "has" ", 'c'" `shouldReturn` "false"
            assignmentOf fx target `shouldReturn` Just (tags ["a", "b"])
            -- The query returns exactly this target, and the returned
            -- table round-trips into inspection without being rebuilt.
            run fx ("gameplayTags.query({ tag = 'a' }, " <> opts <> ")")
                `shouldReturn` renderList [target]
            runRaw fx ("local r = gameplayTags.query({ tag = 'b' }, " <> opts <> "); \
                       \return __r(gameplayTags.tagsOf(r[1]))") `shouldReturn` "[a,b]"
            call "add" ", 'c'" `shouldReturn` "true"
            call "add" ", 'c'" `shouldReturn` "true"
            call "tagsOf" "" `shouldReturn` "[a,b,c]"
            call "remove" ", { 'b', 'never' }" `shouldReturn` "true"
            call "tagsOf" "" `shouldReturn` "[a,c]"
            call "replace" ", { 'x' }" `shouldReturn` "true"
            call "tagsOf" "" `shouldReturn` "[x]"
            -- Empty replacement removes the entry from both indexes.
            call "replace" ", {}" `shouldReturn` "true"
            call "tagsOf" "" `shouldReturn` "[]"
            call "has" ", 'x'" `shouldReturn` "false"
            assignmentOf fx target `shouldReturn` Nothing
            call "add" ", 'y'" `shouldReturn` "true"
            call "clear" "" `shouldReturn` "true"
            call "clear" "" `shouldReturn` "true"
            call "tagsOf" "" `shouldReturn` "[]"
            assignmentOf fx target `shouldReturn` Nothing
            v ← view fx
            registryInvariantViolations (liveTagRegistry v) `shouldBe` []

-- | Requirement 3: identities.
identitySpec ∷ SpecWith Fx
identitySpec = describe "target identity" $ do
    it "the same id in four categories, and on two pages, names different targets" $ \fx → do
        resetScene fx
        forM_ [unitT 1, buildingT 1, itemT 1, locationT pageP 1, locationT pageQ 1] $ \t →
            run fx ("gameplayTags.add(" <> t <> ", 'shared')") `shouldReturn` "true"
        let onP = [ TargetUnit (UnitId 1), TargetBuilding (BuildingId 1)
                  , TargetLocation (LocationTarget pageP (LocationInstanceId 1)), itemOf satchel ]
        run fx "gameplayTags.query({ tag = 'shared' })" `shouldReturn` renderList onP
        run fx ("gameplayTags.query({ tag = 'shared' }, { page = '" <> pageText pageQ <> "' })")
            `shouldReturn` renderList [TargetLocation (LocationTarget pageQ (LocationInstanceId 1))]
        run fx ("gameplayTags.remove(" <> unitT 1 <> ", 'shared')") `shouldReturn` "true"
        run fx "gameplayTags.query({ tag = 'shared' })" `shouldReturn` renderList (drop 1 onP)
        run fx ("gameplayTags.tagsOf(" <> buildingT 1 <> ")") `shouldReturn` "[shared]"

    it "a plot, the plants on a tile and the cells under them are distinct targets" $ \fx → do
        resetScene fx
        forM_ [ plotT pageP plotTile, tileT pageP plotTile 10, plantT pageP oakOcc
              , plantT pageP berryOcc, tileT pageP colTile 10 ] $ \t →
            run fx ("gameplayTags.add(" <> t <> ", 'here')") `shouldReturn` "true"
        v ← view fx
        let expected = targetSetsToList (runLiveTagQueryIn [PlantCategory, TileCategory] v
                                            (onPage v pageP) (tagged (tg "here")))
        length expected `shouldBe` 5
        run fx "gameplayTags.query({ tag = 'here' }, { categories = { 'plant', 'tile' } })"
            `shouldReturn` renderList expected

    it "ids above 2^53 cross the boundary exactly, as Lua integers" $ \fx → do
        resetScene fx
        run fx ("gameplayTags.add(" <> itemT 9007199254740993 <> ", 'big')") `shouldReturn` "true"
        run fx ("gameplayTags.add(" <> plantT pageP rowCrop <> ", 'big')") `shouldReturn` "true"
        run fx "gameplayTags.query({ tag = 'big' }, { categories = { 'item', 'plant' } })"
            `shouldReturn` renderList [occOf pageP rowCrop, itemOf bigItem]
        runRaw fx "local r = gameplayTags.query({ tag = 'big' }, { categories = 'item' }); \
                  \return math.type(r[1].id) .. ' ' .. tostring(r[1].id == 9007199254740993)"
            `shouldReturn` "integer true"
        -- The neighbouring ids name nothing.
        run fx ("gameplayTags.tagsOf(" <> itemT 9007199254740992 <> ")")
            `shouldReturn` "nil|gameplayTags.tagsOf: no such live target (it does not exist, or is not loaded)"

    it "a seam alias of a cell or plot names the canonical target" $ \fx → do
        resetScene fx
        let s = worldSize * chunkSize `div` 2
            alias (x, y) = (x + s, y - s)
        run fx ("gameplayTags.add(" <> tileT pageP (alias colTile) 10 <> ", 'aliased')")
            `shouldReturn` "true"
        run fx ("gameplayTags.add(" <> plotT pageP (alias plotTile) <> ", 'aliased')")
            `shouldReturn` "true"
        run fx ("gameplayTags.tagsOf(" <> tileT pageP colTile 10 <> ")") `shouldReturn` "[aliased]"
        run fx ("gameplayTags.tagsOf(" <> plotT pageP plotTile <> ")") `shouldReturn` "[aliased]"
        run fx "gameplayTags.query({ tag = 'aliased' })"
            `shouldReturn` renderList [plotOf pageP plotTile, cell pageP colTile 10]

    it "a returned target keeps its page whichever page is active when it is reused" $ \fx → do
        resetScene fx
        run fx ("gameplayTags.add(" <> tileT pageQ colTile 10 <> ", 'far_page')") `shouldReturn` "true"
        _ ← runRaw fx ("_G.__kept = gameplayTags.query({ tag = 'far_page' }, { page = '"
                       <> pageText pageQ <> "' })[1]; return 'ok'")
        -- Page P's cell at the same coordinate is a different target.
        run fx ("gameplayTags.tagsOf(" <> tileT pageP colTile 10 <> ")") `shouldReturn` "[]"
        run fx "gameplayTags.remove(__kept, 'far_page')" `shouldReturn` "true"
        assignmentOf fx (cell pageQ colTile 10) `shouldReturn` Nothing

-- | Requirement 5: the native algebra, over units.
querySpec ∷ SpecWith Fx
querySpec = describe "queries" $ do
    let squad fx = do
            resetScene fx
            forM_ [ (1, ["defender", "ranged"]), (5, ["defender", "ranged", "reserve"])
                  , (6, ["defender"]) ] $ \(u, ts) →
                run fx ("gameplayTags.add(" <> unitT u <> ", { "
                        <> T.intercalate ", " [ "'" <> t <> "'" | t ← ts ] <> " })")
                    `shouldReturn` "true"
        units = renderList . map (TargetUnit . UnitId)
        cases =
            [ ("{ tag = 'defender' }", QMatch (TagFilter (tags ["defender"]) Set.empty Set.empty), [1, 5, 6])
            , ("{ difference = { { intersection = { { tag = 'defender' }, { tag = 'ranged' } } }, { tag = 'reserve' } } }"
              , QDifference (QIntersection (tagged (tg "defender")) (tagged (tg "ranged"))) (tagged (tg "reserve")), [1])
            , ("{ all = { 'defender', 'ranged' }, none = { 'reserve' } }"
              , QMatch (TagFilter (tags ["defender", "ranged"]) Set.empty (tags ["reserve"])), [1])
            , ("{ complement = { tag = 'defender' } }", QComplement (tagged (tg "defender")), [2])
            , ("{ complement = { tag = 'never' } }", QComplement (tagged (tg "never")), [1, 2, 5, 6])
            , ("{ any = { 'reserve', 'never' } }", QMatch (TagFilter Set.empty (tags ["reserve", "never"]) Set.empty), [5])
            , ("{ union = { { tag = 'reserve' }, { complement = { tag = 'defender' } } } }"
              , QUnion (tagged (tg "reserve")) (QComplement (tagged (tg "defender"))), [2, 5])
            , ("{ intersection = { { tag = 'defender' } } }", tagged (tg "defender"), [1, 5, 6])
            , ("{ universe = true }", QUniverse, [1, 2, 5, 6])
            , ("{ all = {} }", QMatch (TagFilter Set.empty Set.empty Set.empty), [1, 2, 5, 6])
            , ("{ none = 'defender' }", QMatch (TagFilter Set.empty Set.empty (tags ["defender"])), [2])
            , ("{ complement = { complement = { tag = 'ranged' } } }", QComplement (QComplement (tagged (tg "ranged"))), [1, 5])
            , ("{ tag = 'never' }", tagged (tg "never"), [])
            ]
    forM_ cases $ \(expr, native, expected) →
        it ("list, count and exists agree with the native evaluation for " <> T.unpack expr) $ \fx → do
            squad fx
            let opts = "{ categories = 'unit' }"
            run fx ("gameplayTags.query(" <> expr <> ", " <> opts <> ")") `shouldReturn` units expected
            v ← view fx
            renderList (targetSetsToList (runLiveTagQueryIn [UnitCategory] v (onPage v pageP) native))
                `shouldBe` units expected
            run fx ("gameplayTags.count(" <> expr <> ", " <> opts <> ")") `shouldReturn` tshow (length expected)
            run fx ("gameplayTags.exists(" <> expr <> ", " <> opts <> ")")
                `shouldReturn` (if null expected then "false" else "true")

    it "selects one, several or all categories, in the documented order" $ \fx → do
        resetScene fx
        forM_ [unitT 1, buildingT 1, itemT 1, tileT pageP colTile 10, plantT pageP oakOcc] $ \t →
            run fx ("gameplayTags.add(" <> t <> ", 'shared')") `shouldReturn` "true"
        let u = TargetUnit (UnitId 1); b = TargetBuilding (BuildingId 1)
            p = occOf pageP oakOcc; i = itemOf satchel; c = cell pageP colTile 10
        run fx "gameplayTags.query({ tag = 'shared' }, { categories = 'item' })" `shouldReturn` renderList [i]
        run fx "gameplayTags.query({ tag = 'shared' }, { categories = { 'tile', 'unit', 'item', 'unit' } })"
            `shouldReturn` renderList [u, i, c]
        run fx "gameplayTags.query({ tag = 'shared' })" `shouldReturn` renderList [u, b, p, i, c]
        run fx "gameplayTags.count({ tag = 'shared' })" `shouldReturn` "5"

    it "an empty universe answers an empty list, zero and false" $ \fx → do
        resetScene fx
        let opts = "{ categories = 'building', page = '" <> pageText pageQ <> "' }"
        run fx ("gameplayTags.query({ universe = true }, " <> opts <> ")") `shouldReturn` "[]"
        run fx ("gameplayTags.count({ complement = { tag = 'x' } }, " <> opts <> ")") `shouldReturn` "0"
        run fx ("gameplayTags.exists({ universe = true }, " <> opts <> ")") `shouldReturn` "false"

    it "negative tile queries return every untagged live cell, untruncated" $ \fx → do
        resetScene fx
        run fx ("gameplayTags.add(" <> tileT pageP colTile 10 <> ", 'meeting_point')") `shouldReturn` "true"
        v ← view fx
        let live = Set.size (Set.filter ((≡ pageP) . tileTargetPage) (liveTileTargets v))
        run fx "gameplayTags.count({ complement = { tag = 'meeting_point' } }, { categories = 'tile' })"
            `shouldReturn` tshow (live - 1)
        runRaw fx "return tostring(#gameplayTags.query({ none = 'meeting_point' }, { categories = 'tile' }))"
            `shouldReturn` tshow (live - 1)

-- | Requirement 6: count and exists marshal no target list.
scalarSpec ∷ SpecWith Fx
scalarSpec = describe "scalar modes" $
    it "count and exists push no target, through the production verbs with a counting seam" $ \fx → do
        resetScene fx
        forM_ [1, 5, 6] $ \u →
            run fx ("gameplayTags.add(" <> unitT u <> ", 'defender')") `shouldReturn` "true"
        let env = fxEnv fx
        pushes ← newIORef (0 ∷ Int)
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env) (assetPoolRef env)
                                   (nextObjectIdRef env) (inputStateRef env) (loggerRef env)
        stats ← newLuaCallStats
        let counting = TagMarshal $ \t → do
                Lua.liftIO (modifyIORef' pushes (+ 1))
                tmPushTarget productionTagMarshal t
        Lua.runWith (lbsLuaState ls) (registerGameplayTagAPIWith counting stats env)
        let eval = executeDebugLua (lbsLuaState ls)
        eval "gameplayTags.count({ tag = 'defender' }, { categories = 'unit' })" `shouldReturn` "3"
        eval "gameplayTags.exists({ tag = 'defender' }, { categories = 'unit' })" `shouldReturn` "true"
        eval "gameplayTags.count({ universe = true })" ≫= (`shouldNotBe` "0")
        readIORef pushes `shouldReturn` 0
        eval "#gameplayTags.query({ tag = 'defender' }, { categories = 'unit' })" `shouldReturn` "3"
        readIORef pushes `shouldReturn` 3

-- | Requirement 4: the loaded-target contract.
residencySpec ∷ SpecWith Fx
residencySpec = describe "loaded targets" $ do
    it "an evicted target is neither editable nor visible, keeps its tags, and is live again \
       \once readmitted" $ \fx → do
        resetScene fx
        let far = tileT pageP farTile 10
        run fx ("gameplayTags.add(" <> far <> ", 'far')") `shouldReturn` "true"
        run fx ("gameplayTags.add(" <> plantT pageP farOak <> ", 'far')") `shouldReturn` "true"
        evictAAndC fx
        before ← assignments fx
        run fx ("gameplayTags.tagsOf(" <> far <> ")")
            `shouldReturn` "nil|gameplayTags.tagsOf: no such live target (it does not exist, or is not loaded)"
        run fx ("gameplayTags.add(" <> far <> ", 'more')")
            `shouldReturn` "nil|gameplayTags.add: no such live target (it does not exist, or is not loaded)"
        run fx ("gameplayTags.has(" <> plantT pageP farOak <> ", 'far')")
            `shouldReturn` "nil|gameplayTags.has: no such live target (it does not exist, or is not loaded)"
        run fx "gameplayTags.query({ tag = 'far' })" `shouldReturn` "[]"
        run fx "gameplayTags.exists({ tag = 'far' })" `shouldReturn` "false"
        assignments fx `shouldReturn` before
        assignmentOf fx (cell pageP farTile 10) `shouldReturn` Just (tags ["far"])
        readmitC fx
        run fx ("gameplayTags.tagsOf(" <> far <> ")") `shouldReturn` "[far]"
        run fx "gameplayTags.query({ tag = 'far' })"
            `shouldReturn` renderList [occOf pageP farOak, cell pageP farTile 10]

    it "air, unstored and ungenerated cells are refused without loading anything" $ \fx → do
        resetScene fx
        before ← assignments fx
        forM_ [ tileT pageP colTile 7, tileT pageP colTile 4, tileT pageP colTile 11
              , tileT pageP (5 * 16 + 1, 5 * 16 + 1) 10 ] $ \t → do
            run fx ("gameplayTags.add(" <> t <> ", 'nope')")
                `shouldReturn` "nil|gameplayTags.add: no such live target (it does not exist, or is not loaded)"
            run fx ("gameplayTags.tagsOf(" <> t <> ")") ≫= (`shouldSatisfy` T.isPrefixOf "nil|")
        assignments fx `shouldReturn` before
        wm ← readIORef (worldManagerRef (fxEnv fx))
        ws ← maybe (fail "no page") pure (lookup pageP (wmWorlds wm))
        td ← readIORef (wsTilesRef ws)
        lookupChunk (ChunkCoord 5 5) td `shouldSatisfy` isNothing
        v ← view fx
        Set.member (cell pageP (81, 81) 10) (Set.map TargetTile (liveTileTargets v)) `shouldBe` False

    it "a page with no params, or one that is not live, answers a refusal" $ \fx → do
        resetScene fx
        run fx "gameplayTags.add({ kind = 'tile', page = 'nowhere', x = 1, y = 1, z = 10 }, 'a')"
            `shouldReturn` "nil|gameplayTags.add: no live page 'nowhere'"
        run fx "gameplayTags.query({ tag = 'a' }, { page = 'nowhere' })"
            `shouldReturn` "nil|gameplayTags.query: no live page 'nowhere'"

-- | Requirement 2 and 8: destruction, replacement and stale work.
lifecycleSpec ∷ SpecWith Fx
lifecycleSpec = describe "destruction, replacement and stale work" $ do
    it "a destroyed unit and a dug-away cell drop out; the exposed cell keeps its own tags" $ \fx → do
        resetScene fx
        run fx ("gameplayTags.add(" <> unitT 6 <> ", 'doomed')") `shouldReturn` "true"
        run fx ("gameplayTags.add(" <> tileT pageP colTile 10 <> ", 'top')") `shouldReturn` "true"
        run fx ("gameplayTags.add(" <> tileT pageP colTile 9 <> ", 'below')") `shouldReturn` "true"
        Q.writeQueue (unitQueue (fxEnv fx)) (UnitDestroy (UnitId 6))
        drainEntities (fxEnv fx)
        run fx ("gameplayTags.tagsOf(" <> unitT 6 <> ")") ≫= (`shouldSatisfy` T.isPrefixOf "nil|")
        run fx "gameplayTags.query({ tag = 'doomed' })" `shouldReturn` "[]"
        assignmentOf fx (TargetUnit (UnitId 6)) `shouldReturn` Nothing
        editChunkA fx (WeDeleteTile (fst colTile) (snd colTile))
        run fx ("gameplayTags.tagsOf(" <> tileT pageP colTile 10 <> ")") ≫= (`shouldSatisfy` T.isPrefixOf "nil|")
        run fx ("gameplayTags.tagsOf(" <> tileT pageP colTile 9 <> ")") `shouldReturn` "[below]"
        run fx "gameplayTags.query({ tag = 'top' })" `shouldReturn` "[]"

    it "an edit queued before its page is re-initialised is refused and never reaches the \
       \replacement" $ \fx → do
        resetScene fx
        fresh ← emptyWorldState
        armTagEditHook fx $ registerPageIncarnation (fxEnv fx) pageP $ \m →
            m { wmWorlds = [ if p ≡ pageP then (p, fresh) else (p, w) | (p, w) ← wmWorlds m ] }
        run fx ("gameplayTags.add(" <> tileT pageP colTile 10 <> ", 'stale')")
            `shouldReturn` "nil|gameplayTags.add: no such live target (it does not exist, or is not loaded)"
        readIORef (wsPageTagsRef fresh) `shouldReturn` emptyPageTags

    it "an edit queued before its page is destroyed is refused" $ \fx → do
        resetScene fx
        armTagEditHook fx $ atomicModifyIORef' (worldManagerRef (fxEnv fx)) $ \m →
            (m { wmWorlds = filter ((≢ pageP) . fst) (wmWorlds m), wmVisible = [pageQ] }, ())
        run fx ("gameplayTags.add(" <> plantT pageP oakOcc <> ", 'stale')")
            `shouldReturn` "nil|gameplayTags.add: no such live target (it does not exist, or is not loaded)"
        run fx "gameplayTags.query({ tag = 'stale' })" `shouldReturn` "[]"

    it "a completed edit is visible to the next read, from the same Lua chunk" $ \fx → do
        resetScene fx
        runRaw fx ("gameplayTags.add(" <> tileT pageP colTile 10 <> ", 'now'); \
                   \return tostring(gameplayTags.has(" <> tileT pageP colTile 10 <> ", 'now'))")
            `shouldReturn` "true"
        ws ← fmap (lookup pageP . wmWorlds) (readIORef (worldManagerRef (fxEnv fx)))
        pt ← maybe (fail "page") (readIORef . wsPageTagsRef) ws
        Set.map TargetTile (categoryTargets (ptTiles pt)) `shouldBe` Set.singleton (cell pageP colTile 10)

-- | Requirement 7: validation before mutation or evaluation.
refusalSpec ∷ SpecWith Fx
refusalSpec = describe "refusals" $ do
    let refusals =
            -- argument counts
            [ ("gameplayTags.add()", "add: expected 2 arguments, got 0")
            , ("gameplayTags.add(" <> unitT 1 <> ")", "add: expected 2 arguments, got 1")
            , ("gameplayTags.add(" <> unitT 1 <> ", 'a', 3)", "add: expected 2 arguments, got 3")
            , ("gameplayTags.clear()", "clear: expected 1 argument, got 0")
            , ("gameplayTags.query()", "query: expected 1 or 2 arguments, got 0")
            -- target discriminators and fields
            , ("gameplayTags.add('unit', 'a')", "add: a target must be a table, got a string")
            , ("gameplayTags.add({ id = 1 }, 'a')", "add: a target's kind must be a string, got nil")
            , ("gameplayTags.add({ kind = 'robot', id = 1 }, 'a')", "add: unknown target kind 'robot'")
            , ("gameplayTags.add({ kind = 'unit' }, 'a')", "add: a unit target is missing its 'id' field")
            , ("gameplayTags.add({ kind = 'unit', id = 1, page = 'x' }, 'a')", "add: a unit target has an unknown field 'page'")
            , ("gameplayTags.add({ kind = 'unit', id = 1, colour = 'red' }, 'a')", "add: a target has an unknown field 'colour'")
            , ("gameplayTags.add({ kind = 'unit', id = 1, [1] = 2 }, 'a')", "add: a target has a key that is a number")
            , ("gameplayTags.add({ kind = 'tile', x = 1, y = 2 }, 'a')", "add: a tile target is missing its 'z' field")
            , ("gameplayTags.add({ kind = 'plot', x = 1, y = 2, z = 3 }, 'a')", "add: a plot target has an unknown field 'z'")
            , ("gameplayTags.add({ kind = 'plant', id = 1, page = '' }, 'a')", "add: a plant target's page must not be empty")
            -- numbers
            , ("gameplayTags.add({ kind = 'unit', id = '1' }, 'a')", "add: a unit target's id must be an integer, got a string")
            , ("gameplayTags.add({ kind = 'unit', id = 1.5 }, 'a')", "add: a unit target's id must be an integer; a float")
            , ("gameplayTags.add({ kind = 'unit', id = 0/0 }, 'a')", "add: a unit target's id must be an integer; a float")
            , ("gameplayTags.add({ kind = 'unit', id = math.huge }, 'a')", "add: a unit target's id must be an integer; a float")
            , ("gameplayTags.add({ kind = 'item', id = 9007199254740994.0 }, 'a')", "add: an item target's id must be an integer; a float")
            , ("gameplayTags.add({ kind = 'unit', id = -1 }, 'a')", "add: a unit target's id is out of range 0 .. 4294967295")
            , ("gameplayTags.add({ kind = 'unit', id = 4294967296 }, 'a')", "add: a unit target's id is out of range")
            , ("gameplayTags.add({ kind = 'item', id = 0 }, 'a')", "add: an item target's id is out of range 1 ..")
            , ("gameplayTags.add({ kind = 'plant', id = 1 << 63 }, 'a')", "add: a plant target's id is out of range")
            , ("gameplayTags.add({ kind = 'tile', x = 2147483648, y = 0, z = 0 }, 'a')", "add: a tile target's x is out of range")
            -- tag collections
            , ("gameplayTags.add(" <> unitT 1 <> ", '')", "add: a tag must not be the empty string")
            , ("gameplayTags.add(" <> unitT 1 <> ", 5)", "add: tags must be a string or a list of strings, got a number")
            , ("gameplayTags.add(" <> unitT 1 <> ", { 'ok', 2 })", "add: a tag must be a string, got a number")
            , ("gameplayTags.add(" <> unitT 1 <> ", { 'ok', '\\255' })", "add: a tag is not valid UTF-8")
            , ("gameplayTags.add(" <> unitT 1 <> ", { x = 'a' })", "add: a tag list must be a list")
            , ("gameplayTags.add(" <> unitT 1 <> ", { 'a', nil, 'b' })", "add: a tag list must be a list")
            , ("gameplayTags.has(" <> unitT 1 <> ", { 'a' })", "has: a tag must be a string, got a table")
            -- expressions
            , ("gameplayTags.query('a')", "query: an expression must be a table, got a string")
            , ("gameplayTags.query({})", "query: an expression node takes exactly one of")
            , ("gameplayTags.query({ tag = 'a', union = {} })", "query: an expression node takes exactly one of")
            , ("gameplayTags.query({ tag = 'a', all = 'b' })", "query: an expression node takes exactly one of")
            , ("gameplayTags.query({ op = 'union' })", "query: an expression has an unknown field 'op'")
            , ("gameplayTags.query({ union = {} })", "query: union needs at least one operand")
            , ("gameplayTags.query({ intersection = {} })", "query: intersection needs at least one operand")
            , ("gameplayTags.query({ difference = { { tag = 'a' } } })", "query: difference needs exactly two operands")
            , ("gameplayTags.query({ complement = 'a' })", "query: an expression must be a table, got a string")
            , ("gameplayTags.query({ universe = false })", "query: universe must be true")
            , ("gameplayTags.query({ tag = '' })", "query: a tag must not be the empty string")
            , ("gameplayTags.query({ union = { { tag = 'a' }, 7 } })", "query: an expression must be a table, got a number")
            -- query options
            , ("gameplayTags.query({ tag = 'a' }, 'unit')", "query: query options must be a table, got a string")
            , ("gameplayTags.query({ tag = 'a' }, { categories = {} })", "query: the category list must name at least one category")
            , ("gameplayTags.query({ tag = 'a' }, { categories = 'plot' })", "query: unknown category 'plot'")
            , ("gameplayTags.query({ tag = 'a' }, { categories = { kind = 'unit' } })", "query: the category list must be a list")
            , ("gameplayTags.count({ tag = 'a' }, { limit = 3 })", "count: query options has an unknown field 'limit'")
            , ("gameplayTags.exists({ tag = 'a' }, { page = 3 })", "exists: the query page must be a string, got a number")
            ]
    it "every malformed request answers nil and a diagnostic, changes nothing, and the next \
       \call succeeds" $ \fx → do
        resetScene fx
        run fx ("gameplayTags.add(" <> unitT 1 <> ", 'kept')") `shouldReturn` "true"
        before ← assignments fx
        forM_ refusals $ \(call, prefix) → do
            out ← run fx call
            unless (("nil|gameplayTags." <> prefix) `T.isPrefixOf` out) $
                expectationFailure (T.unpack (call <> "\n  expected nil|gameplayTags." <> prefix <> "…\n  got " <> out))
            assignments fx `shouldReturn` before
        -- No partial mutation: the valid 'ok' beside the invalid tag was
        -- not added.
        run fx ("gameplayTags.tagsOf(" <> unitT 1 <> ")") `shouldReturn` "[kept]"
        run fx ("gameplayTags.add(" <> unitT 1 <> ", 'after')") `shouldReturn` "true"
        run fx ("gameplayTags.tagsOf(" <> unitT 1 <> ")") `shouldReturn` "[after,kept]"

    it "integral floats up to 2^53 are integers; above it they are refused" $ \fx → do
        resetScene fx
        run fx "gameplayTags.add({ kind = 'unit', id = 1.0 }, 'f')" `shouldReturn` "true"
        run fx "gameplayTags.tagsOf({ kind = 'item', id = 9007199254740992.0 })"
            `shouldReturn` "nil|gameplayTags.tagsOf: no such live target (it does not exist, or is not loaded)"
        run fx "gameplayTags.tagsOf({ kind = 'item', id = 2.0^60 })"
            ≫= (`shouldSatisfy` T.isPrefixOf "nil|gameplayTags.tagsOf: an item target's id must be an integer; a float")

    it "a cyclic expression is refused, a shared subexpression is not" $ \fx → do
        resetScene fx
        runRaw fx "local e = { union = {} }; e.union[1] = e; return __r(gameplayTags.query(e))"
            `shouldReturn` "nil|gameplayTags.query: the expression is cyclic: a table contains itself"
        runRaw fx "local ops = {}; ops[1] = { union = ops }; return __r(gameplayTags.query({ union = ops }))"
            `shouldReturn` "nil|gameplayTags.query: the expression is cyclic: a table contains itself"
        runRaw fx "local leaf = { tag = 'never' }; \
                  \return __r(gameplayTags.count({ union = { leaf, { complement = leaf } } }, { categories = 'unit' }))"
            `shouldReturn` "4"

    it "nesting is bounded by depth and by visited elements, with a diagnostic" $ \fx → do
        resetScene fx
        let nest n = "local e = { tag = 'x' }; for i = 1, " <> tshow n
                     <> " do e = { complement = e } end; return __r(gameplayTags.count(e, { categories = 'unit' }))"
        runRaw fx (nest (maxExpressionDepth - 1)) ≫= (`shouldSatisfy` (not . T.isPrefixOf "nil|"))
        runRaw fx (nest maxExpressionDepth)
            `shouldReturn` "nil|gameplayTags.count: the expression nests deeper than 64 levels"
        runRaw fx (nest (200000 ∷ Int))
            `shouldReturn` "nil|gameplayTags.count: the expression nests deeper than 64 levels"
        runRaw fx "local e = { tag = 'x' }; for i = 1, 20 do e = { union = { e, e } } end; \
                  \return __r(gameplayTags.exists(e))"
            `shouldReturn` "nil|gameplayTags.exists: the expression visits more than 65536 elements"

    it "decoding runs no metamethod" $ \fx → do
        resetScene fx
        _ ← runRaw fx "_G.__mm = false; _G.__trap = { __index = function() _G.__mm = true; return 'unit' end, \
                      \__len = function() _G.__mm = true; return 9 end, \
                      \__pairs = function() _G.__mm = true; return next, {}, nil end, \
                      \__eq = function() _G.__mm = true; return true end }; return 'ok'"
        run fx "gameplayTags.add(setmetatable({ id = 1 }, __trap), 'm')"
            `shouldReturn` "nil|gameplayTags.add: a target's kind must be a string, got nil"
        run fx ("gameplayTags.add(" <> unitT 1 <> ", setmetatable({ 'm' }, __trap))") `shouldReturn` "true"
        run fx "gameplayTags.count(setmetatable({ tag = 'm' }, __trap), setmetatable({ categories = 'unit' }, __trap))"
            `shouldReturn` "1"
        run fx "gameplayTags.query({ union = setmetatable({ { tag = 'm' } }, __trap) }, { categories = 'unit' })"
            `shouldReturn` renderList [TargetUnit (UnitId 1)]
        runRaw fx "return tostring(_G.__mm)" `shouldReturn` "false"

    it "the registry and the Lua stack stay usable across thousands of refusals" $ \fx → do
        resetScene fx
        runRaw fx ("for i = 1, 5000 do gameplayTags.add({ kind = 'unit', id = '1' }, 'a'); \
                   \gameplayTags.query({ union = {} }) end; \
                   \return __r(gameplayTags.add(" <> unitT 2 <> ", 'ok'))") `shouldReturn` "true"

-- | Requirement 7 and 8: detached results and faction isolation.
isolationSpec ∷ SpecWith Fx
isolationSpec = describe "isolation" $ do
    it "editing a returned table changes nothing in the engine" $ \fx → do
        resetScene fx
        run fx ("gameplayTags.add(" <> unitT 1 <> ", { 'a', 'b' })") `shouldReturn` "true"
        before ← assignments fx
        runRaw fx "local r = gameplayTags.query({ tag = 'a' }); r[1].id = 2; r[1].kind = 'building'; \
                  \r[2] = { kind = 'unit', id = 5 }; \
                  \local t = gameplayTags.tagsOf({ kind = 'unit', id = 1 }); t[1] = 'zzz'; t[3] = 'c'; \
                  \return 'ok'" `shouldReturn` "ok"
        assignments fx `shouldReturn` before
        run fx "gameplayTags.query({ tag = 'a' })" `shouldReturn` renderList [TargetUnit (UnitId 1)]
        run fx ("gameplayTags.tagsOf(" <> unitT 1 <> ")") `shouldReturn` "[a,b]"

    it "general-tag writes leave faction policy state unchanged" $ \fx → do
        resetScene fx
        let env = fxEnv fx
            factionState = do
                cat ← show ⊚ readIORef (factionCatalogueRef env)
                um ← readIORef (unitManagerRef env)
                pure (cat, [ show (uiFaction u) | u ← HM.elems (umInstances um) ])
        before ← factionState
        relation ← runRaw fx "return faction.relation('player', 'wildlife')"
        forM_ [1, 2, 3] $ \u → do
            run fx ("gameplayTags.add(" <> unitT u <> ", { 'player', 'wildlife', 'hostile', 'ally' })")
                `shouldReturn` "true"
            run fx ("gameplayTags.replace(" <> unitT u <> ", 'enemy')") `shouldReturn` "true"
        factionState `shouldReturn` before
        runRaw fx "return faction.relation('player', 'wildlife')" `shouldReturn` relation

-- | Requirement 10: the reference's examples, run as written.
exampleSpec ∷ SpecWith Fx
exampleSpec = describe "the documented examples" $
    it "scripts/gameplay_tag_examples.lua runs against the production API" $ \fx → do
        resetScene fx
        forM_ [ (1, "{ 'defender', 'ranged' }"), (5, "{ 'defender', 'ranged', 'reserve' }")
              , (6, "'defender'") ] $ \(u, ts) →
            run fx ("gameplayTags.add(" <> unitT u <> ", " <> ts <> ")") `shouldReturn` "true"
        let ex = "require('scripts.gameplay_tag_examples')."
            p = "'" <> pageText pageP <> "'"
        run fx (ex <> "markMeetingPoint(" <> p <> ", 3, 4, 10)")
            `shouldReturn` renderList [cell pageP colTile 10]
        run fx (ex <> "markMeetingPoint(nil, 6, 9, 10)")
            `shouldReturn` renderList [cell pageP colTile 10, cell pageP otherTile 10]
        run fx (ex <> "rangedDefenders(" <> p <> ")") `shouldReturn` renderList [TargetUnit (UnitId 1)]
        run fx (ex <> "rangedDefendersShorthand(" <> p <> ")") `shouldReturn` renderList [TargetUnit (UnitId 1)]
        -- Unit 2 carries no tag at all and is still selected.
        run fx (ex <> "nonDefenders(" <> p <> ")") `shouldReturn` renderList [TargetUnit (UnitId 2)]
        runRaw fx ("local n, any = " <> ex <> "rangedDefenderCount(" <> p <> "); \
                   \return tostring(n) .. ',' .. tostring(any)") `shouldReturn` "1,true"
        runRaw fx ("local ex = " <> T.dropEnd 1 ex <> "; \
                   \local from = gameplayTags.query({ tag = 'meeting_point' }, { categories = 'tile' })[1]; \
                   \return __r(ex.moveMeetingPoint(from, { kind = 'tile', x = 10, y = 2, z = 10 }))")
            `shouldReturn` "true"
        run fx "gameplayTags.query({ tag = 'meeting_point' })"
            `shouldReturn` renderList [cell pageP otherTile 10, cell pageP plotTile 10]
