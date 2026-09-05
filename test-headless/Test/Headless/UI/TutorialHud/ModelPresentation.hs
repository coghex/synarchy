-- | The Tutorial HUD's rendering of #958's view model (requirements
--   1/3/4): the shipped-YAML and injected trees, the active-row rules,
--   and — via #1941 and #2056 — the renderer-presentation handshake
--   that decides when a sticky row has actually been shown to the
--   player and may retire.
--
--   A fixture-consuming fragment of 'Test.Headless.UI.TutorialHud'.
--   The snapshot helpers below drive the shared UI manager directly and
--   are this owner's alone; the engine, the Lua backend and the
--   per-case reset belong to 'Test.Headless.UI.TutorialHud.Support'.
module Test.Headless.UI.TutorialHud.ModelPresentation (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Tutorial.Types (emptyTutorialRegistry)
import UI.Manager.Presentation (snapshotArmedToken, witnessPresentation)
import Test.Headless.UI.TutorialHud.Support
    ( Fixture, bootAt, decodeOr, evalOk, luaLines, resetFixture, subId
    , treeHelpers )

-- * The presentation boundary (#2056)
--
-- The boundary is a real handshake with the RENDERER, and this fixture
-- has no renderer: @renderUIPages@ bails out headless for want of a
-- bindless texture system, which is exactly the honest answer (a
-- GPU-less engine draws no frame, so it witnesses nothing). So the
-- boundary is crossed here by performing, against the same shared UI
-- manager, EXACTLY the two steps 'UI.Render.renderUIPages' performs —
-- read the manager once, then publish that snapshot's own armed token —
-- rather than by poking the witness to a value of the test's choosing.
--
-- Keeping the two steps separately callable is what makes the
-- stale-evidence cases possible: a snapshot BEGUN before a mutation and
-- FINISHED after it publishes the token it actually held, which is the
-- in-flight frame the boundary has to refuse.

-- | The renderer's read: the token this snapshot is carrying.
beginSnapshot ∷ EngineEnv → IO Word64
beginSnapshot env = snapshotArmedToken ⊚ readIORef (uiManagerRef env)

-- | The renderer's publication, once that snapshot has been rendered.
finishSnapshot ∷ EngineEnv → Word64 → IO ()
finishSnapshot env token =
    atomicModifyIORef' (uiManagerRef env)
                       (\m → (witnessPresentation token m, ()))

-- | One complete, uninterrupted renderer snapshot of whatever is on
--   screen right now.
renderSnapshot ∷ EngineEnv → IO ()
renderSnapshot env = beginSnapshot env ⌦ finishSnapshot env

-- | The shipped shape with its prepare branch ALREADY latched and both
--   subobjectives checked before the branch is ever revealed — #996's
--   case, and the one #1941 retires. Ends with the branch revealed, the
--   panel still collapsed and nothing presented.
stickyBoot ∷ Int → Int → Text
stickyBoot w h = luaLines
    [ bootAt w h "shippedShape()"
    , "tp.setSubobjectiveChecked('prepare_water', true);"
    , "tp.setSubobjectiveChecked('prepare_food', true);"
    , "tp.completeObjective('prepare_expedition');"
    , "tp.completeObjective('place_portal');"
    , "tp.completeObjective('secure_water');"
    ]

-- | Re-bind the three module locals. Every boundary case runs as
--   SEVERAL console chunks — the snapshot has to happen between two of
--   them, from Haskell — and @bootAt@'s locals do not outlive the chunk
--   that declared them.
reModules ∷ Text
reModules = luaLines
    [ "local tp = require('scripts.tutorial_progress');"
    , "local th = require('scripts.tutorial_hud');"
    , "local hud = require('scripts.hud');"
    ]

-- | The three ids of the already-latched prepare branch, comma-joined
--   as every case here reports them.
prepareBranch ∷ Text
prepareBranch = T.intercalate ","
    [ "prepare_expedition", "prepare_water", "prepare_food" ]

-- | 'executeDebugLua' serialises a bare STRING return as JSON, quotes
--   included, so a case that returns one compares against this rather
--   than the raw text (a table return decodes through 'decodeOr'
--   instead and needs none of it).
quoted ∷ Text → Text
quoted t = "\"" <> t <> "\""

-- * Decoded probe shapes

data RowProbe = RowProbe
    { rpId ∷ Text, rpKind ∷ Text, rpLabel ∷ Text, rpTooltip ∷ Text
    , rpMarker ∷ Text
    , rpCompleted ∷ Maybe Bool, rpChecked ∷ Maybe Bool
    , rpBlocks ∷ Bool, rpCaptures ∷ Bool, rpOnClick ∷ Maybe Text
    , rpDepth ∷ Int } deriving (Show, Eq)
instance FromJSON RowProbe where
    parseJSON = withObject "RowProbe" $ \o →
        RowProbe <$> o .: "id" <*> o .: "kind" <*> o .: "label"
                  <*> o .: "tooltip" <*> o .: "marker"
                  <*> o .:? "completed" <*> o .:? "checked"
                  <*> o .: "blocks" <*> o .: "captures" <*> o .:? "onClick"
                  <*> o .: "depth"

data ActiveRowsProbe = ActiveRowsProbe
    { arpRowIds ∷ Text, arpActiveIds ∷ Text } deriving (Show, Eq)
instance FromJSON ActiveRowsProbe where
    parseJSON = withObject "ActiveRowsProbe" $ \o →
        ActiveRowsProbe <$> o .: "rowIds" <*> o .: "activeIds"

data YamlProbe = YamlProbe
    { ypLoaded ∷ Bool, ypFirstIds ∷ Text, ypSecondIds ∷ Text
    , ypThirdIds ∷ Text, ypFirstLabel ∷ Text, ypFirstTooltip ∷ Text
    , ypTreeId ∷ Maybe Text }
instance FromJSON YamlProbe where
    parseJSON = withObject "YamlProbe" $ \o →
        YamlProbe <$> o .: "loaded" <*> o .: "firstIds" <*> o .: "secondIds"
                   <*> o .: "thirdIds" <*> o .: "firstLabel"
                   <*> o .: "firstTooltip" <*> o .:? "treeId"

data ReverseProbe = ReverseProbe { rvDoneIds ∷ Text, rvReopenedIds ∷ Text }
instance FromJSON ReverseProbe where
    parseJSON = withObject "ReverseProbe" $ \o →
        ReverseProbe <$> o .: "doneIds" <*> o .: "reopenedIds"

-- | #1941: the two views a retirement passes through — the build that
--   rendered the sticky rows, and the tick that reported them presented
--   and rebuilt without them.
-- | #2056: one reading of the checklist taken across the presentation
--   boundary — what is laid out, what the model still holds active,
--   whether a completed renderer snapshot has held THESE rows, and how
--   many rebuilds the observed window cost.
data BoundaryProbe = BoundaryProbe
    { bpRowIds ∷ Text, bpActiveIds ∷ Text
    , bpPresented ∷ Bool, bpRebuilds ∷ Int } deriving (Show, Eq)
instance FromJSON BoundaryProbe where
    parseJSON = withObject "BoundaryProbe" $ \o →
        BoundaryProbe <$> o .: "rowIds" <*> o .: "activeIds"
                       <*> o .: "presented" <*> o .: "rebuilds"

spec ∷ SpecWith Fixture
spec = do

    describe "rendering the #958 view model (requirements 1/3/4)" $ do
        it "renders the shipped YAML tree's own labels and tooltips, revealing rows as objectives latch" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ treeHelpers
                , "local hud = require('scripts.hud');"
                , "hud.init(1, 2, 1280, 720); hud.createUI(); hud.visible = true;"
                -- The REAL registry, loaded from the shipped directory.
                , "local loaded = engine.loadTutorialDir('data/tutorials');"
                , "local tp = require('scripts.tutorial_progress'); tp.reset();"
                , "local th = require('scripts.tutorial_hud');"
                , "th.init(); th.reflow(1280, 720); th.setOpen(true);"
                , "local first = th.dump();"
                , "tp.completeObjective('first_session_place_portal'); th.rebuild();"
                , "local second = th.dump();"
                , "tp.completeObjective('first_session_secure_water'); th.rebuild();"
                , "local third = th.dump();"
                , "return { loaded = (loaded ~= false),"
                , "         firstIds = table.concat(first.rowIds, ','),"
                , "         secondIds = table.concat(second.rowIds, ','),"
                , "         thirdIds = table.concat(third.rowIds, ','),"
                , "         firstLabel = first.rows[1].label,"
                , "         firstTooltip = first.rows[1].tooltip,"
                , "         treeId = (engine.getTutorialTree() or {}).id }"
                ]
            probe ← decodeOr r ∷ IO YamlProbe
            ypLoaded probe `shouldBe` True
            ypTreeId probe `shouldBe` Just "first_session"
            ypFirstIds probe `shouldBe` "first_session_place_portal"
            -- Labels/tooltips come straight from data/tutorials/first_session.yaml.
            ypFirstLabel probe `shouldBe` "Place portal"
            ypFirstTooltip probe `shouldBe`
                "Zoom in for the build tool, then anchor your colony."
            -- Completing the root REVEALS its child and keeps the root
            -- itself active (its subtree is not done yet).
            ypSecondIds probe `shouldBe`
                "first_session_place_portal,first_session_secure_water"
            -- Completing that child in turn hides the now-finished root
            -- (#958's active-view rule) and reveals the composite with
            -- its live subobjectives. Order is the model's own pre-order
            -- display walk — never re-sorted here.
            ypThirdIds probe `shouldBe` T.intercalate ","
                [ "first_session_secure_water"
                , "first_session_prepare_expedition"
                , "first_session_prepare_water", "first_session_prepare_food" ]
            -- Leave the shared registry as this suite found it.
            writeIORef (tutorialRegistryRef env) emptyTutorialRegistry

        it "keeps latched completion and live subobjective checks distinct, and never renders inactive rows" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "tp.completeObjective('place_portal');"
                , "tp.completeObjective('secure_water');"
                , "tp.setSubobjectiveChecked('prepare_water', true);"
                , "th.setOpen(true);"
                , "local d = th.dump();"
                , "local out = {};"
                , "for i, row in ipairs(d.rows) do"
                , "    out[i] = { id = row.id, kind = row.kind, label = row.label,"
                , "               tooltip = row.tooltip, marker = row.marker,"
                , "               completed = row.completed, checked = row.checked,"
                , "               blocks = row.pointerBlocking,"
                , "               captures = row.scrollCapture,"
                , "               onClick = UI.getElementOnClick(row.handle),"
                , "               depth = row.depth } end;"
                , "return out"
                ]
            rows ← decodeOr r ∷ IO [RowProbe]
            -- 'place_portal' latched AND its only child latched, so it
            -- has left the active view entirely — retained history is
            -- reported by the model with active = false and must never
            -- be rendered here.
            map rpId rows `shouldBe`
                [ "secure_water", "prepare_expedition"
                , "prepare_water", "prepare_food" ]
            let byId i = case [x | x ← rows, rpId x ≡ i] of
                            (x:_) → x
                            []    → error ("no row " ⧺ T.unpack i)
            -- A latched FULL objective reports `completed` and only
            -- `completed`; a live SUBOBJECTIVE reports `checked` and
            -- only `checked`. Collapsing the two is the bug #958 exists
            -- to prevent, so the split is asserted on both sides.
            rpCompleted (byId "secure_water") `shouldBe` Just True
            rpChecked (byId "secure_water") `shouldBe` Nothing
            rpMarker (byId "secure_water") `shouldBe` "[x]"
            rpKind (byId "secure_water") `shouldBe` "full"
            rpKind (byId "prepare_water") `shouldBe` "subobjective"
            rpCompleted (byId "prepare_expedition") `shouldBe` Just False
            rpMarker (byId "prepare_expedition") `shouldBe` "[ ]"
            rpChecked (byId "prepare_water") `shouldBe` Just True
            rpCompleted (byId "prepare_water") `shouldBe` Nothing
            rpMarker (byId "prepare_water") `shouldBe` "(x)"
            rpChecked (byId "prepare_food") `shouldBe` Just False
            rpMarker (byId "prepare_food") `shouldBe` "( )"
            -- Depth drives indentation only; the model's order stands.
            map rpDepth rows `shouldBe` [1, 2, 3, 3]
            -- Requirement 4: display-only rows.
            forM_ rows $ \row → do
                rpBlocks row `shouldBe` False
                rpOnClick row `shouldBe` Nothing
                rpCaptures row `shouldBe` True
                rpTooltip row `shouldSatisfy` (not ∘ T.null)
                rpLabel row `shouldSatisfy` (not ∘ T.null)

        it "drops a row from the active view when its whole subtree is satisfied" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "tp.completeObjective('place_portal');"
                , "tp.completeObjective('secure_water');"
                , "tp.completeObjective('prepare_expedition');"
                , "tp.setSubobjectiveChecked('prepare_water', true);"
                , "tp.setSubobjectiveChecked('prepare_food', true);"
                , "th.setOpen(true);"
                , "local done = th.dump();"
                -- Reversible: unchecking a subobjective brings the
                -- composite (and its chain) straight back.
                , "tp.setSubobjectiveChecked('prepare_food', false); th.rebuild();"
                , "local reopened = th.dump();"
                , "return { doneIds = table.concat(done.rowIds, ','),"
                , "         reopenedIds = table.concat(reopened.rowIds, ',') }"
                ]
            probe ← decodeOr r ∷ IO ReverseProbe
            rvDoneIds probe `shouldBe` ""
            -- Only the composite chain comes back: the two full
            -- objectives above it stay finished, since an unchecked
            -- subobjective does not un-complete anything durable.
            rvReopenedIds probe `shouldBe` T.intercalate ","
                [ "prepare_expedition", "prepare_water", "prepare_food" ]

        -- #996: the composite (and both prepare subobjectives) latches
        -- BEFORE secure_water ever reveals it — the shipped acolyte spawn
        -- kit satisfies both prepare subobjectives on its own. The real
        -- HUD must still show the branch, in authored order, the first
        -- time it is ever revealed rather than rendering nothing.
        it "renders an already-latched prepare branch the first time it \
           \is revealed, instead of an empty checklist" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls $ luaLines
                [ bootAt 1280 720 "shippedShape()"
                , "tp.setSubobjectiveChecked('prepare_water', true);"
                , "tp.setSubobjectiveChecked('prepare_food', true);"
                , "tp.completeObjective('prepare_expedition');"
                , "tp.completeObjective('place_portal');"
                , "tp.completeObjective('secure_water');"
                , "th.setOpen(true); th.rebuild();"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ',') }"
                ]
            probe ← decodeOr r ∷ IO ActiveRowsProbe
            let want = T.intercalate ","
                    [ "prepare_expedition", "prepare_water", "prepare_food" ]
            arpRowIds probe `shouldBe` want
            arpActiveIds probe `shouldBe` want

        -- #1941: and then it RETIRES. This surface is the only thing
        -- that can testify a row was actually put in front of the
        -- player, so #958 hands it that job.
        --
        -- #2056 is what makes that testimony true. Before it, ONE Lua
        -- update tick counted as presentation, and the four cases below
        -- encoded that: the tick showed the page, acknowledged, and
        -- rebuilt without the rows, all inside one uninterrupted call
        -- that no renderer snapshot had to fall between. Every case here
        -- now separates the two halves — nothing retires until a
        -- COMPLETED snapshot has held exactly these rows, and then it
        -- retires promptly.
        it "does NOT retire on the update tick alone — no snapshot has \
           \held the rows, so the branch is still waiting (#2056)" $ \(env, ls) → do
            resetFixture env ls
            built ← evalOk ls $ luaLines
                [ stickyBoot 1280 720
                , "th.setOpen(true);"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ',') }"
                ]
            shown ← decodeOr built ∷ IO ActiveRowsProbe
            -- #996, unchanged: opening renders the whole branch.
            arpRowIds shown `shouldBe` prepareBranch
            arpActiveIds shown `shouldBe` prepareBranch
            -- Ticks are free; frames are not. However many times the
            -- module reports, nothing has been rendered, so nothing may
            -- be acknowledged.
            ticked ← evalOk ls $ luaLines
                [ reModules
                , "local before = th.dump().rebuildCount;"
                , "for _ = 1, 8 do th.update(0) end;"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = d.presented,"
                , "         rebuilds = d.rebuildCount - before }"
                ]
            held ← decodeOr ticked ∷ IO BoundaryProbe
            bpRowIds held `shouldBe` prepareBranch
            bpActiveIds held `shouldBe` prepareBranch
            bpPresented held `shouldBe` False
            bpRebuilds held `shouldBe` 0

        it "retires on the first tick after a completed snapshot has \
           \held exactly those rows (#1941 via #2056)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls $ luaLines
                [ stickyBoot 1280 720, "th.setOpen(true); return true" ]
            -- The renderer sees the open panel.
            renderSnapshot env
            after ← evalOk ls $ luaLines
                [ reModules
                , "local before = th.dump().rebuildCount;"
                , "local presented = th.dump().presented;"
                , "th.update(0);"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = presented,"
                , "         rebuilds = d.rebuildCount - before }"
                ]
            probe ← decodeOr after ∷ IO BoundaryProbe
            -- The boundary really had been crossed before the tick ran.
            bpPresented probe `shouldBe` True
            -- The reporting tick spends the suppression and rebuilds
            -- ONCE: the ordinary completed-history view, an empty
            -- checklist.
            bpActiveIds probe `shouldBe` ""
            bpRowIds probe `shouldBe` ""
            bpRebuilds probe `shouldBe` 1

        it "never acknowledges from a COLLAPSED panel, however many \
           \frames are drawn over it (#1941)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls $ luaLines [ stickyBoot 1280 720, "return true" ]
            -- Ticks AND frames with the panel closed: the HUD is
            -- visible and the branch is active in the model, but the
            -- checklist lays out no rows at all, so nothing is
            -- presented. The frames are what makes this stronger than
            -- the pre-#2056 version, which could only tick.
            suppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            renderSnapshot env
            stillSuppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            suppressed `shouldBe` quoted prepareBranch
            stillSuppressed `shouldBe` quoted prepareBranch
            -- Opened, it renders — and a snapshot taken BEFORE the open
            -- (above) buys nothing, because opening re-armed.
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.setOpen(true); th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            exposed `shouldBe` quoted prepareBranch
            renderSnapshot env
            retired ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            retired `shouldBe` quoted ""

        it "never acknowledges while the gameplay HUD is HIDDEN, and \
           \the rising edge alone is NOT the presentation (#2056)" $ \(env, ls) → do
            resetFixture env ls
            -- Open, but with the whole HUD hidden: the page is not
            -- painted, so a build behind it presents nothing — and
            -- neither do the frames drawn while it is hidden.
            _ ← evalOk ls $ luaLines
                [ stickyBoot 1280 720
                , "hud.visible = false; th.update(0);"
                , "th.setOpen(true); return true" ]
            renderSnapshot env
            suppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            suppressed `shouldBe` quoted prepareBranch
            -- THE DEFECT #2056 CLOSED. The HUD comes back with no
            -- content change of any kind. Before #2056 this one call
            -- showed the page, acknowledged the rows and deleted them
            -- again with no frame in between, retiring a terminal
            -- branch the player had never seen. The rows must survive
            -- the rising edge, and survive every further tick until a
            -- frame has actually drawn them.
            edge ← evalOk ls $ luaLines
                [ reModules
                , "hud.visible = true;"
                , "th.update(0);"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = d.presented,"
                , "         rebuilds = 0 }"
                ]
            edged ← decodeOr edge ∷ IO BoundaryProbe
            bpRowIds edged `shouldBe` prepareBranch
            bpActiveIds edged `shouldBe` prepareBranch
            bpPresented edged `shouldBe` False
            stillHeld ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 5 do th.update(0) end;"
                , "return table.concat(th.dump().activeIds, ',')" ]
            stillHeld `shouldBe` quoted prepareBranch
            -- And then the frame arrives, and it retires.
            renderSnapshot env
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            exposed `shouldBe` quoted ""

        it "never acknowledges a sticky row scrolled OUT of the \
           \viewport, and does once it is scrolled back (#1941)" $ \(env, ls) → do
            resetFixture env ls
            -- Scroll the composite itself off the top. Its
            -- subobjectives stay on screen, so the panel is busy — but
            -- the only STICKY id is no longer laid out, and frames
            -- drawn in that state present it no more than ticks do.
            suppressed ← evalOk ls $ luaLines
                [ bootAt 1024 768 "stickyTree(60)"
                , "preLatch(tp, 60);"
                , "th.setOpen(true);"
                , "th.setScrollOffset(1);"
                , "for _ = 1, 3 do th.update(0) end;"
                , "local d = th.dump();"
                , "return tostring(d.rows[1] ~= nil and d.rows[1].id or 'none')"
                , "    .. '/' .. tostring(d.activeIds[1] or 'none')"
                ]
            renderSnapshot env
            stillSuppressed ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 3 do th.update(0) end;"
                , "local d = th.dump();"
                , "return tostring(d.rows[1] ~= nil and d.rows[1].id or 'none')"
                , "    .. '/' .. tostring(d.activeIds[1] or 'none')"
                ]
            -- The viewport starts at the first subobjective while the
            -- model still reports the sticky composite as active: the
            -- row was never rendered, so it was never presented.
            let offscreen = quoted (subId 1 <> "/branch")
            suppressed `shouldBe` offscreen
            stillSuppressed `shouldBe` offscreen
            -- Back to the top. The sticky row IS in the viewport now,
            -- but the scroll re-armed, so the snapshot above — taken
            -- while it was off screen — authorises nothing.
            stale ← evalOk ls $ luaLines
                [ reModules
                , "th.setScrollOffset(0);"
                , "for _ = 1, 3 do th.update(0) end;"
                , "return table.concat(th.dump().rowIds, ',') ~= ''"
                ]
            stale `shouldBe` "true"
            renderSnapshot env
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            -- Retired: the whole branch leaves the active view, so the
            -- rebuilt viewport is empty.
            exposed `shouldBe` quoted ""

        -- The in-flight frame, which is the case a bare "a frame has
        -- happened since" counter cannot tell apart from a good one.
        -- The snapshot is BEGUN while the open panel is on screen and
        -- FINISHED after a rebuild has destroyed and recreated every
        -- element in it. What it publishes is the token it actually
        -- held, so the rows it never saw stay unacknowledged.
        it "refuses evidence from a snapshot that predates a rebuild \
           \(#2056)" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls $ luaLines
                [ stickyBoot 1280 720, "th.setOpen(true); return true" ]
            inFlight ← beginSnapshot env
            -- The renderer is still working on that snapshot when the
            -- panel rebuilds — same rows, all-new elements.
            _ ← evalOk ls $ luaLines
                [ reModules, "th.reflow(1280, 720); return true" ]
            finishSnapshot env inFlight
            stale ← evalOk ls $ luaLines
                [ reModules
                , "for _ = 1, 5 do th.update(0) end;"
                , "local d = th.dump();"
                , "return { rowIds = table.concat(d.rowIds, ','),"
                , "         activeIds = table.concat(d.activeIds, ','),"
                , "         presented = d.presented, rebuilds = 0 }"
                ]
            probe ← decodeOr stale ∷ IO BoundaryProbe
            bpPresented probe `shouldBe` False
            bpRowIds probe `shouldBe` prepareBranch
            bpActiveIds probe `shouldBe` prepareBranch
            -- A snapshot of the rebuilt list does authorise it.
            renderSnapshot env
            exposed ← evalOk ls $ luaLines
                [ reModules
                , "th.update(0);"
                , "return table.concat(th.dump().rowIds, ',')" ]
            exposed `shouldBe` quoted ""
