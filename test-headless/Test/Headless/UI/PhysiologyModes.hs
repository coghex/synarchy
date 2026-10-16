{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "UI.PhysiologyModes" gate (#2832, S6 of epic #2827): the unit-info
--   Status panel and the legacy debug text show a unit's RESOLVED physiology
--   modes (design D-8), through the SHIPPED panel modules over the real
--   @unit.*@ API and a real 'unitManagerRef'.
--
--   Nothing here fakes the panel: @scripts\/unit_info_v2.lua@ runs its real
--   @update@ over the real @hud@ page, and the rows are read back from what
--   the shipped row builder created. Three recorders wrapped around the
--   engine's @UI@ verbs (text creation, @setColor@, @setTooltipRich@) are
--   how colour and tooltip content, which @UI.getElementInfo@ does not
--   expose, are observed; they change nothing.
--
--   Rendered pixels are NOT claimed here: a headless run has no frames. The
--   offscreen captures for this change are committed under
--   @docs\/pr-proofs\/@ (see the pull request).
module Test.Headless.UI.PhysiologyModes (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (modifyIORef')
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Types
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture (richPhysiology)

-----------------------------------------------------------------------
-- Rows
-----------------------------------------------------------------------

-- | One rendered Status row, as the shipped builder made it.
data Row = Row
    { rowText   ∷ !Text   -- ^ the value label's text
    , rowColour ∷ !Text   -- ^ @r,g,b@ to two places
    , rowTip    ∷ !Text   -- ^ the first line of the VALUE tooltip's hint, or @-@
    , rowIcon   ∷ !Text   -- ^ the first 24 characters of the ICON tooltip's hint
    } deriving (Eq, Show)

neutral ∷ Text
neutral = "1.00,1.00,1.00"

nominalHint ∷ Text
nominalHint = "Not simulated for this unit: always healthy."

-- | Every row of the Status panel, keyed by the name its icon tooltip
--   carries ("Hydration", "Body Temp", "Bleeding", ...).
rowsOf ∷ LuaBackendState → IO (HM.HashMap Text Row)
rowsOf ls = do
    out ← luaText ls "return __dump()"
    pure $ HM.fromList
        [ (name, Row txt col tip icon)
        | part ← T.splitOn "~" out, not (T.null part)
        , [name, txt, col, tip, icon] ← [T.splitOn "|" part] ]

-- | The row named @n@, or the spec fails.
rowNamed ∷ HM.HashMap Text Row → Text → IO Row
rowNamed rs n = case HM.lookup n rs of
    Just r  → pure r
    Nothing → do
        expectationFailure ("no Status row " <> show n <> "; rows: "
                            <> show (HM.keys rs))
        pure (Row "" "" "" "")

absent ∷ HM.HashMap Text Row → [Text] → Expectation
absent rs names = [ n | n ← names, HM.member n rs ] `shouldBe` []

-- | The Lua the scene runs once: the real HUD and unit-info module over a
--   selection the spec controls, plus the three recorders and the helpers.
prelude ∷ Text
prelude = T.unlines
    [ "local hud = require('scripts.hud')"
    , "hud.init(1, 2, 1920, 1080)"
    , "hud.createUI()"
    , "_G.__u = require('scripts.unit_info_v2')"
    , "_G.__label = require('scripts.ui.label')"
    , "_G.__sel = {}"
    , "unit.getSelected = function() return __sel end"
    , "_G.__tt, _G.__col, _G.__rand = {}, {}, 0"
    , "local setTR = UI.setTooltipRich"
    , "UI.setTooltipRich = function(h, c)"
    , "  local i = UI.getElementInfo(h)"
    , "  if i then __tt[i.name] = c end"
    , "  return setTR(h, c)"
    , "end"
    , "local setC = UI.setColor"
    , "UI.setColor = function(h, r, g, b, a)"
    , "  local i = UI.getElementInfo(h)"
    , "  if i then __col[i.name] = { r, g, b, a } end"
    , "  return setC(h, r, g, b, a)"
    , "end"
    , "local newT = UI.newText"
    , "UI.newText = function(name, text, font, size, r, g, b, a, page)"
    , "  __col[name] = { r, g, b, a }"
    , "  __tt[(name:gsub('_text$', '')) .. '_hit'] = nil   -- a fresh label has no tooltip yet"
    , "  return newT(name, text, font, size, r, g, b, a, page)"
    , "end"
    , "local rnd = math.random"
    , "math.random = function(...) __rand = __rand + 1; return rnd(...) end"
    , "local function clean(s)"
    , "  s = tostring(s or '-'):gsub('\\194\\176', 'deg'):gsub('\\n.*', '')"
    , "  return (s:gsub('[|~]', '/'))"
    , "end"
    , "function _G.__tall()"
    , "  -- room for every row: the panel clips to the rows that fit its rect"
    , "  __u.statsContentRect.h = 8 + 48 * 40"
    , "  __u.lastContentSig = nil"
    , "  __u.update(0.016)"
    , "end"
    , "function _G.__select(uid)"
    , "  __sel = { uid }"
    , "  __u.update(0.016); __u.update(0.016)"
    , "  __tall()"
    , "end"
    , "function _G.__tick() __u.update(0.016) end"
    , "function _G.__dump()"
    , "  local rows = {}"
    , "  for _, e in ipairs(__u.statsContentElements) do"
    , "    if e.kind == 'label' then"
    , "      local nm = __label.getName(e.id)"
    , "      local n = nm:match('^unit_info_v2_stat_val_(%d+)$')"
    , "      if n then"
    , "        local icon = __tt['unit_info_v2_stat_icon_' .. n]"
    , "                  or __tt['unit_info_v2_stat_lbl_' .. n .. '_hit']"
    , "        local c = __col[nm .. '_text'] or {}"
    , "        local tip = __tt[nm .. '_hit']"
    , "        rows[#rows + 1] = table.concat({"
    , "          clean(icon and icon.text or ('?' .. n)),"
    , "          clean(__label.getText(e.id)),"
    , "          string.format('%.2f,%.2f,%.2f', c[1] or -1, c[2] or -1, c[3] or -1),"
    , "          clean(tip and tip.hint or '-'),"
    , "          clean(icon and icon.hint or '-'):sub(1, 24) }, '|')"
    , "      end"
    , "    end"
    , "  end"
    , "  return table.concat(rows, '~')"
    , "end"
    , "function _G.__ids()"
    , "  local ids = {}"
    , "  for _, e in ipairs(__u.statsContentElements) do ids[#ids + 1] = tostring(e.id) end"
    , "  return table.concat(ids, ',')"
    , "end"
    , "return true" ]

-- | A scene with the units, the real panel built and the first unit's
--   Status tab selected.
withPanel ∷ [UnitSpec] → (Scene → IO a) → IO a
withPanel specs act = withScene specs $ \sc@(_, ls) → do
    ok ← luaText ls prelude
    ok `shouldBe` "true"
    select ls 1
    act sc

select ∷ LuaBackendState → Int → IO ()
select ls uid = void $ luaText ls ("__select(" <> tshow uid <> "); return true")

tick ∷ LuaBackendState → IO ()
tick ls = void $ luaText ls "__tick(); return true"

-- | A standing, rich-profile acolyte with the unowned stats the panel
--   reads present, so rendering rolls nothing.
acolyte ∷ Int → UnitSpec
acolyte uid = (unitSpec uid "acolyte" (Just "rich"))
    { usStats = [("sleep_pressure", 50), ("max_sleep_pressure", 100)] }

setMode ∷ LuaBackendState → Int → Text → Text → IO ()
setMode ls uid system mode = do
    r ← luaText ls ("return tostring(__physiology.setMode(" <> tshow uid
                    <> ", '" <> system <> "', '" <> mode <> "'))")
    r `shouldNotBe` "nil"

-----------------------------------------------------------------------
-- The group
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "UI.PhysiologyModes" $ do
    simulatedSpec
    nominalSpec
    unsupportedSpec
    conditionsSpec
    transitionSpec
    readOnlySpec
    debugTextSpec

simulatedSpec ∷ Spec
simulatedSpec = describe "Simulated rows keep their formats and colours" $ do
    it "shows today's cur / max pairs, temperature and salt, with no value tooltip" $
        withPanel [acolyte 1] $ \(_, ls) → do
            rs ← rowsOf ls
            hy ← rowNamed rs "Hydration"
            rowText hy `shouldBe` "42.0 / 42.0"
            rowColour hy `shouldBe` neutral
            rowTip hy `shouldBe` "-"
            (rowText <$> rowNamed rs "Hunger") `shouldReturn` "700.0 / 700.0"
            (rowText <$> rowNamed rs "Calories") `shouldReturn` "1400.0 / 1400.0"
            (rowText <$> rowNamed rs "Body Temp") `shouldReturn` "37.0degC"
            (rowText <$> rowNamed rs "Salt") `shouldReturn` "100%"
            (rowText <$> rowNamed rs "Stamina") `shouldReturn` "100.0 / 100.0"

    it "recolours a draining pool by its threshold, in place, without a rebuild" $
        withPanel [acolyte 1] $ \(_, ls) → do
            before ← luaText ls "return __ids()"
            -- 20 % of the stomach: amber, and no condition changes with it
            _ ← luaText ls "unit.setStat(1, 'hunger', 140); return true"
            tick ls
            hu ← rowsOf ls >>= \rs → rowNamed rs "Hunger"
            rowText hu `shouldBe` "140.0 / 700.0"
            rowColour hu `shouldBe` "1.00,0.62,0.25"
            luaText ls "return __ids()" `shouldReturn` before

nominalSpec ∷ Spec
nominalSpec = describe "A supported Nominal system" $ do
    it "shows the word Nominal, neutral, with the exact tooltip, and stores nothing" $
        withPanel [(acolyte 1) { usOverrides = [("hydration", PhysioNominal)
                                               , ("nutrition", PhysioNominal)] }] $
            \(_, ls) → do
                -- a low-looking fixture elsewhere must not leak into the row
                rs ← rowsOf ls
                forM_ ["Hydration", "Hunger", "Calories"] $ \n → do
                    r ← rowNamed rs n
                    rowText r `shouldBe` "Nominal"
                    rowColour r `shouldBe` neutral
                    rowTip r `shouldBe` nominalHint
                -- the icon tooltip is the stat's own description, unchanged
                (rowIcon <$> rowNamed rs "Hydration")
                    `shouldReturn` "Need for water. Drops fa"
                forM_ ["hydration", "hunger", "calories"] $ \k →
                    luaText ls ("return tostring(unit.getStatBase(1, '" <> k <> "'))")
                        `shouldReturn` "nil"
                -- ... and a hundred ticks of rendering still store nothing
                replicateM_ 100 (tick ls)
                forM_ ["hydration", "hunger", "calories"] $ \k →
                    luaText ls ("return tostring(unit.getStatBase(1, '" <> k <> "'))")
                        `shouldReturn` "nil"

    it "shows thermo's and salt's healthy readings from the OWNING system" $ do
        -- thermo Nominal, salt Simulated
        withPanel [(acolyte 1) { usOverrides = [("thermo", PhysioNominal)] }] $
            \(_, ls) → do
                rs ← rowsOf ls
                t ← rowNamed rs "Body Temp"
                rowText t `shouldBe` "37.0degC"
                rowColour t `shouldBe` neutral
                rowTip t `shouldBe` nominalHint
                s ← rowNamed rs "Salt"
                rowText s `shouldBe` "100%"
                rowTip s `shouldBe` "-"
        -- salt Nominal, thermo Simulated, and a hot core that would be amber
        withPanel [(acolyte 1) { usOverrides = [("salt", PhysioNominal)]
                               , usStats = [("sleep_pressure", 50)
                                           , ("max_sleep_pressure", 100)
                                           , ("core_temp", 38.4)] }] $
            \(_, ls) → do
                rs ← rowsOf ls
                s ← rowNamed rs "Salt"
                rowText s `shouldBe` "100%"
                rowColour s `shouldBe` neutral
                rowTip s `shouldBe` nominalHint
                t ← rowNamed rs "Body Temp"
                rowText t `shouldBe` "38.4degC"
                rowColour t `shouldBe` "1.00,0.62,0.25"
                rowTip t `shouldBe` "-"

unsupportedSpec ∷ Spec
unsupportedSpec = describe "A bodyless (Unsupported) unit" $
    it "hides every physiology row and keeps the unowned stamina, blood and injury rows" $
        withPanel [ (unitSpec 1 "tiller" Nothing)
                      { usStats = [("sleep_pressure", 50), ("max_sleep_pressure", 100)]
                      , usWounds = [wound] } ] $ \(_, ls) → do
            rs ← rowsOf ls
            absent rs ["Hydration", "Hunger", "Calories", "Body Temp", "Salt"]
            (rowText <$> rowNamed rs "Stamina") `shouldReturn` "100.0 / 100.0"
            forM_ ["Blood", "Pain"] (void ∘ rowNamed rs)
            -- the injury row survives: it is the one row that is no fixed vital
            let vitals = ["Blood", "Pain", "Heart Rate", "Blood O\8322"
                         , "Consciousness", "Circulation", "Stamina", "Sleep"
                         , "Carry Load"]
            [ n | n ← HM.keys rs, n `notElem` vitals ] `shouldSatisfy` (not ∘ null)
  where
    wound = Wound { woundPart = "torso", woundKind = "slash", woundSeverity = 0.5
                  , woundAt = 0, woundBandage = 1.0, woundClot = 0.0
                  , woundHeal = 0.0, woundDressing = "", woundInfection = 0.0
                  , woundClean = False, woundInfectionType = ""
                  , woundNecrosis = 0.0 }

-- | The conditions and meters. The facade's answers are stubbed to a
--   dangerous 0.9 \/ 0.1 so that WHAT hides a row is the system's mode and
--   not a healthy value: a Nominal system's conditions and meters never
--   appear, even against stale display fixtures, while the unowned,
--   injury-driven ones (blood loss, the wound-driven organ meter) remain.
conditionsSpec ∷ Spec
conditionsSpec = describe "Conditions and failure meters follow the owning system's mode" $ do
    let stub = T.unlines
            [ "local p = require('scripts.physiology')"
            , "local realGet, realFrac = p.get, p.fraction"
            , "p.get = function(uid, key)"
            , "  if key == 'hypothermia' or key == 'salt_imbalance' or key == 'organ' then"
            , "    return 0.9, nil"
            , "  end"
            , "  return realGet(uid, key)"
            , "end"
            , "p.fraction = function(uid, key, maxKey)"
            , "  if key == 'hydration' or key == 'calories' then return 0.1 end"
            , "  return realFrac(uid, key, maxKey)"
            , "end"
            , "return true" ]
        names rs = HM.keys rs
        has rs frag = any (frag `T.isInfixOf`) (names rs)
    it "shows hydration, hunger, thermo and salt conditions only while Simulated" $ do
        withPanel [acolyte 1] $ \(_, ls) → do
            _ ← luaText ls stub
            select ls 1 >> tick ls
            rs ← rowsOf ls
            has rs "Dehydrated" `shouldBe` True
            has rs "Starving" `shouldBe` True
            has rs "Hypothermic" `shouldBe` True
            has rs "Electrolyte crisis" `shouldBe` True
            has rs "Organ failure" `shouldBe` True
        withPanel [(acolyte 1) { usOverrides =
                      [ ("hydration", PhysioNominal), ("nutrition", PhysioNominal)
                      , ("thermo", PhysioNominal), ("salt", PhysioNominal) ] }] $
            \(_, ls) → do
                _ ← luaText ls stub
                select ls 1 >> tick ls
                rs ← rowsOf ls
                has rs "Dehydrated" `shouldBe` False
                has rs "Starving" `shouldBe` False
                has rs "Hungry" `shouldBe` False
                has rs "Hypothermic" `shouldBe` False
                has rs "Electrolyte crisis" `shouldBe` False
                -- the wound-driven meter is unowned: it keeps its rule
                has rs "Organ failure" `shouldBe` True

    it "names the collapse gate by mode: dehydration only while hydration is Simulated" $ do
        let down u = u { usPose = "collapsed" }
        withPanel [down (acolyte 1)] $ \(_, ls) → do
            _ ← luaText ls stub
            select ls 1 >> tick ls
            rs ← rowsOf ls
            has rs "Collapsed (dehydrated)" `shouldBe` True
        withPanel [down (acolyte 1) { usOverrides = [("hydration", PhysioNominal)] }] $
            \(_, ls) → do
                _ ← luaText ls stub
                select ls 1 >> tick ls
                rs ← rowsOf ls
                has rs "Collapsed (dehydrated)" `shouldBe` False
                has rs "Collapsed" `shouldBe` True

    it "keeps the blood-loss condition under Nominal hydration" $
        withPanel [ (acolyte 1) { usPose = "collapsed", usBlood = Just 1.0
                                , usOverrides = [("hydration", PhysioNominal)] } ] $
            \(_, ls) → do
                _ ← luaText ls stub
                select ls 1 >> tick ls
                rs ← rowsOf ls
                has rs "Unconscious (blood loss)" `shouldBe` True

transitionSpec ∷ Spec
transitionSpec = describe "A selected unit changing mode rebuilds without reselecting" $ do
    it "swaps Simulated <-> Nominal rows and tooltips, keeping tab, selection and scroll" $
        withPanel [acolyte 1] $ \(_, ls) → do
            -- a short panel so the list scrolls, scrolled two rows down
            _ ← luaText ls
                "__u.statsContentRect.h = 8 + 48 * 5; __u.statsScrollOffset = 2; \
                \__tick(); return true"
            sb ← luaText ls "return tostring(__u.statsScrollbarId)"
            sb `shouldNotBe` "nil"
            luaText ls "return tostring(__u.statsScrollOffset)" `shouldReturn` "2"
            tab ← luaText ls "return tostring(__u.activeSubTab)"
            ids0 ← luaText ls "return __ids()"
            setMode ls 1 "hydration" "nominal"
            tick ls
            ids1 ← luaText ls "return __ids()"
            ids1 `shouldNotBe` ids0                      -- rebuilt
            luaText ls "return tostring(__u.activeUid)" `shouldReturn` "1"
            luaText ls "return tostring(__u.activeSubTab)" `shouldReturn` tab
            luaText ls "return tostring(__u.statsScrollOffset)" `shouldReturn` "2"
            luaText ls "return tostring(__u.statsScrollbarId)" `shouldReturn` sb
            -- scroll to where hydration sits and read it
            _ ← luaText ls "__u.statsScrollOffset = 0; __u.statsContentRect.h = 8 + 48 * 40; __tick(); return true"
            rs ← rowsOf ls
            hy ← rowNamed rs "Hydration"
            rowText hy `shouldBe` "Nominal"
            rowTip hy `shouldBe` nominalHint
            -- and back: Simulated again, the stored pool reseeded healthy
            setMode ls 1 "hydration" "simulated"
            tick ls
            rs' ← rowsOf ls
            hy' ← rowNamed rs' "Hydration"
            rowText hy' `shouldBe` "42.0 / 42.0"
            rowTip hy' `shouldBe` "-"
            luaText ls "return tostring(__u.activeUid)" `shouldReturn` "1"

    it "rebuilds when the definition's default or support changes under the selection" $
        withPanel [acolyte 1] $ \(env, ls) → do
            let setDefaults f = modifyIORef' (unitManagerRef env) $ \um →
                    um { umDefs = HM.map (\d → d { udPhysiology = f (udPhysiology d) })
                                         (umDefs um) }
            (rowText <$> (rowsOf ls >>= \rs → rowNamed rs "Salt"))
                `shouldReturn` "100%"
            -- a changed DEFAULT: salt becomes Nominal for this definition
            setDefaults (\up → up { upDefaults = HM.insert "salt" PhysioNominal
                                                              (upDefaults up) })
            tick ls
            salt ← rowsOf ls >>= \rs → rowNamed rs "Salt"
            rowTip salt `shouldBe` nominalHint
            -- a changed SUPPORT: the definition loses its body entirely
            setDefaults (const noUnitPhysiology)
            tick ls
            rs ← rowsOf ls
            absent rs ["Hydration", "Hunger", "Calories", "Body Temp", "Salt"]
            luaText ls "return tostring(__u.activeUid)" `shouldReturn` "1"
            -- ... and back
            setDefaults (const richPhysiology)
            tick ls
            (HM.member "Hydration" <$> rowsOf ls) `shouldReturn` True

readOnlySpec ∷ Spec
readOnlySpec = describe "Rendering is read-only" $
    it "inserts no owned value, changes no override or mode and rolls nothing" $
        withPanel [ (acolyte 1) { usOverrides = [("hydration", PhysioNominal)] }
                  , (unitSpec 2 "tiller" Nothing)
                      { usStats = [("sleep_pressure", 50), ("max_sleep_pressure", 100)] } ] $
            \(env, ls) → do
                before ← readInstance env 1
                beforeB ← readInstance env 2
                modes0 ← luaText ls "return table.concat({ __physiology.mode(1, 'hydration') or '-', __physiology.mode(2, 'salt') or '-' }, ',')"
                _ ← luaText ls "__rand = 0; return true"
                forM_ [1, 2] $ \u → select ls u >> replicateM_ 20 (tick ls)
                select ls 1 >> replicateM_ 20 (tick ls)
                after ← readInstance env 1
                afterB ← readInstance env 2
                uiPhysioOverrides after `shouldBe` uiPhysioOverrides before
                uiStats after `shouldBe` uiStats before
                uiStats afterB `shouldBe` uiStats beforeB
                uiPhysioOverrides afterB `shouldBe` uiPhysioOverrides beforeB
                luaText ls "return table.concat({ __physiology.mode(1, 'hydration') or '-', __physiology.mode(2, 'salt') or '-' }, ',')"
                    `shouldReturn` modes0
                luaText ls "return tostring(__rand)" `shouldReturn` "0"

debugTextSpec ∷ Spec
debugTextSpec = describe "The legacy debug text (unit_info_panel push path)" $ do
    let debugLua = T.unlines
            [ "package.loaded.__unit_info_v2_suppress = nil"
            , "local ip = require('scripts.hud.info_panel')"
            , "_G.__pushed = {}"
            , "ip.setUnitInfo = function(status) __pushed[#__pushed + 1] = status end"
            , "unit.getSelected = function() return { 1 } end"
            , "_G.__w = require('scripts.unit_info_panel')"
            , "__w.suppressed = false"
            , "__w.update(0.1)"
            , "return true" ]
        pushed ls = do
            out ← luaText ls
                "return ((__pushed[#__pushed] or 'NONE'):gsub('\\n', '~'))"
            pure (T.splitOn "~" out)
    it "prints Nominal for a Nominal pool and today's X / Y for a Simulated one" $ do
        withScene [(acolyte 1)] $ \(_, ls) → do
            _ ← luaText ls debugLua
            ls' ← pushed ls
            ls' `shouldSatisfy` ("Hydration: 42.00 / 42.00" `elem`)
            ls' `shouldSatisfy` ("Hunger: 700.00 / 700.00" `elem`)
            ls' `shouldSatisfy` ("Calories: 1400.00 / 1400.00" `elem`)
            ls' `shouldSatisfy` ("Stamina: 100.00 / 100.00" `elem`)
        withScene [(acolyte 1) { usOverrides = [("hydration", PhysioNominal)
                                               , ("nutrition", PhysioNominal)] }] $
            \(_, ls) → do
                _ ← luaText ls debugLua
                ls' ← pushed ls
                ls' `shouldSatisfy` ("Hydration: Nominal" `elem`)
                ls' `shouldSatisfy` ("Hunger: Nominal" `elem`)
                ls' `shouldSatisfy` ("Calories: Nominal" `elem`)
                ls' `shouldSatisfy` ("Stamina: 100.00 / 100.00" `elem`)
    it "omits the lines of an Unsupported unit and keeps the unowned stamina line" $
        withScene [ (unitSpec 1 "tiller" Nothing) ] $ \(_, ls) → do
            _ ← luaText ls debugLua
            ls' ← pushed ls
            filter (\l → any (`T.isPrefixOf` l)
                        ["Hydration", "Hunger", "Calories"]) ls' `shouldBe` []
            ls' `shouldSatisfy` ("Stamina: 100.00 / 100.00" `elem`)
