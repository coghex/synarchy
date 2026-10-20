{-# LANGUAGE Strict #-}
-- | The unit-id ownership queries of "Lua faction model" (#2518, FTS-4):
--   @unit.isPlayerOwned@, @unit.isPlayerCommandable@ and
--   @unit.hasUnrestrictedCombat@, driven through their REAL registration
--   ('Engine.Scripting.Lua.API.registerLuaAPI') against real unit
--   instances, not a stub table.
--
--   The roster mixes every case the issue names: an owned portal
--   acolyte, the same acolyte on a page that is loaded but NOT visible,
--   an uncontrolled acolyte sharing the owned one's tag (allied by
--   policy, owned and commandable by nobody), a debug spawn, a wolf, and
--   ids no unit carries. Each verb answers from the unit's live faction
--   profile in the global roster, so page visibility changes nothing.
--
--   Own engine: it WRITES the unit manager ref.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua faction model"'@.
module Test.Headless.Lua.UnitOwnership (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (writeIORef)
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, mkUnit, newBareLuaBackend)
import Unit.Faction (Faction(..), FactionRelation(..))
import Unit.Faction.Membership
    ( UnitFactionProfile, resolveLegacyFaction, resolveSpawnFaction
    , unitPolicyProfile )
import Unit.Faction.Profile (emptyFactionPolicy, relationFromTo, tagAcolyte)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

visiblePage, hiddenPage ∷ WorldPageId
visiblePage = WorldPageId "ownership_visible"
hiddenPage  = WorldPageId "ownership_hidden"

-- | uid → (def, page, faction profile). Every profile comes from the
--   real spawn ingress: a legacy tag through the D-26 mapping against
--   the acolyte definition's authored @acolyte@ default, or an OMITTED
--   tag (D-31), which is what an uncontrolled acolyte is.
roster ∷ [(Int, Text, WorldPageId, UnitInstance → UnitInstance)]
roster =
    [ (1, "acolyte", visiblePage, legacy FactionPlayer)
    , (2, "acolyte", hiddenPage,  legacy FactionPlayer)
    , (3, "acolyte", visiblePage, \u → u { uiFaction = uncontrolled })
    , (4, "acolyte", visiblePage, legacy FactionDebug)
    , (5, "wolf",    visiblePage, legacy FactionWildlife)
    ]
  where
    legacy f u = u { uiFaction = resolveLegacyFaction [tagAcolyte] f }

uncontrolled ∷ UnitFactionProfile
uncontrolled = resolveSpawnFaction [tagAcolyte] Nothing

resetWorld ∷ EngineEnv → IO ()
resetWorld env = do
    wsV ← emptyWorldState
    wsH ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(visiblePage, wsV), (hiddenPage, wsH)]
        , wmVisible = [visiblePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (UnitId (fromIntegral uid)
              , set ((mkUnit def FactionNeutral (0, 0) 0 [] []) { uiPage = page }))
            | (uid, def, page, set) ← roster ]
        }

-- | One verb's answer for each argument expression, comma-joined.
answers ∷ EngineEnv → Text → [Text] → IO Text
answers env verb args = do
    resetWorld env
    ls ← newBareLuaBackend env
    evalDebug ls $ "local out = {}; for _, f in ipairs({"
        <> mconcat [ "function() return " <> verb <> "(" <> a <> ") end, "
                   | a ← args ]
        <> "}) do local ok, v = pcall(f); "
        <> "out[#out + 1] = ok and tostring(v) or ('error:' .. tostring(v)) end; "
        <> "return table.concat(out, ',')"

q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | owned, owned-hidden, uncontrolled, debug, wolf.
unitArgs ∷ [Text]
unitArgs = ["1", "2", "3", "4", "5"]

-- | No argument, nil, an id with no unit, a negative id, a fractional
--   id, a numeric STRING ('Lua.tointeger' would coerce it), an id past
--   'Word32', and a table.
missingArgs ∷ [Text]
missingArgs = ["", "nil", "4242", "-1", "1.5", "'1'", "2^40", "{}"]

spec ∷ SpecWith EngineEnv
spec = describe "Lua faction model" $
    describe "unit-id ownership queries (#2518, FTS-4)" $ do
        it "the uncontrolled acolyte really is the owned acolyte's ally" $ \_ →
            relationFromTo emptyFactionPolicy
                (unitPolicyProfile uncontrolled)
                (unitPolicyProfile (resolveLegacyFaction [tagAcolyte] FactionPlayer))
                `shouldBe` RelAlly

        it "unit.isPlayerOwned: only the local roster, on any page" $ \env →
            answers env "unit.isPlayerOwned" unitArgs
                `shouldReturn` q "true,true,false,false,false"

        it "unit.isPlayerCommandable: the roster plus the debug spawn" $ \env →
            answers env "unit.isPlayerCommandable" unitArgs
                `shouldReturn` q "true,true,false,true,false"

        it "unit.hasUnrestrictedCombat: the debug spawn alone" $ \env →
            answers env "unit.hasUnrestrictedCombat" unitArgs
                `shouldReturn` q "false,false,false,true,false"

        forM_ [ "unit.isPlayerOwned", "unit.isPlayerCommandable"
              , "unit.hasUnrestrictedCombat" ] $ \verb →
            it (T.unpack verb <> " answers false, never raising, for \
                                 \anything that names no live unit") $ \env →
                answers env verb missingArgs
                    `shouldReturn` q "false,false,false,false,false,false,false,false"
