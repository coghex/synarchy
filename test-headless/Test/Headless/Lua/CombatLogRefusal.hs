-- | @scripts\/combat_log.lua@ renders a REFUSED strike as its own thing
--   (#2328).
--
--   'Combat.Resolution' now publishes a @kind = "refused"@ event when a
--   queued strike's preconditions no longer hold at commit. Before this
--   gate the panel's @formatEvent@ handled only @miss@, @hit@ and
--   @death@ specially and dumped everything else through one generic
--   @"[ts] atk kind tgt"@ line, so all three refusal reasons would have
--   read identically and said nothing about why nothing happened.
--
--   Requirement 4 is that a refusal is DISTINGUISHABLE from a miss, so
--   the assertions here are about separation, not about wording: each
--   reason renders differently from the other two, none of them equals
--   what the same pair renders as a miss or a dodge, and none falls
--   through to the unknown-kind dump. An unrecognised reason — a newer
--   engine talking to an older panel — must still say the strike did
--   not happen rather than degrading into a miss.
--
--   Technique: the shared headless engine plus one bare Lua VM, the
--   same fixture shape "Test.Headless.Lua.GroupedLogRetention" uses to
--   drive this module. Only @engine.gameTime@, @unit.getInfo@ and
--   @combat.drainEvents@ are stubbed; @formatEvent@ itself is the real
--   one.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua.CombatLogRefusal"'@.
module Test.Headless.Lua.CombatLogRefusal (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef)
import Data.List (nub)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Combat.Resolution.Admission (AttackRefusal(..), refusalReason)
import Test.Headless.Harness (withHeadlessEngine)

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

withFixture ∷ (LuaBackendState → IO ()) → IO ()
withFixture action = withHeadlessEngine $ \env → do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    booted ← eval ls bootLua
    booted `shouldBe` "ok"
    action ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

eval ∷ LuaBackendState → Text → IO Text
eval ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    when (isLuaError r) $ expectationFailure ("Lua error: " ⧺ T.unpack r)
    pure $ if T.length r ≥ 2 ∧ T.head r ≡ '"' ∧ T.last r ≡ '"'
        then T.dropEnd 1 (T.drop 1 r)
        else r

bootLua ∷ Text
bootLua = luaLines
    [ "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end;"
    , "engine.gameTime = function() return 0 end;"
    , "unit.getInfo = function(uid) return { defName = 'probe_unit',"
    , "  displayName = 'U' .. tostring(uid) } end;"
    , "combat.drainEvents = function() return {} end;"
    , "M = require('scripts.combat_log');"
      -- One attacker/target pair for every line, so the only thing that
      -- can differ between two renderings is the event itself.
    , "function LINE(kind, payload)"
    , "  local text = M.formatEvent({ kind = kind, attacker = 7, target = 9,"
    , "                               ts = 0, payload = payload or {} });"
    , "  return text end;"
    , "function REFUSED(reason) return LINE('refused', { reason = reason }) end;"
    , "return 'ok'"
    ]

-- | The generic dump 'formatEvent' falls through to for a kind it does
--   not know: @"[ts] attacker kind target"@. Any refusal rendering that
--   equals this shape never reached a branch of its own.
unknownKindLine ∷ Text
unknownKindLine = "[00:00:00] U7 refused U9"

spec ∷ Spec
spec = around withFixture $
  describe "Lua.CombatLogRefusal" $ do

    it "renders each refusal reason as its own line" $ \ls → do
        lines' ← mapM (\r → eval ls ("return REFUSED('" <> refusalReason r <> "')"))
            [ RefusedDifferentPage, RefusedOutOfReach, RefusedInsufficientStance ]
        -- Distinct from each other…
        length (nub lines') `shouldBe` 3
        -- …and none of them is the unknown-kind dump.
        lines' `shouldNotContain` [unknownKindLine]
        mapM_ (\l → l `shouldSatisfy` (not ∘ T.null)) lines'

    it "never renders a refusal through the miss branch" $ \ls → do
        missLine  ← eval ls "return LINE('miss')"
        dodgeLine ← eval ls "return LINE('miss', { dodge = '1' })"
        lungeLine ← eval ls "return LINE('miss', { lunge = '1' })"
        refusals  ← mapM (\r → eval ls ("return REFUSED('" <> refusalReason r <> "')"))
            [ RefusedDifferentPage, RefusedOutOfReach, RefusedInsufficientStance ]
        -- A refusal that read as a miss would credit the defender with
        -- an evasion it never made, and would hide the reason entirely.
        mapM_ (\l → refusals `shouldNotContain` [l])
            [ missLine, dodgeLine, lungeLine ]

    it "still reports an unrecognised reason as a strike that did not happen" $
      \ls → do
        unknown  ← eval ls "return REFUSED('some_future_reason')"
        missing  ← eval ls "return LINE('refused')"
        missLine ← eval ls "return LINE('miss')"
        -- Not the unknown-KIND dump: the refusal branch was entered.
        unknown `shouldNotBe` unknownKindLine
        missing `shouldNotBe` unknownKindLine
        -- …and still not a miss.
        unknown `shouldNotBe` missLine
        missing `shouldNotBe` missLine
        -- A reason the panel does not know renders the same as none at
        -- all: one honest fallback, not a second vocabulary.
        unknown `shouldBe` missing
