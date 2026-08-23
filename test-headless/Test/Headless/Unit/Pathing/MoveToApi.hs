-- | The LIVE Lua surface of @unit.moveTo@'s hazard-policy argument
--   (#1217), driven through the REAL registered production API
--   (#1605).
--
--   Distinct from 'Test.Headless.Unit.Pathing.Hazard', which is
--   engine-free and exercises 'parseMoveHazardPolicy' and the cost/tick
--   layers directly. What is gated HERE is the byte boundary that pure
--   suite structurally cannot see: @unit.moveTo@ reads its fifth
--   argument with @Lua.tostring@, which yields a raw 'ByteString',
--   and a Lua string is an arbitrary byte array. Decoding it strictly
--   throws 'UnicodeException' out of the middle of the Lua call
--   instead of taking the refusal path the surrounding code was
--   written to emit — the regression commit @893cbd8f@ introduced and
--   `tools/lua_strict_decode_audit.py` now catches statically.
--
--   The malformed-bytes example is the regression proper; every other
--   example is a control proving the fix changed nothing else about
--   the argument's documented behaviour.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unit.moveTo hazard argument"'@
module Test.Headless.Unit.Pathing.MoveToApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Exception (bracket)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Log
    ( LogConfig(..), LogBackend(..), LogEntry(..), LogLevel(..)
    , LoggerState, defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv, loggerRef)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.TransferApi (evalDebug, newBareLuaBackend)
import Unit.Command.Types (UnitCommand(..))
import Unit.Pathing.Hazard (MoveHazardPolicy(..), defaultMoveHazardPolicy)
import Unit.Types (UnitId(..))

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | The unit id every call below names. @unit.moveTo@ does not resolve
--   it — it constructs the 'UnitId' and enqueues — so no unit fixture
--   is needed to observe what the argument boundary does.
movedUnit ∷ Integer
movedUnit = 4242

-- | The warning prefix @unit.moveTo@ emits when it refuses a token.
--   Assertions key on it rather than on "the only warning captured":
--   'withHeadlessEngine' runs a real world worker concurrently, which
--   is free to log warnings of its own at any moment, and a bare count
--   would make this spec fail for something it is not testing.
warnPrefix ∷ Text
warnPrefix = "unit.moveTo:"

-- | What one @unit.moveTo@ call did: the Lua-visible return value, the
--   warnings the call itself emitted, and the move commands it queued.
data MoveOutcome = MoveOutcome
    { moReturned ∷ Text
    , moWarnings ∷ [Text]
    , moQueued   ∷ [(UnitId, Float, Float, Float, MoveHazardPolicy)]
    } deriving (Show, Eq)

-- | Drive one @unit.moveTo@ call through the real registered API and
--   observe all three of its effects.
--
--   The engine's logger is swapped for a capturing one across the call
--   and restored after, since 'unitMoveToFn' reads @loggerRef@ at the
--   moment it warns. The unit queue is drained BEFORE the call so the
--   post-call drain sees only what this call produced; nothing consumes
--   that queue headlessly (the harness starts the world worker only),
--   so draining it is inert.
runMoveTo ∷ EngineEnv → LuaBackendState → Text → IO MoveOutcome
runMoveTo env ls luaArgs = do
    let queue = ucUnitQueue (toUnitCombatCapability env)
    _ ← Q.flushQueue queue
    entriesRef ← newIORef []
    captured ← capturingLogger entriesRef
    returned ← bracket (swapLogger env captured) (swapLogger env) $ \_ →
        evalDebug ls ("return unit.moveTo(" <> luaArgs <> ")")
    warns ← moveToWarnings entriesRef
    queued ← Q.flushQueue queue
    pure MoveOutcome
        { moReturned = returned
        , moWarnings = warns
        , moQueued   = [ (uid, tx, ty, sp, hz)
                       | UnitMoveTo uid tx ty sp hz ← queued ]
        }

capturingLogger ∷ IORef [LogEntry] → IO LoggerState
capturingLogger entriesRef = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }

-- | Install @logger@ and return the one it replaced, so 'bracket' can
--   put the original back.
swapLogger ∷ EngineEnv → LoggerState → IO LoggerState
swapLogger env logger = do
    previous ← readIORef (loggerRef env)
    writeIORef (loggerRef env) logger
    pure previous

moveToWarnings ∷ IORef [LogEntry] → IO [Text]
moveToWarnings entriesRef =
    filter (warnPrefix `T.isPrefixOf`)
        ∘ reverse ∘ map leMessage ∘ filter ((≡ LevelWarn) ∘ leLevel)
        <$> readIORef entriesRef

-- | The queue entry an accepted call must produce.
accepted ∷ MoveHazardPolicy → [(UnitId, Float, Float, Float, MoveHazardPolicy)]
accepted policy = [(UnitId (fromIntegral movedUnit), 5.0, 7.0, 2.0, policy)]

-- | A Lua argument list naming 'movedUnit' at (5, 7) with speed 2, plus
--   whatever fifth argument the example is about.
withHazard ∷ Text → Text
withHazard hazard = T.pack (show movedUnit) <> ", 5, 7, 2.0, " <> hazard

-----------------------------------------------------------------------
-- The spec
-----------------------------------------------------------------------

spec ∷ SpecWith EngineEnv
spec = describe "unit.moveTo hazard argument" $ do

    describe "malformed UTF-8 (the #1605 regression)" $ do

        -- `\255\254` is not a valid UTF-8 sequence in any position:
        -- 0xFF never appears in well-formed UTF-8. Written as Lua
        -- decimal escapes, so the console source this test sends is
        -- itself plain ASCII and the malformed bytes are produced by
        -- Lua's own lexer at run time — exactly how a script or a
        -- console user would deliver them.
        it "is refused with a warning, not an exception" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"\\255\\254\"")
            moReturned outcome `shouldBe` "false"
            length (moWarnings outcome) `shouldBe` 1
            moQueued outcome `shouldBe` []

        it "names the expected policies in that warning" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"\\255\\254\"")
            case moWarnings outcome of
                [w] → do
                    w `shouldSatisfy` T.isInfixOf "'allow_falls' or 'avoid_falls'"
                    w `shouldSatisfy` T.isInfixOf "move refused"
                ws  → expectationFailure ("expected one warning, got " ⧺ show ws)

        -- The two decode sites are one contract: :273 sits in the
        -- branch :267 reaches only after deciding the token is
        -- unrecognized, so a fix to :267 alone routes these very bytes
        -- into a still-strict decode. Formatting the warning at all is
        -- what proves the second site is lenient too.
        it "still formats a warning containing the offending token" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"bad\\255suffix\"")
            moReturned outcome `shouldBe` "false"
            case moWarnings outcome of
                [w] → do
                    w `shouldSatisfy` T.isInfixOf "bad"
                    w `shouldSatisfy` T.isInfixOf "suffix"
                ws  → expectationFailure ("expected one warning, got " ⧺ show ws)

    describe "controls — behaviour the fix must not have changed" $ do

        it "defaults when the argument is omitted" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (T.pack (show movedUnit) <> ", 5, 7, 2.0")
            moReturned outcome `shouldBe` "true"
            moWarnings outcome `shouldBe` []
            moQueued outcome `shouldBe` accepted defaultMoveHazardPolicy

        it "accepts allow_falls" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"allow_falls\"")
            moReturned outcome `shouldBe` "true"
            moWarnings outcome `shouldBe` []
            moQueued outcome `shouldBe` accepted FallPermitted

        it "accepts avoid_falls" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"avoid_falls\"")
            moReturned outcome `shouldBe` "true"
            moWarnings outcome `shouldBe` []
            moQueued outcome `shouldBe` accepted FallProhibited

        -- Unit.Pathing.Hazard:79 parses `T.toLower (T.strip t)`; a
        -- decode change must not quietly cost that.
        it "stays case- and whitespace-insensitive" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"  AVOID_Falls \\t\"")
            moReturned outcome `shouldBe` "true"
            moWarnings outcome `shouldBe` []
            moQueued outcome `shouldBe` accepted FallProhibited

        it "refuses a valid-UTF-8 unknown token the same way" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"avoid_fals\"")
            moReturned outcome `shouldBe` "false"
            length (moWarnings outcome) `shouldBe` 1
            moQueued outcome `shouldBe` []

        it "refuses a non-ASCII but well-formed token the same way" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls (withHazard "\"évitez\"")
            moReturned outcome `shouldBe` "false"
            length (moWarnings outcome) `shouldBe` 1
            moQueued outcome `shouldBe` []

        -- Spawn.hs returns for a missing/non-integer unit id BEFORE the
        -- hazard argument is inspected. That precedence is existing
        -- behaviour and must survive: a malformed fifth argument does
        -- not turn the early return into a warning or an exception.
        it "keeps the missing-unit-id early return ahead of the hazard argument" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls "nil, 5, 7, 2.0, \"\\255\\254\""
            moReturned outcome `shouldBe` "false"
            moWarnings outcome `shouldBe` []
            moQueued outcome `shouldBe` []

        it "keeps that early return for a non-integer unit id too" $ \env → do
            ls ← newBareLuaBackend env
            outcome ← runMoveTo env ls "\"not-an-id\", 5, 7, 2.0, \"\\255\\254\""
            moReturned outcome `shouldBe` "false"
            moWarnings outcome `shouldBe` []
            moQueued outcome `shouldBe` []
