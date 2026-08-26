-- | #1714 gate: every committed event-log mutation carries a stable,
--   consecutive sequence, and @engine.getEventLog()@ publishes it.
--
--   The playtest oracle used to infer "what was appended since I last
--   looked" from row VALUES alone, which is ambiguous the moment two
--   rows can be byte-identical — and at ring capacity that ambiguity
--   silently dropped rows from the trace the critic reads. The repair
--   is identity: the store stamps each committed mutation with the next
--   integer, so an observer compares numbers instead of guessing.
--
--   Everything here runs against the REAL store through the REAL
--   emitters, and reads back through the REAL Lua surface
--   (@engine.getEventLog()@ over the debug console), because the
--   regression this exists to catch is precisely a sequence that is
--   correct in the @TVar@ and absent, reordered or stringified by the
--   time Lua sees it.
--
--   Three fixture choices are load-bearing:
--
--   * __No case assumes the store starts at sequence 1.__ The harness
--     boot may itself emit, and the counter is deliberately
--     process-lifetime, so each case reads the store's own
--     'esNextSequence' first and asserts against that base. A case
--     pinned to literal @1@ would pass or fail on unrelated boot
--     chatter.
--   * __The load-reset case drives the PRODUCTION reset.__
--     'World.Load.Publish.resetTransientState' is what a load publish
--     actually runs; a test-local @writeTVar ... empty@ would prove
--     only that the test preserves the counter, which is not the claim.
--   * __The high-water mark comes from the same engine-side snapshot
--     as the rows.__ Sampled separately the two can straddle a commit,
--     and the whole reason the pair exists is that they disagree in
--     exactly one direction — a load publish empties the ring while the
--     counter keeps counting.
--   * __Coalescing is configured per case__, by installing a
--     purpose-built category rather than borrowing a shipped one, so a
--     later edit to @data/notification_categories.yaml@ cannot silently
--     turn a coalescing case into an appending one.
module Test.Headless.Event.PlayerEventProgress (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), Value(..), decode, withObject, (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Concurrent.STM.TVar (modifyTVar')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.PlayerEvent
  (CategoryCfg(..), EventStore(..), clearEventStoreRows, eventStoreCap)
import Engine.PlayerEvent.Emit (emitEvent, emitEventFullOnPage)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import World.Load.Publish (resetTransientState)

-----------------------------------------------------------
-- Fixture
-----------------------------------------------------------

-- | A category that LOGS and never pops up. @window@ is its
--   event-log coalescing window in game-seconds: @0@ makes every emit
--   its own row, @>0@ folds identical repeats.
logCategory ∷ Double → Text → CategoryCfg
logCategory window cid = CategoryCfg
    { ccId = cid, ccDisplayName = cid, ccDescription = ""
    , ccTextColor = (1, 1, 1, 1)
    , ccLog = True, ccPopup = False, ccPause = False
    , ccPopupCoalesceWindow = 0, ccLogCoalesceWindow = window }

-- | Category id every case emits under. Not a shipped id: a case must
--   not inherit a coalescing window from YAML.
probeCat ∷ Text
probeCat = "probe_progress"

-- | Install 'probeCat' with the given log-coalescing window, clear the
--   ring's ROWS through the production helper (the counter deliberately
--   survives — that is what 'clearEventStoreRows' is), and hand back
--   the sequence the next committed mutation will take.
--
--   Clearing matters because the harness boot can leave rows behind,
--   and a rollover case has to count rows exactly.
prepare ∷ EngineEnv → Double → IO Int
prepare env window = do
    writeIORef (notificationCfgRef env) $
        HM.singleton probeCat (logCategory window probeCat)
    atomically $ modifyTVar' (eventStoreRef env) clearEventStoreRows
    esNextSequence <$> readTVarIO (eventStoreRef env)

-----------------------------------------------------------
-- The Lua surface under test
-----------------------------------------------------------

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalJSONOn ∷ FromJSON α ⇒ LuaBackendState → Text → IO α
evalJSONOn ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when ("error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t) $
        expectationFailure ("Lua error: " ⧺ T.unpack t)
    case decode (BL.fromStrict (TE.encodeUtf8 t)) of
        Just v  → pure v
        Nothing → fail ("failed to decode Lua result: " ⧺ T.unpack t)

evalJSON ∷ FromJSON α ⇒ EngineEnv → Text → IO α
evalJSON env code = newBareLuaBackend env ≫= (`evalJSONOn` code)

-- | Just the sequences, in the order @engine.getEventLog()@ returns
--   them, plus the row count — the shape the ordering and rollover
--   cases assert on without hauling a thousand full rows through JSON.
--
--   @luaTypes@ records @math.type@ per row so "is an integer, not a
--   float and not a decimal string" is checked on the WIRE rather than
--   inferred from a successful JSON decode (Lua numbers serialize
--   indistinguishably once they reach JSON).
data SeqProbe = SeqProbe
    { spCount   ∷ Int
    , spSeqs    ∷ [Int]
    , spTypes   ∷ [Text]
    , spHighest ∷ Int
      -- ^ @engine.getEventLogProgress()@'s @highest@, from the same
      --   snapshot as the rows — the pair the oracle actually observes.
    , spHighestType ∷ Text
    } deriving Show

-- | Lua has one table type, so an EMPTY list serializes as @{}@ — a
--   JSON object, not an empty array. Every list field below goes
--   through this wrapper, which accepts both shapes; without it the
--   post-reset case, whose whole point is "the ring is empty", would
--   fail to DECODE rather than assert.
newtype LuaList α = LuaList { unLuaList ∷ [α] }

instance FromJSON α ⇒ FromJSON (LuaList α) where
    parseJSON (Object o) | null o = pure (LuaList [])
    parseJSON v                   = LuaList <$> parseJSON v

instance FromJSON SeqProbe where
    parseJSON = withObject "SeqProbe" $ \o → SeqProbe
        <$> o .: "n"
        <*> (unLuaList <$> o .: "seqs")
        <*> (unLuaList <$> o .: "types")
        <*> o .: "highest"
        <*> o .: "highestType"

seqProbeLua ∷ Text
seqProbeLua = T.intercalate " "
    [ "local p = engine.getEventLogProgress();"
    , "local log, highest = p.rows, p.highest;"
    , "local seqs, types = {}, {};"
    , "for i, r in ipairs(log) do"
    , "  seqs[i] = r.sequence;"
    , "  types[i] = math.type(r.sequence) or type(r.sequence) end;"
    , "return { n = #log, seqs = seqs, types = types,"
    , "         highest = highest,"
    , "         highestType = math.type(highest) or type(highest) }"
    ]

readSeqs ∷ EngineEnv → IO SeqProbe
readSeqs env = evalJSON env seqProbeLua

-- | The one invariant the atomicity case hammers, and nothing else:
--   how many rows the snapshot held, its newest row's sequence, and the
--   high-water mark that came with it. Deliberately tiny — the case
--   takes hundreds of reads while an emitter runs flat out, and hauling
--   a thousand full rows through JSON each time would slow it to the
--   point of never interleaving.
data PairProbe = PairProbe
    { ppCount    ∷ Int
    , ppLastSeq  ∷ Int   -- ^ 0 when the ring is empty.
    , ppHighest  ∷ Int
    } deriving (Show, Eq)

instance FromJSON PairProbe where
    parseJSON = withObject "PairProbe" $ \o → PairProbe
        <$> o .: "n" <*> o .: "last" <*> o .: "highest"

pairProbeLua ∷ Text
pairProbeLua = T.intercalate " "
    [ "local p = engine.getEventLogProgress();"
    , "local n = #p.rows;"
    , "return { n = n, highest = p.highest,"
    , "         last = (n > 0) and p.rows[n].sequence or 0 }"
    ]

-- | Every field of one row, read back through Lua exactly as a consumer
--   would. Optional fields use '.:?' so an absent @coords@\/@page@\/
--   @uid@ (a Lua @nil@, which serializes as an absent key) decodes as
--   'Nothing' rather than failing.
data LogRow = LogRow
    { lrSequence ∷ Int
    , lrCategory ∷ Text
    , lrText     ∷ Text
    , lrGameTime ∷ Double
    , lrSource   ∷ Text
    , lrUid      ∷ Maybe Word32
    , lrCount    ∷ Int
    , lrCoords   ∷ Maybe Coords
    , lrPage     ∷ Maybe Text
    } deriving Show

data Coords = Coords { cX ∷ Int, cY ∷ Int } deriving (Show, Eq)

instance FromJSON Coords where
    parseJSON = withObject "Coords" $ \o → Coords <$> o .: "x" <*> o .: "y"

instance FromJSON LogRow where
    parseJSON = withObject "LogRow" $ \o → LogRow
        <$> o .: "sequence" <*> o .: "category" <*> o .: "text"
        <*> o .: "gameTime" <*> o .: "source"   <*> o .:? "uid"
        <*> o .: "count"    <*> o .:? "coords"  <*> o .:? "page"

-- | The whole log as rows. Used only by the small cases.
readRows ∷ EngineEnv → IO [LogRow]
readRows env = unLuaList <$> evalJSON env "return engine.getEventLog()"

-- | Emit as fast as the store will take it until told to stop. Runs on
--   its own thread so the atomicity case reads a store that is actually
--   being written, which is the only condition under which a torn pair
--   is observable.
emitUntilStopped ∷ EngineEnv → IORef Bool → IO ()
emitUntilStopped env stop = go (1 ∷ Int)
  where
    go i = do
        halt ← readIORef stop
        unless halt $ do
            emitEvent env probeCat "Test" ("concurrent " <> tshow i)
            go (i + 1)

-----------------------------------------------------------
-- Assertions shared by several cases
-----------------------------------------------------------

-- | Sequences must be strictly increasing in the oldest-first order the
--   log is returned in (requirement 2). Stated as its own helper so
--   every case checks the SAME property.
shouldBeStrictlyIncreasing ∷ [Int] → Expectation
shouldBeStrictlyIncreasing xs =
    and (zipWith (<) xs (drop 1 xs)) `shouldBe` True

-----------------------------------------------------------
-- Spec
-----------------------------------------------------------

spec ∷ Spec
spec = around withHeadlessEngine $ describe "Player event progress" $ do

    it "gives byte-identical appends distinct, consecutive, ordered \
       \sequences" $ \env → do
        -- The exact shape the value-matching oracle could not tell
        -- apart: three rows equal in every player-visible field.
        base ← prepare env 0
        mapM_ (const (emitEvent env probeCat "Test" "same")) [1 .. 3 ∷ Int]
        probe ← readSeqs env
        spCount probe `shouldBe` 3
        spSeqs probe `shouldBe` [base, base + 1, base + 2]
        shouldBeStrictlyIncreasing (spSeqs probe)
        -- Requirement 2's wire form: a Lua INTEGER on every row, and on
        -- the high-water mark beside them.
        spTypes probe `shouldBe` replicate 3 "integer"
        spHighestType probe `shouldBe` "integer"
        -- With rows present the two agree; the case below is the one
        -- where they must not.
        spHighest probe `shouldBe` base + 2
        map lrText <$> readRows env ≫= (`shouldBe` replicate 3 "same")

    it "gives a coalesced replacement a fresh sequence and moves it to \
       \the tail, retiring the superseded one" $ \env → do
        base ← prepare env 5
        emitEvent env probeCat "Test" "alpha"   -- base
        emitEvent env probeCat "Test" "beta"    -- base + 1
        emitEvent env probeCat "Test" "alpha"   -- coalesces -> base + 2
        rows ← readRows env
        map lrText rows `shouldBe` ["beta", "alpha"]
        map lrSequence rows `shouldBe` [base + 1, base + 2]
        -- The count bump and timestamp refresh are unchanged (#1714 is
        -- additive), and the replacement genuinely sits at the tail.
        map lrCount rows `shouldBe` [1, 2]
        -- The superseded row's sequence is GONE, not reused: that
        -- absence is the evidence an observer reports as a gap.
        (base `elem` map lrSequence rows) `shouldBe` False
        shouldBeStrictlyIncreasing (map lrSequence rows)

    it "keeps ordered, consecutive sequence metadata across front \
       \rollover" $ \env → do
        base ← prepare env 0
        let extra = 5
            total = eventStoreCap + extra
        mapM_ (\i → emitEvent env probeCat "Test" ("row " <> tshow i))
              [1 .. total]
        probe ← readSeqs env
        -- Eviction removes rows; it consumes no sequence. So the ring
        -- holds the LAST `eventStoreCap` mutations, and the first five
        -- sequences are permanently absent.
        spCount probe `shouldBe` eventStoreCap
        spSeqs probe `shouldBe` [base + extra .. base + total - 1]
        shouldBeStrictlyIncreasing (spSeqs probe)
        -- Eviction removes rows without touching the counter, so the
        -- high-water mark still names the newest mutation.
        spHighest probe `shouldBe` base + total - 1

    it "keeps the counter across the production load-publish reset, so \
       \post-reset rows outrank a pre-reset cursor" $ \env → do
        base ← prepare env 0
        emitEvent env probeCat "Test" "before one"
        emitEvent env probeCat "Test" "before two"
        beforeSeqs ← spSeqs <$> readSeqs env
        beforeSeqs `shouldBe` [base, base + 1]

        -- The reset a load publish actually performs, not a test-local
        -- clearing of the store.
        resetTransientState env
        emptied ← readSeqs env
        spCount emptied `shouldBe` 0

        emitEvent env probeCat "Test" "after"
        afterProbe ← readSeqs env
        spSeqs afterProbe `shouldBe` [base + 2]
        -- The whole point: a cursor retained from before the load is
        -- OLDER than the post-load row, so the row is reported rather
        -- than suppressed as already-seen.
        all (> maximum beforeSeqs) (spSeqs afterProbe) `shouldBe` True
        spHighest afterProbe `shouldBe` base + 2

    it "keeps reporting the high-water mark after the reset even with an \
       \empty ring, so discarded mutations stay visible" $ \env → do
        -- The round-1 review blocker: rows alone cannot distinguish
        -- "nothing has happened" from "everything that happened was
        -- discarded". Two mutations commit, the production reset throws
        -- their rows away, and NOTHING is emitted afterwards — the only
        -- surviving evidence that they existed is the counter.
        base ← prepare env 0
        emitEvent env probeCat "Test" "doomed one"
        emitEvent env probeCat "Test" "doomed two"
        resetTransientState env
        emptied ← readSeqs env
        spCount emptied `shouldBe` 0
        spSeqs emptied `shouldBe` []
        spHighest emptied `shouldBe` base + 1
        spHighestType emptied `shouldBe` "integer"

    it "leaves every pre-existing Lua row field intact beside the \
       \sequence" $ \env → do
        base ← prepare env 0
        emitEventFullOnPage env probeCat "World.Thread.Discovery"
            "Discovered: cave" (Just (4, 5)) (Just 12) (Just "page_beta")
        rows ← readRows env
        case rows of
            [row] → do
                lrSequence row `shouldBe` base
                lrCategory row `shouldBe` probeCat
                lrText     row `shouldBe` "Discovered: cave"
                lrSource   row `shouldBe` "World.Thread.Discovery"
                lrUid      row `shouldBe` Just 12
                lrCount    row `shouldBe` 1
                lrCoords   row `shouldBe` Just (Coords 4 5)
                lrPage     row `shouldBe` Just "page_beta"
                -- gameTime is whatever the headless clock reads; the
                -- contract is that the field still EXISTS and decodes
                -- as a number, which the successful decode above states.
                lrGameTime row `shouldSatisfy` (>= 0)
            other → expectationFailure
                ("expected exactly one row, got " ⧺ show (length other))

    it "takes the rows and the high-water mark from ONE snapshot, so a \
       \concurrent emitter can never make the mark name an unseen row" $
      \env → do
        -- Round-2 review blocker. Read separately, the two halves can
        -- straddle a commit: the mark names a mutation the rows do not
        -- show, so an observer reports a still-RETAINED row as lost,
        -- advances its cursor past it, and suppresses it on every later
        -- read — the row never reaches the trace.
        --
        -- The shape is production's own: a non-Lua thread emitting
        -- while the Lua thread reads. Nothing here is timing-tolerant —
        -- one atomic read makes the invariant hold on every sample, so
        -- a single violation in hundreds is a real failure, not a flake.
        _ ← prepare env 0
        ls ← newBareLuaBackend env
        stop ← newIORef False
        done ← newEmptyMVar
        _ ← forkIO $ emitUntilStopped env stop `finally` putMVar done ()
        probes ← mapM (const (evalJSONOn ls pairProbeLua))
                      [1 .. 400 ∷ Int]
        writeIORef stop True
        takeMVar done

        -- With rows present, the newest row IS the last committed
        -- mutation: every commit appends at the tail, and rows only
        -- leave from the front or all at once.
        let torn = [ p | p ← probes, ppCount p > 0
                   , ppHighest p ≢ ppLastSeq p ]
        torn `shouldBe` []
        -- ...and the emitter really did run alongside the reads, or the
        -- case above proves nothing at all.
        let marks = map ppHighest probes
        (foldr max 0 marks > foldr min maxBound marks) `shouldBe` True

    it "keeps a coords-free, unit-free emit's nil fields nil" $ \env → do
        -- The other half of requirement 7: additive means the ABSENT
        -- fields stay absent, so a consumer's `if ev.coords then`
        -- branch still behaves.
        _ ← prepare env 0
        emitEvent env probeCat "Test" "plain"
        rows ← readRows env
        map lrCoords rows `shouldBe` [Nothing]
        map lrPage   rows `shouldBe` [Nothing]
        map lrUid    rows `shouldBe` [Nothing]
