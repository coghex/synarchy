{-# LANGUAGE Strict #-}
-- | "Loot realization" (#2502, epic #1231 PLC-13): the deterministic
--   pass that turns a loot PROFILE and a realization context into a
--   crate's actual cargo, and the @loot.simulate@ diagnostic over it.
--
--   Four different kinds of proof live here, and they are deliberately
--   not interchangeable:
--
--   1. __Fixed vectors.__ Three contexts are pinned against a fixture
--      profile and a fixture storage container, rendered down to every
--      field a lot's physical identity is made of — definition,
--      quality, condition, weight, fill and nesting, with instance ids
--      masked. Any change to the derivation, the draw order or the
--      shuffle moves these, which is the whole point: the mapping from
--      context to cargo is a contract, not an implementation detail.
--
--   2. __Order and isolation.__ The same three contexts are evaluated
--      forwards and backwards and must agree, because a realization
--      that depended on what ran before it would be a save/load bug
--      nobody could reproduce.
--
--   3. __The capacity boundary, mutation-tested.__ Every bound this
--      slice can reach — the shell's internal weight capacity and its
--      internal bulk capacity — is driven at the tightest value that
--      REFUSES and then loosened by exactly one unit, and the verdict
--      has to flip. A capacity check that was deleted, or widened to
--      the next convenient constant, fails an example here.
--
--   4. __The three outcomes that must not collapse.__ "Came up empty",
--      "the shell declares no storage" and "an entry names an item
--      nobody registered" are three different answers (design D-23),
--      and an intrinsically oversized lot is a fourth thing again — an
--      ordinary capacity rejection.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Loot realization"'@.
module Test.Headless.Loot.Realization
    ( spec
    , luaSpec
    ) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef
    (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Random (StdGen, mkStdGen)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Core (CoreCapability, toCoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability, toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability, toWorldSimCapability)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LoggerState )
import Engine.Core.State
    ( EngineEnv, itemManagerRef, nextItemInstanceIdRef, statRNGRef
    , worldManagerRef )
import Engine.Scripting.Lua.API.LootSimulate (lootSimulateFn)
import Item.Materialize (materializeItem, pristineItem)
import Item.Ownership (OwnershipRefusal(..))
import Item.Types
    ( ItemContainer(..), ItemContentEntry(..), ItemDef(..)
    , ItemInstance(..), ItemManager(..), ItemStorage(..) )
import LootProfile.Realize
import LootProfile.Simulate
import LootProfile.Types
    ( LootProfileDef(..), LootProfileEntry(..), emptyLootProfileRegistry
    , registerLootProfile )
import Test.Headless.Harness (sharedWorld, sharedWorldPageId)
import World.Types (WorldManager(..))

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | A definition with nothing rolled and nothing held, in the shape
--   'Test.Headless.Item.Materialize' established: every fixture below
--   is this plus the one property it is about.
bareDef ∷ Text → Float → Float → ItemDef
bareDef name weight bulk = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = weight, idWeightSpec = Nothing, idBulk = bulk
    , idStorage = Nothing, idKind = "misc"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0, idSourcePath = "test-fixture"
    }

contentEntry ∷ Text → Int → ItemContentEntry
contentEntry name n = ItemContentEntry
    { iceItem = name, iceCount = n, iceFill = Nothing, iceContents = Nothing }

-- | The fixture registry. Each definition exists for one property:
--
--     * @crate@ / @stocked_crate@ — the shells. The second authors
--       default contents, so "existing children are preserved and
--       reduce the capacity a lot is admitted against" (D-22) has a
--       shell it is true of.
--     * @steel_bar@ / @rations@ — one unit of weight and bulk each, so
--       a capacity expressed in whole units means what it says.
--     * @anvil@ — heavier and bulkier than any shell here: the
--       intrinsically oversized lot.
--     * @gem@ — rolls BOTH quality and weight, so "the committed tree
--       equals the capacity-tested candidate" has rolled values to
--       compare rather than two copies of a constant.
--     * @toolbox@ — carries authored contents of its own: its
--       recursive weight is 3 while its external bulk is 1, which is
--       what separates the recursive weight charge from the
--       direct-child bulk charge (D-5).
--     * @canteen@ — a fluid container spawning full: its case weighs 1
--       and the 3 L it spawns holding weigh 3 more, so a lot's weight
--       includes its fill.
--     * @ingot@ — three times a bar's weight and the same bulk, so a
--       fixture can hold a lot that fits an EMPTY shell and not a
--       stocked one.
--     * @stocked_small_crate@ — 4 kg of capacity with 2 kg of authored
--       rations already aboard: an ingot fits it empty and does not fit
--       it as authored, which is exactly what the simulation's
--       saturation measure is about.
--     * @plain_box@ — declares no @storage:@: the refusing shell.
--     * @456@ — a perfectly ordinary storage container whose NAME is
--       all digits, which is what makes @loot.simulate@'s refusal of a
--       Lua @number@ container argument provable: were the argument
--       coerced, @456@ would resolve rather than fail.
probeItems ∷ ItemManager
probeItems = ItemManager $ HM.fromList
    [ ("crate", (bareDef "crate" 2 5)
        { idStorage = Just (ItemStorage 10 10) })
    , ("stocked_crate", (bareDef "stocked_crate" 2 5)
        { idStorage = Just (ItemStorage 10 10)
        , idDefaultContents = [contentEntry "rations" 2] })
    , ("stocked_small_crate", (bareDef "stocked_small_crate" 2 5)
        { idStorage = Just (ItemStorage 4 20)
        , idDefaultContents = [contentEntry "rations" 2] })
    , ("plain_box", bareDef "plain_box" 2 5)
    , ("456", (bareDef "456" 2 5)
        { idStorage = Just (ItemStorage 10 10) })
    , ("steel_bar", bareDef "steel_bar" 1 1)
    , ("rations", bareDef "rations" 1 1)
    , ("anvil", bareDef "anvil" 100 100)
    , ("ingot", bareDef "ingot" 3 1)
    , ("gem", (bareDef "gem" 1 1)
        { idWeightSpec = Just (1.0, 0.4)
        , idQualitySpec = Just (20, 80) })
    , ("toolbox", (bareDef "toolbox" 1 1)
        { idDefaultContents = [contentEntry "steel_bar" 2] })
    , ("canteen", (bareDef "canteen" 1 1)
        { idContainer = Just ItemContainer
            { icCapacity = 5, icHolds = "water"
            , icFillWeight = 1.0, icDefaultFill = 3 } })
    ]

profileOf ∷ Text → (Int, Int) → [(Text, Float, Int)] → LootProfileDef
profileOf pid (mn, mx) es = LootProfileDef
    { lpdId            = pid
    , lpdMultiplierMin = mn
    , lpdMultiplierMax = mx
    , lpdEntries =
        [ LootProfileEntry { lpeItem = i, lpeChance = c
                           , lpeQuantityFactor = f }
        | (i, c, f) ← es ]
    }

-- | The profile the fixed vectors are pinned against. Every field a
--   pin is supposed to cover has an entry that carries it: a factor of
--   2 so a lot is a lot rather than an instance, a rolled quality and
--   weight, an item that arrives holding its own authored contents, one
--   that arrives holding a fill, and one whose appearance is a coin
--   flip so the pins differ by context rather than only by shuffle. The
--   multiplier range is wide enough that per-entry multipliers can
--   disagree.
vectorProfile ∷ LootProfileDef
vectorProfile = profileOf "probe_vectors" (1, 3)
    [ ("steel_bar", 1.0, 2)
    , ("gem",       1.0, 1)
    , ("toolbox",   1.0, 1)
    , ("canteen",   1.0, 1)
    , ("rations",   0.6, 1)
    ]

-- | Exactly one lot of one instance, always. The capacity cases use it
--   so the only thing that can decide the verdict is the bound under
--   test.
oneBarProfile ∷ LootProfileDef
oneBarProfile = profileOf "probe_one_bar" (1, 1) [("steel_bar", 1.0, 1)]

-- | Exactly three lots of one instance each, always: the "a rejection
--   never evicts an admission" fixture.
threeBarProfile ∷ LootProfileDef
threeBarProfile = profileOf "probe_three_bars" (3, 3) [("steel_bar", 1.0, 1)]

-- | One ATOMIC lot of three: whole-lot admission.
tripleLotProfile ∷ LootProfileDef
tripleLotProfile = profileOf "probe_triple" (1, 1) [("steel_bar", 1.0, 3)]

-- | Nothing ever appears.
neverProfile ∷ LootProfileDef
neverProfile = profileOf "probe_never" (1, 1)
    [("steel_bar", 0.0, 1), ("rations", 0.0, 1)]

-- | Every lot is bigger than any shell here.
oversizedProfile ∷ LootProfileDef
oversizedProfile = profileOf "probe_oversized" (1, 1) [("anvil", 1.0, 1)]

-- | A profile whose ID is all digits, for the same reason @456@ is an
--   item: a coerced @number@ profile argument would find it.
digitIdProfile ∷ LootProfileDef
digitIdProfile = profileOf "123" (1, 1) [("steel_bar", 1.0, 1)]

-- | Names an item nobody registered.
unknownItemProfile ∷ LootProfileDef
unknownItemProfile = profileOf "probe_unknown" (1, 1)
    [("steel_bar", 1.0, 1), ("no_such_item", 1.0, 1)]

-- | One 1 kg lot and one 3 kg lot, always. Against
--   @stocked_small_crate@ — 4 kg of capacity, 2 kg of it already spent
--   on authored rations — the bar is admitted and the ingot is not,
--   while the ingot WOULD have fitted the same crate empty. That is
--   the saturation definition, and it is the only shape that tells
--   saturation apart from both "intrinsically oversized" and "measured
--   against the wrong shell".
saturatingProfile ∷ LootProfileDef
saturatingProfile = profileOf "probe_saturating" (1, 1)
    [("steel_bar", 1.0, 1), ("ingot", 1.0, 1)]

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

silentLogger ∷ IO LoggerState
silentLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

-- | An id allocator and a reader for how many ids it has handed out.
allocatorFrom ∷ Word64 → IO (IO Word64, IO Word64)
allocatorFrom start = do
    ref ← newIORef start
    pure ( atomicModifyIORef' ref (\n → (n + 1, n))
         , subtract start <$> readIORef ref )

-- | Mint a shell through the REAL materializer, so its authored default
--   contents, rolled weight and fill are the production ones.
mintShell ∷ Text → IO ItemInstance
mintShell name = do
    logger ← silentLogger
    rng    ← newIORef (mkStdGen 20502 ∷ StdGen)
    (alloc, _) ← allocatorFrom 1
    mInst  ← materializeItem probeItems logger rng alloc pristineItem name
    case mInst of
        Just inst → pure inst
        Nothing   →
            fail ("mintShell: unknown fixture item " ⧺ T.unpack name)

-- | Re-state a shell's internal capacities. The knob every capacity
--   case turns, so no case depends on a bound another case tuned.
withStorage ∷ Float → Float → ItemInstance → ItemInstance
withStorage w b shell = shell { iiStorage = Just (ItemStorage w b) }

-- | Realize against the fixture registry, answering the result and how
--   many REAL instance ids it spent. Real ids start well above the
--   shell's own, so a committed id is recognizable on sight.
realizeWith ∷ LootProfileDef → ItemInstance → RealizeContext
            → IO (RealizeResult, Word64)
realizeWith profile shell ctx = do
    logger ← silentLogger
    (alloc, spent) ← allocatorFrom 1000
    result ← realizeLootProfile probeItems logger alloc ctx profile shell
    (,) result <$> spent

ctxOf ∷ Int → Int → Int → RealizeContext
ctxOf seed inst slot = RealizeContext
    { rcWorldSeed = seed, rcInstanceId = inst, rcSlot = slot }

doneShell ∷ RealizeResult → ItemInstance
doneShell (RealizeDone shell _) = shell
doneShell (RealizeRefused r)    =
    error ("expected a completed realization, got " ⧺ show r)

doneReport ∷ RealizeResult → RealizeReport
doneReport (RealizeDone _ rep) = rep
doneReport (RealizeRefused r)  =
    error ("expected a completed realization, got " ⧺ show r)

-- | An instance rendered down to everything that is its physical
--   identity, ids MASKED: definition name, quality, condition, empty
--   weight, current fill, and — recursively — its contents in order.
renderInstance ∷ ItemInstance → Text
renderInstance i = T.concat
    [ iiDefName i
    , "(q=", tshow (iiQuality i)
    , " c=", tshow (iiCondition i)
    , " w=", tshow (iiWeight i)
    , " f=", tshow (iiCurrentFill i)
    , ")"
    , if null (iiContents i)
        then ""
        else "[" <> T.intercalate " " (map renderInstance (iiContents i))
                 <> "]"
    ]

-- | A whole shell's contents rendered, one line per direct child, in
--   order — which is what a fixed vector pins.
renderContents ∷ ItemInstance → [Text]
renderContents = map renderInstance ∘ iiContents

-- | Every instance id in a tree, parents AFTER their children — the
--   order 'materializeItem' allocates in.
idsDepthFirst ∷ ItemInstance → [Word64]
idsDepthFirst i = concatMap idsDepthFirst (iiContents i) ⧺ [iiInstanceId i]

-- | The verdicts of a report, in consideration order.
verdicts ∷ RealizeReport → [LotVerdict]
verdicts = map lrVerdict ∘ rpLots

-- | The (entry, lot) identities admitted, in consideration order.
admittedIds ∷ RealizeReport → [(Int, Int)]
admittedIds rep = [ (lrEntryIndex l, lrLotIndex l) | l ← reportAdmitted rep ]

-----------------------------------------------------------------------
-- The pure spec
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "Loot realization" $ do
    fixedVectorSpec
    orderSpec
    candidateCommitSpec
    drawContractSpec
    capacitySpec
    outcomeSpec
    simulationSpec

-- * Fixed vectors

-- | The three contexts the vectors are pinned for. Two share a seed and
--   differ only in the instance id, so the pin proves the instance axis
--   actually reaches the draws; the third changes all three components.
vectorContexts ∷ [RealizeContext]
vectorContexts = [ctxOf 42 1 0, ctxOf 42 2 0, ctxOf 99 7 3]

-- | What each of those contexts realizes into a @crate@ whose
--   capacities are generous enough that nothing is rejected — so the
--   pin is about the DRAW, not about admission.
--
--   Regenerated deliberately, never "to make the test pass": a diff
--   here means the context-to-cargo mapping moved.
pinnedVectors ∷ [[Text]]
pinnedVectors = [vectorCargo1, vectorCargo2, vectorCargo3]

vectorCargo1, vectorCargo2, vectorCargo3 ∷ [Text]
vectorCargo1 =
    [ "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "canteen(q=100.0 c=100.0 w=1.0 f=3.0)"
    , "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "gem(q=65.165764 c=100.0 w=1.0158615 f=0.0)"
    , "gem(q=61.847595 c=100.0 w=0.8673214 f=0.0)"
    , "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "gem(q=47.346092 c=100.0 w=1.0211625 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    ]

vectorCargo2 =
    [ "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "gem(q=20.0 c=100.0 w=1.1255314 f=0.0)"
    , "gem(q=66.94791 c=100.0 w=0.97253966 f=0.0)"
    , "canteen(q=100.0 c=100.0 w=1.0 f=3.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "gem(q=48.876606 c=100.0 w=0.9054163 f=0.0)"
    , "canteen(q=100.0 c=100.0 w=1.0 f=3.0)"
    ]

vectorCargo3 =
    [ "canteen(q=100.0 c=100.0 w=1.0 f=3.0)"
    , "canteen(q=100.0 c=100.0 w=1.0 f=3.0)"
    , "canteen(q=100.0 c=100.0 w=1.0 f=3.0)"
    , "rations(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "gem(q=80.0 c=100.0 w=1.0119021 f=0.0)"
    , "gem(q=39.523235 c=100.0 w=0.8111639 f=0.0)"
    , "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    , "rations(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "rations(q=100.0 c=100.0 w=1.0 f=0.0)"
    , "toolbox(q=100.0 c=100.0 w=1.0 f=0.0)[steel_bar(q=100.0 c=100.0 w=1.0 f=0.0) steel_bar(q=100.0 c=100.0 w=1.0 f=0.0)]"
    ]

-- | The lot identities admitted for each pinned context.
pinnedAdmissions ∷ [[(Int, Int)]]
pinnedAdmissions =
    [ [(3, 3), (4, 1), (3, 2), (2, 2), (2, 1), (3, 1), (2, 3), (1, 1)]
    , [(3, 3), (2, 1), (2, 2), (4, 1), (1, 1), (3, 1), (3, 2), (2, 3)
      , (4, 2)]
    , [(4, 1), (4, 3), (4, 2), (5, 2), (1, 3), (1, 1), (2, 2), (2, 1)
      , (3, 2), (5, 3), (1, 2), (5, 1), (3, 1)]
    ]

fixedVectorSpec ∷ Spec
fixedVectorSpec = describe "fixed vectors" $ do
    it "realizes each pinned context into exactly its pinned cargo" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        rendered ← forM vectorContexts $ \ctx →
            renderContents ∘ doneShell ∘ fst
                <$> realizeWith vectorProfile shell ctx
        rendered `shouldBe` pinnedVectors

    it "admits exactly the pinned (entry, lot) identities, in the \
       \pinned shuffled order" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        got ← forM vectorContexts $ \ctx →
            admittedIds ∘ doneReport ∘ fst
                <$> realizeWith vectorProfile shell ctx
        got `shouldBe` pinnedAdmissions

    it "answers the same cargo a second time — the realization reads no \
       \generator of its own" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        let ctx = ctxOf 42 1 0
        first  ← renderContents ∘ doneShell ∘ fst
                     <$> realizeWith vectorProfile shell ctx
        second ← renderContents ∘ doneShell ∘ fst
                     <$> realizeWith vectorProfile shell ctx
        second `shouldBe` first

-- * Order and isolation

orderSpec ∷ Spec
orderSpec = describe "context isolation" $ do
    it "answers the same cargo whichever order the contexts are \
       \evaluated in" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        let run ctxs = forM ctxs $ \ctx →
                (,) ctx ∘ renderContents ∘ doneShell ∘ fst
                    <$> realizeWith vectorProfile shell ctx
        forwards  ← run vectorContexts
        backwards ← run (reverse vectorContexts)
        sortOnFst backwards `shouldBe` sortOnFst forwards

    it "answers the same cargo when only ONE context is evaluated, so \
       \no context is carrying state into the next" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        alone ← forM vectorContexts $ \ctx →
            renderContents ∘ doneShell ∘ fst
                <$> realizeWith vectorProfile shell ctx
        alone `shouldBe` pinnedVectors
  where
    sortOnFst = sort ∘ map (\(ctx, r) → (show ctx, r))

-- * Candidate / commit

candidateCommitSpec ∷ Spec
candidateCommitSpec = describe "candidate and commit" $ do
    it "commits trees whose physical values equal the capacity-tested \
       \candidates, rolled fields included" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, _) ← realizeWith vectorProfile shell (ctxOf 42 1 0)
        let rep       = doneReport result
            candidate = concatMap lrCandidates (reportAdmitted rep)
            committed = iiContents (doneShell result)
        map renderInstance committed `shouldBe` map renderInstance candidate

    it "rolls a VARIABLE weight and quality per instance, so that \
       \comparison is not two copies of one constant" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, _) ← realizeWith (profileOf "probe_gems" (3, 3)
                                       [("gem", 1.0, 2)])
                                  shell (ctxOf 42 1 0)
        let gems = iiContents (doneShell result)
        length gems `shouldBe` 6
        length (nub (map iiWeight gems)) `shouldSatisfy` (> 1)
        length (nub (map iiQuality gems)) `shouldSatisfy` (> 1)

    it "spends no real instance id when every lot is rejected" $ do
        shell ← withStorage 0 1000 <$> mintShell "crate"
        (result, spent) ← realizeWith threeBarProfile shell (ctxOf 42 1 0)
        reportAdmitted (doneReport result) `shouldBe` []
        spent `shouldBe` 0

    it "spends exactly one real id per committed instance" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, spent) ← realizeWith threeBarProfile shell (ctxOf 42 1 0)
        length (iiContents (doneShell result)) `shouldBe` 3
        spent `shouldBe` 3

    it "keeps descendant-before-parent allocation inside a committed \
       \tree" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, _) ← realizeWith (profileOf "probe_boxes" (1, 1)
                                       [("toolbox", 1.0, 1)])
                                  shell (ctxOf 42 1 0)
        case iiContents (doneShell result) of
            [box] → do
                length (iiContents box) `shouldBe` 2
                idsDepthFirst box `shouldSatisfy` isStrictlyAscending
            other → expectationFailure
                        ("expected one committed toolbox, got "
                         ⧺ show (length other))
  where
    isStrictlyAscending xs = and (zipWith (<) xs (drop 1 xs))
    nub = foldl' (\acc x → if x `elem` acc then acc else acc ⧺ [x]) []

-- * The draw contract

drawContractSpec ∷ Spec
drawContractSpec = describe "the draw contract" $ do
    it "rolls a multiplier PER successful entry, not one for the \
       \profile" $ do
        -- Both entries always appear, so any difference in how many
        -- lots they propose can only come from two separate draws.
        let profile = profileOf "probe_two" (1, 6)
                          [("steel_bar", 1.0, 1), ("rations", 1.0, 1)]
            perEntry ctx =
                [ length [ () | l ← proposedLots ctx profile
                              , lotEntryIndex l ≡ ix ]
                | ix ← [1, 2] ]
            spreads = [ perEntry (ctxOf 42 i 0) | i ← [1 .. 40] ]
        spreads `shouldSatisfy` any (\xs → case xs of
            [a, b] → a ≢ b
            _      → False)

    it "indexes every draw by the entry's AUTHORED position, so \
       \reordering a file is a content change" $ do
        let asWritten = profileOf "probe_order" (1, 1)
                            [("steel_bar", 0.5, 1), ("rations", 0.5, 1)]
            swapped   = profileOf "probe_order" (1, 1)
                            [("rations", 0.5, 1), ("steel_bar", 0.5, 1)]
            itemsFor p ctx = sort (map lotItem (proposedLots ctx p))
            differing =
                [ () | i ← [1 .. 40]
                     , let ctx = ctxOf 42 i 0
                     , itemsFor asWritten ctx ≢ itemsFor swapped ctx ]
        differing `shouldSatisfy` (not ∘ null)

    it "derives a lot's contents from its own (entry, lot) identity, so \
       \the shuffle reorders lots without rerolling them" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, _) ← realizeWith threeBarProfile shell (ctxOf 42 1 0)
        let rep = doneReport result
        -- Three lots of one bar each: whatever order they were
        -- considered in, each lot's identity is its own.
        sort (map (\l → (lrEntryIndex l, lrLotIndex l)) (rpLots rep))
            `shouldBe` [(1, 1), (1, 2), (1, 3)]

    it "considers lots in an order the SHUFFLE decided, not authored \
       \order" $ do
        let profile = profileOf "probe_shuffle" (4, 4)
                          [("steel_bar", 1.0, 1), ("rations", 1.0, 1)]
            lotsFor i  = proposedLots (ctxOf 42 i 0) profile
            orderFor i = map lotItem (shuffleLots (ctxOf 42 i 0) (lotsFor i))
            authored i = map lotItem (lotsFor i)
            moved = [ () | i ← [1 .. 40], orderFor i ≢ authored i ]
        moved `shouldSatisfy` (not ∘ null)

-- * Capacity

capacitySpec ∷ Spec
capacitySpec = describe "capacity admission" $ do
    it "refuses a lot on WEIGHT alone and admits it when the weight \
       \bound is loosened by one unit" $ do
        shell ← mintShell "crate"
        -- One lot of three bars: 3 kg and 3 L. Bulk is far out of the
        -- way, so only the weight bound can decide this.
        tight ← verdictOf tripleLotProfile (withStorage 2 1000 shell)
        loose ← verdictOf tripleLotProfile (withStorage 3 1000 shell)
        tight `shouldBe` [LotRejected OverTargetWeight]
        loose `shouldBe` [LotAdmitted]

    it "refuses a lot on BULK alone and admits it when the bulk bound \
       \is loosened by one unit" $ do
        shell ← mintShell "crate"
        tight ← verdictOf tripleLotProfile (withStorage 1000 2 shell)
        loose ← verdictOf tripleLotProfile (withStorage 1000 3 shell)
        tight `shouldBe` [LotRejected OverTargetBulk]
        loose `shouldBe` [LotAdmitted]

    it "admits a lot sitting exactly ON the weight bound — it is \
       \inclusive" $ do
        shell ← mintShell "crate"
        verdictOf oneBarProfile (withStorage 1 1000 shell)
            `shouldReturn` [LotAdmitted]

    it "charges a lot's RECURSIVE weight — a toolbox weighs its bars" $ do
        shell ← mintShell "crate"
        -- The toolbox's own weight is 1; with its two authored bars it
        -- weighs 3, so a shell with room for 2 kg must still refuse it.
        tight ← verdictOf boxProfile (withStorage 2 1000 shell)
        loose ← verdictOf boxProfile (withStorage 3 1000 shell)
        tight `shouldBe` [LotRejected OverTargetWeight]
        loose `shouldBe` [LotAdmitted]

    it "charges only the DIRECT child's external bulk — a toolbox's \
       \bars cost the crate nothing" $ do
        shell ← mintShell "crate"
        -- Bulk 1 is the toolbox's own, and its two bars would add 2
        -- more if bulk were recursive.
        verdictOf boxProfile (withStorage 1000 1 shell)
            `shouldReturn` [LotAdmitted]

    it "counts the FILL a lot holds — a full canteen outweighs its \
       \empty case" $ do
        shell ← mintShell "crate"
        -- Case 1 kg plus 3 L of water at 1 kg/L. A shell with room for
        -- three would take the empty case and must refuse the full one.
        tight ← verdictOf canteenProfile (withStorage 3 1000 shell)
        loose ← verdictOf canteenProfile (withStorage 4 1000 shell)
        tight `shouldBe` [LotRejected OverTargetWeight]
        loose `shouldBe` [LotAdmitted]

    it "admits or rejects a lot WHOLE — a shell with room for two of \
       \three takes none of them" $ do
        shell ← mintShell "crate"
        tightShell ← doneShell ∘ fst
            <$> realizeWith tripleLotProfile (withStorage 2 1000 shell)
                            (ctxOf 42 1 0)
        iiContents tightShell `shouldBe` []

    it "rejects one lot without evicting the lots already admitted, and \
       \admits all three when the bound is loosened by one unit" $ do
        shell ← mintShell "crate"
        tight ← verdictOf threeBarProfile (withStorage 2 1000 shell)
        loose ← verdictOf threeBarProfile (withStorage 3 1000 shell)
        length [ () | LotAdmitted ← tight ] `shouldBe` 2
        length [ () | LotRejected _ ← tight ] `shouldBe` 1
        loose `shouldBe` [LotAdmitted, LotAdmitted, LotAdmitted]

    it "preserves the shell's authored children, in order, and admits \
       \against what they left" $ do
        shell ← mintShell "stocked_crate"
        let authored = iiContents shell
        length authored `shouldBe` 2
        -- 2 kg of rations already aboard, so a 3 kg shell has room for
        -- exactly one more bar and none for two.
        (result, _) ← realizeWith threeBarProfile
                          (withStorage 3 1000 shell) (ctxOf 42 1 0)
        let after = doneShell result
        take 2 (iiContents after) `shouldBe` authored
        length (iiContents after) `shouldBe` 3
        length [ () | LotRejected _ ← verdicts (doneReport result) ]
            `shouldBe` 2

    it "leaves an authored shell EXACTLY as it was when nothing fits" $ do
        shell ← mintShell "stocked_crate"
        (result, spent) ← realizeWith threeBarProfile
                              (withStorage 2 1000 shell) (ctxOf 42 1 0)
        iiContents (doneShell result) `shouldBe` iiContents shell
        spent `shouldBe` 0

    it "leaves the shell's own identity and physical values untouched" $ do
        shell ← withStorage 1000 1000 <$> mintShell "stocked_crate"
        (result, _) ← realizeWith vectorProfile shell (ctxOf 42 1 0)
        let after = doneShell result
        ( iiDefName after, iiInstanceId after, iiWeight after
          , iiQuality after, iiCondition after, iiCurrentFill after
          , iiBulk after, iiStorage after )
            `shouldBe`
            ( iiDefName shell, iiInstanceId shell, iiWeight shell
            , iiQuality shell, iiCondition shell, iiCurrentFill shell
            , iiBulk shell, iiStorage shell )
  where
    boxProfile     = profileOf "probe_box" (1, 1) [("toolbox", 1.0, 1)]
    canteenProfile = profileOf "probe_canteen" (1, 1) [("canteen", 1.0, 1)]
    verdictOf profile shell =
        verdicts ∘ doneReport ∘ fst
            <$> realizeWith profile shell (ctxOf 42 1 0)

-- * The outcomes that must stay distinct

outcomeSpec ∷ Spec
outcomeSpec = describe "distinct outcomes" $ do
    it "succeeds with empty contents when no entry appears" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, spent) ← realizeWith neverProfile shell (ctxOf 42 1 0)
        rpLots (doneReport result) `shouldBe` []
        iiContents (doneShell result) `shouldBe` []
        spent `shouldBe` 0

    it "REFUSES a shell that declares no storage, changing nothing" $ do
        shell ← mintShell "plain_box"
        (result, spent) ← realizeWith oneBarProfile shell (ctxOf 42 1 0)
        result `shouldBe` RealizeRefused ShellNotStorage
        spent `shouldBe` 0

    it "REFUSES a profile naming an unregistered item, changing \
       \nothing" $ do
        shell ← withStorage 1000 1000 <$> mintShell "crate"
        (result, spent) ← realizeWith unknownItemProfile shell (ctxOf 42 1 0)
        result `shouldBe` RealizeRefused (UnknownEntryItem "no_such_item")
        spent `shouldBe` 0

    it "gives the two refusals different stable spellings" $
        map realizeRefusalId [ShellNotStorage, UnknownEntryItem "x"]
            `shouldBe` ["shell_not_storage", "unknown_entry_item"]

    it "treats an intrinsically oversized lot as an ordinary capacity \
       \rejection, NOT a refusal" $ do
        shell ← mintShell "crate"
        (result, _) ← realizeWith oversizedProfile shell (ctxOf 42 1 0)
        verdicts (doneReport result) `shouldBe` [LotRejected OverTargetBulk]
        iiContents (doneShell result) `shouldBe` []

    it "keeps 'came up empty' and 'refused' apart as VALUES, not just \
       \as contents" $ do
        empty  ← withStorage 1000 1000 <$> mintShell "crate"
        boxed  ← mintShell "plain_box"
        (a, _) ← realizeWith neverProfile empty (ctxOf 42 1 0)
        (b, _) ← realizeWith neverProfile boxed (ctxOf 42 1 0)
        (a ≡ b) `shouldBe` False

-- * The simulation

simulationSpec ∷ Spec
simulationSpec = describe "the distribution simulation" $ do
    it "answers nil for a non-positive sample count" $ do
        logger ← silentLogger
        simulateLootProfile probeItems logger 42 vectorProfile "crate" 0
            `shouldReturn` Nothing

    it "answers nil for an unknown item definition" $ do
        logger ← silentLogger
        simulateLootProfile probeItems logger 42 vectorProfile "nope" 8
            `shouldReturn` Nothing

    it "answers nil for an item definition that declares no storage" $ do
        logger ← silentLogger
        simulateLootProfile probeItems logger 42 vectorProfile "plain_box" 8
            `shouldReturn` Nothing

    it "answers the same report twice for the same seed and arguments" $ do
        logger ← silentLogger
        a ← simulateLootProfile probeItems logger 42 vectorProfile "crate" 32
        b ← simulateLootProfile probeItems logger 42 vectorProfile "crate" 32
        b `shouldBe` a

    it "answers a DIFFERENT report for a different world seed" $ do
        logger ← silentLogger
        a ← simulateLootProfile probeItems logger 42 vectorProfile "crate" 32
        b ← simulateLootProfile probeItems logger 77 vectorProfile "crate" 32
        (b ≡ a) `shouldBe` False

    it "reports every sample naturally empty, and no saturation, for a \
       \profile nothing appears in" $ do
        logger ← silentLogger
        Just s ← simulateLootProfile probeItems logger 42 neverProfile
                                     "crate" 16
        (ssSamples s, ssNaturallyEmpty s, ssSaturated s, ssRefused s)
            `shouldBe` (16, 16, 0, 0)
        ssRejectedByItem s `shouldBe` []
        ssWeightBins s `shouldBe` (16 : replicate 9 0)

    it "reports NO saturation when every lot is intrinsically \
       \oversized — a lot the empty crate could not take either" $ do
        logger ← silentLogger
        Just s ← simulateLootProfile probeItems logger 42 oversizedProfile
                                     "crate" 16
        ssSaturated s `shouldBe` 0
        ssNaturallyEmpty s `shouldBe` 16
        ssRejectedByItem s `shouldBe` [("anvil", 16)]

    it "reports saturation when a rejected lot WOULD have fitted the \
       \otherwise empty crate" $ do
        logger ← silentLogger
        Just s ← simulateLootProfile probeItems logger 42 saturatingProfile
                                     "stocked_small_crate" 16
        ssSaturated s `shouldBe` 16
        ssNaturallyEmpty s `shouldBe` 0
        ssRejectedByItem s `shouldBe` [("ingot", 16)]

    it "measures that eligibility against the shell with its AUTHORED \
       \contents removed, not against the shell as stocked" $ do
        logger ← silentLogger
        -- The ingot fits the empty 4 kg crate and does not fit it
        -- alongside its 2 kg of authored rations, so a measure taken
        -- against the stocked shell would report no saturation at all.
        Just s ← simulateLootProfile probeItems logger 42 saturatingProfile
                                     "stocked_small_crate" 8
        ssSaturated s `shouldBe` 8

    it "counts every sample into both histograms" $ do
        logger ← silentLogger
        Just s ← simulateLootProfile probeItems logger 42 vectorProfile
                                     "crate" 24
        sum (ssWeightBins s) `shouldBe` 24
        sum (ssBulkBins s) `shouldBe` 24
        length (ssWeightBins s) `shouldBe` histogramBins
        length (ssBulkBins s) `shouldBe` histogramBins

    it "bins occupancy on left-closed tenths with a CLOSED top bin" $
        -- Capacity 10, so the used values below are 0%, 0.01%, 9.99%,
        -- 10%, 19.99%, 90%, 99.99% and 100%.
        map (occupancyBin 10) [0, 0.001, 0.999, 1, 1.999, 9, 9.999, 10]
            `shouldBe` [0, 0, 0, 1, 1, 9, 9, 9]

    it "puts an over-capacity occupancy in the top bin rather than off \
       \the end" $
        occupancyBin 10 25 `shouldBe` 9

-----------------------------------------------------------------------
-- The Lua surface
-----------------------------------------------------------------------

-- | @loot.simulate@ through the registered production function, against
--   the live engine's own refs.
--
--   The seed the verb reads is the ACTIVE world page's, so this borrows
--   three of the engine's refs — the item registry, the loot-profile
--   registry and the world manager's visible-page list — seeds them,
--   and restores every one of them afterwards. That is the technique
--   'Test.Headless.Loot.Profiles' established for the two registry
--   refs; the visible list is the third because nothing else can make
--   @world.getSeed()@'s answer predictable in a suite sharing one
--   engine with every other world spec.
luaSpec ∷ SpecWith EngineEnv
luaSpec = describe "Loot realization (loot.simulate)" $ do

    it "answers nil while the caller's arguments are not a profile, a \
       \storage container and a positive count" $ \env →
        withSimulateFixture env $ \core regs regsView wsc → do
            let ask chunk = runSimulateLua core regs regsView wsc chunk
            ask "return tostring(lootSimulate())"
                `shouldReturn` Just "nil"
            ask "return tostring(lootSimulate('no_such_profile', \
                \'crate', 8))"
                `shouldReturn` Just "nil"
            ask "return tostring(lootSimulate('probe_vectors', \
                \'no_such_item', 8))"
                `shouldReturn` Just "nil"
            ask "return tostring(lootSimulate('probe_vectors', \
                \'plain_box', 8))"
                `shouldReturn` Just "nil"
            ask "return tostring(lootSimulate('probe_vectors', 'crate', 0))"
                `shouldReturn` Just "nil"

    -- Lua's three conversions all coerce across the number/string
    -- line, so all three arguments are type-checked before they are
    -- read. The two name cases need fixtures whose ids are all digits:
    -- against any other registry a coerced number would fail to
    -- resolve anyway, and the example would pass while proving
    -- nothing.
    it "refuses a numeric STRING sample count rather than coercing it" $
      \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                "return tostring(lootSimulate('probe_vectors', 'crate', '8'))"
                `shouldReturn` Just "nil"

    it "refuses a NUMBER profile id even when the digits name a \
       \registered profile" $ \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                "return tostring(lootSimulate(123, '456', 8))"
                `shouldReturn` Just "nil"

    it "refuses a NUMBER container name even when the digits name a \
       \registered storage item" $ \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                "return tostring(lootSimulate('123', 456, 8))"
                `shouldReturn` Just "nil"

    -- The control that keeps both refusals above honest: spelled as
    -- STRINGS, those very same digits resolve and simulate.
    it "and simulates those same digit ids when they arrive as \
       \strings" $ \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                "local r = lootSimulate('123', '456', 8)\n\
                \if r == nil then return 'nil' end\n\
                \return r.samples .. '/' .. #r.weight_histogram"
                `shouldReturn` Just "8/10"

    it "pins the report for a profile nothing ever appears in" $ \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                    (summaryChunk "crate" "probe_never")
                `shouldReturn` Just
                    "16|1.0000|0.0000|0\
                    \|16,0,0,0,0,0,0,0,0,0|16,0,0,0,0,0,0,0,0,0|"

    it "pins the report for a profile whose every lot is intrinsically \
       \oversized" $ \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                (summaryChunk "crate" "probe_oversized")
                `shouldReturn` Just
                    "16|1.0000|0.0000|0\
                    \|16,0,0,0,0,0,0,0,0,0|16,0,0,0,0,0,0,0,0,0\
                    \|anvil=16"

    it "pins the report for a genuinely saturating profile" $ \env →
        withSimulateFixture env $ \core regs regsView wsc →
            runSimulateLua core regs regsView wsc
                (summaryChunk "stocked_small_crate" "probe_saturating")
                `shouldReturn` Just
                    "16|0.0000|1.0000|0\
                    \|0,0,0,0,0,0,0,16,0,0|0,16,0,0,0,0,0,0,0,0\
                    \|ingot=16"

    it "advances neither the engine's instance-id counter nor its \
       \shared stat RNG" $ \env →
        withSimulateFixture env $ \core regs regsView wsc → do
            beforeId  ← readIORef (nextItemInstanceIdRef env)
            beforeRng ← show <$> readIORef (statRNGRef env)
            _ ← runSimulateLua core regs regsView wsc
                    (summaryChunk "crate" "probe_vectors")
            afterId  ← readIORef (nextItemInstanceIdRef env)
            afterRng ← show <$> readIORef (statRNGRef env)
            afterId `shouldBe` beforeId
            afterRng `shouldBe` beforeRng

    it "answers nil while no world page is visible — the same nil \
       \world.getSeed() answers" $ \env →
        withSimulateFixture env $ \core regs regsView wsc → do
            saved ← readIORef (worldManagerRef env)
            let restore = writeIORef (worldManagerRef env) saved
            flip finally restore $ do
                writeIORef (worldManagerRef env)
                    (saved { wmWorlds = [], wmVisible = [] })
                got ← runSimulateLua core regs regsView wsc
                          (summaryChunk "crate" "probe_vectors")
                got `shouldBe` Just "nil"
  where
    -- One flat string per report, so a pin is one readable line and a
    -- drift in any measure moves it.
    summaryChunk container pid = T.concat
        [ "local r = lootSimulate('", pid, "', '", container, "', 16)\n"
        , "if r == nil then return 'nil' end\n"
        , "local rej = {}\n"
        , "for k, v in pairs(r.rejected_by_item) do\n"
        , "  rej[#rej + 1] = k .. '=' .. v\n"
        , "end\n"
        , "table.sort(rej)\n"
        , "return table.concat({ r.samples,\n"
        , "  string.format('%.4f', r.naturally_empty),\n"
        , "  string.format('%.4f', r.saturated),\n"
        , "  r.refused,\n"
        , "  table.concat(r.weight_histogram, ','),\n"
        , "  table.concat(r.bulk_histogram, ','),\n"
        , "  table.concat(rej, ',') }, '|')"
        ]

-- | Seed the engine's item registry, loot-profile registry and visible
--   page with this module's fixtures, run the action against the real
--   capability projections, and restore all three.
withSimulateFixture
    ∷ EngineEnv
    → (CoreCapability → ContentRegistriesCapability
        → ContentRegistriesViewCapability → WorldSimCapability → IO α)
    → IO α
withSimulateFixture env action = do
    -- The shared canonical world: memoized by (seed, size, plateCount),
    -- so this costs nothing the worldgen specs have not already paid.
    _ ← sharedWorld env 42 64 3
    let regs     = toContentRegistriesCapability env
        regsView = toContentRegistriesViewCapability env
        core     = toCoreCapability env
        wsc      = toWorldSimCapability env
    savedItems    ← readIORef (itemManagerRef env)
    savedProfiles ← readIORef (crLootProfileRegistryRef regs)
    savedWorlds   ← readIORef (worldManagerRef env)
    writeIORef (itemManagerRef env) probeItems
    writeIORef (crLootProfileRegistryRef regs) fixtureRegistry
    writeIORef (worldManagerRef env)
        (savedWorlds { wmVisible = [sharedWorldPageId 42 64 3] })
    action core regs regsView wsc `finally` do
        writeIORef (itemManagerRef env) savedItems
        writeIORef (crLootProfileRegistryRef regs) savedProfiles
        writeIORef (worldManagerRef env) savedWorlds
  where
    fixtureRegistry = foldl' (flip registerLootProfile)
                             emptyLootProfileRegistry
                             [ vectorProfile, neverProfile, oversizedProfile
                             , saturatingProfile, digitIdProfile ]

-- | Install the production 'lootSimulateFn' as a global and evaluate
--   one chunk against it.
runSimulateLua
    ∷ CoreCapability → ContentRegistriesCapability
    → ContentRegistriesViewCapability → WorldSimCapability
    → Text → IO (Maybe Text)
runSimulateLua core regs regsView wsc chunk = Lua.run $ do
    Lua.openlibs
    Lua.pushHaskellFunction (lootSimulateFn core regs regsView wsc)
    Lua.setglobal (Lua.Name "lootSimulate")
    st  ← Lua.dostring (TE.encodeUtf8 chunk)
    val ← fmap TE.decodeUtf8Lenient <$> Lua.tostring (-1)
    pure $ if st ≡ Lua.OK
             then val
             else Just ("lua error: " <> fromMaybe "?" val)
