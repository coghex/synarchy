{-# LANGUAGE TypeApplications #-}
-- | #1331: the combat-log sentence builder lists severed subparts in the
--   order the @detail@ array first severs them.
--
--   @scripts/injury_log.lua@ accumulated severs into a set-like table and
--   rendered them with @pairs@, whose order Lua does not define. Lua's
--   string-hash seed is per-STATE, so the clause was stable but arbitrary
--   within a session and different across sessions — while the layer
--   clauses immediately below it were deliberately kept in encounter
--   order.
--
--   Two things make these cases fail against that implementation rather
--   than merely disagree with it by luck:
--
--     * The permutation case renders the SAME three subparts in all six
--       encounter orders inside ONE state. One state has one hash order,
--       so at most one permutation can match it and the other five must
--       fail — no reliance on which order a given state happens to pick.
--     * The determinism case renders one fixed payload in several FRESH
--       states and asserts the exact encounter-ordered clause in each,
--       rather than only asserting the states agree, so a consistently
--       wrong order cannot pass either.
--
--   Calling @M.clauses@ needs no engine stubs: the module's only
--   @require@ is lazy and confined to the non-combat narration path, so
--   these run on a bare Lua state with the real script loaded.
module Test.Headless.Lua.InjuryNarration (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import Data.List (permutations, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | The real module plus two constructors for the engine's per-layer
--   detail records, and a renderer that makes a clause LIST comparable as
--   one string.
prelude ∷ Text
prelude = lns
    [ "local injuryLog = require('scripts.injury_log')"
    , "function layer(sub, name, material, sev)"
    , "  return { sub = sub, layer = name, material = material, sev = sev }"
    , "end"
    , "function render(detail, mech)"
    , "  return table.concat(injuryLog.clauses(detail, mech), ' / ')"
    , "end"
    ]

-- | Run one program in a fresh Lua state of its own and return the string
--   it returns. A fresh state is what varies the string-hash seed, which
--   is the whole reason the old ordering was unstable.
evalFresh ∷ Text → IO Text
evalFresh src = bracket Lua.newstate Lua.close runProgram ⌦ expectOk
  where
    runProgram st = Lua.runWith @Lua.Exception st $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 (prelude <> "\n" <> src))
        value  ← Lua.tostring (-1)
        let text = maybe "<no message>" TE.decodeUtf8Lenient value
        pure $ case status of
            Lua.OK → Right text
            _      → Left text

expectOk ∷ Either Text Text → IO Text
expectOk (Right value) = pure value
expectOk (Left msg)    = expectationFailure (T.unpack msg) ⌦ \_ → pure ""

-- | A severing bone layer for @sub@, spelled as the Lua the fixtures
--   splice into a detail literal. Severity 1.0 is @SEVER_SEVERITY@.
sever ∷ Text → Text
sever sub = "layer('" <> sub <> "','phalanx','bone',1.0)"

fingers ∷ [Text]
fingers = ["index finger", "middle finger", "ring finger"]

-- | What requirement 1 says the clause must read for a given encounter
--   order, written out independently of how the script builds it.
expectedSeverClause ∷ [Text] → Text
expectedSeverClause subs = "slicing off " <> case map ("the " <>) subs of
    [a]       → a
    [a, b]    → a <> " and " <> b
    parts     → T.intercalate ", " (init parts) <> ", and " <> last parts

spec ∷ Spec
spec = do
    -- Requirement 1, and the review's "same set, several orders" point.
    -- All six permutations run in ONE state, so the legacy pairs order —
    -- fixed for that state — can satisfy at most one of them.
    it "lists severed subparts in encounter order, whatever that order is" $ do
        let programFor subs = lns
                [ "local detail = { " <> T.intercalate ", " (map sever subs) <> " }"
                , "return render(detail, 'slash')"
                ]
            orders = sort (permutations fingers)
        results ← mapM (evalFresh ∘ programFor) orders
        results `shouldBe` map expectedSeverClause orders

    -- Requirement 3. Fresh states, exact expected text in each — asserting
    -- only that the states AGREE would accept a consistently wrong order.
    it "renders one fixed payload identically in every fresh state" $ do
        let program = lns
                [ "local detail = { " <> T.intercalate ", " (map sever fingers) <> " }"
                , "return render(detail, 'slash')"
                ]
            expected = expectedSeverClause fingers
        results ← mapM (\_ → evalFresh program) [1 .. 8 ∷ Int]
        results `shouldBe` replicate 8 expected

    -- Requirement 2. The membership dedup the ordering replaced must not
    -- have been lost: a subpart severed through two structural layers is
    -- named once, in the position of the FIRST severing record.
    it "names a subpart severed through two structural layers once" $ do
        evalFresh (lns
            [ "local detail = {"
            , "  layer('nose','nasal bone','bone',1.0),"
            , "  layer('nose','septum','cartilage',1.0),"
            , "}"
            , "return render(detail, 'slash')"
            ]) `shouldReturn` "slicing off the nose"
        -- The cartilage record comes first here, and the bone record for
        -- the same subpart must not re-position it behind the ear.
        evalFresh (lns
            [ "local detail = {"
            , "  layer('nose','septum','cartilage',1.0),"
            , "  layer('ear','auricular cartilage','cartilage',1.0),"
            , "  layer('nose','nasal bone','bone',1.0),"
            , "}"
            , "return render(detail, 'slash')"
            ]) `shouldReturn` "slicing off the nose and the ear"

    -- Requirement 4, through a mixed payload: the severed subpart's own
    -- soft layers stay suppressed, the sever clause stays first, and the
    -- surviving layer groups keep their own encounter order. Running the
    -- same rows in two orders shows the layer clauses following the
    -- payload rather than a hash.
    it "keeps sever clauses first and layer clauses in encounter order" $ do
        let softFirst = lns
                [ "local detail = {"
                , "  layer('index finger','skin','flesh',0.9),"
                , "  layer('index finger','phalanx','bone',1.0),"
                , "  layer('ring finger','skin','flesh',0.6),"
                , "  layer('ring finger','fat','flesh',0.6),"
                , "  layer('little finger','metacarpal','bone',0.5),"
                , "}"
                , "return render(detail, 'slash')"
                ]
            boneFirst = lns
                [ "local detail = {"
                , "  layer('index finger','skin','flesh',0.9),"
                , "  layer('index finger','phalanx','bone',1.0),"
                , "  layer('little finger','metacarpal','bone',0.5),"
                , "  layer('ring finger','skin','flesh',0.6),"
                , "  layer('ring finger','fat','flesh',0.6),"
                , "}"
                , "return render(detail, 'slash')"
                ]
        -- The index finger's 0.9 skin row is absent from both: it belongs
        -- to a severed subpart and is implied by the sever.
        evalFresh softFirst `shouldReturn`
            "slicing off the index finger / lacerating the skin and fat \
            \/ shattering the metacarpal"
        evalFresh boneFirst `shouldReturn`
            "slicing off the index finger / shattering the metacarpal \
            \/ lacerating the skin and fat"
