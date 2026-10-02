-- | The lane machinery of "Test.Headless.Lanes" (#2744), on synthetic
--   lanes: selection, the default lane as the home of every unassigned
--   group, the unknown-lane refusal, flag parsing, and the inventory
--   line format @tools/headless_lanes.py@ reads.
--
--   It runs only under @--lane-self-test@ — never as part of the suite,
--   whose examples must stay exactly the ones it had before lanes — and
--   @tools/headless_lanes.py@ runs it. The REAL suite's coverage, every
--   example in exactly one lane, is that tool's main check.
module Test.Headless.LaneSelection (spec) where

import UPrelude
import Data.List (isInfixOf)
import Test.Hspec
import Test.Hspec.Core.Spec (Tree(..), Item(..), SpecTree, runSpecM)
import Test.Headless.Lanes

-- | Every example a spec registers, as its group path plus description.
examples ∷ Spec → IO [[String]]
examples s = do
    (_, forest) ← runSpecM s
    pure (concatMap (walk []) forest)
  where
    walk ∷ [String] → SpecTree () → [[String]]
    walk ps (Node name ts)            = concatMap (walk (ps ⧺ [name])) ts
    walk ps (NodeWithCleanup _ _ ts)  = concatMap (walk ps) ts
    walk ps (Leaf item)               = [ps ⧺ [itemRequirement item]]

-- | Two explicit lanes and a default lane holding a group nobody
--   assigned anywhere: @fresh@ stands for a group a later change
--   registers without touching any lane definition.
synthetic ∷ Lanes
synthetic = Lanes
    { lanesInOrder =
        [ ("shared", describe "shared" $ it "a" pass >> it "b" pass)
        , ("solo",   describe "solo" $ it "c" pass)
        , ("rest",   do describe "old" $ it "d" pass
                        describe "fresh" $ it "e" pass) ]
    , lanesDefault = "rest"
    }
  where pass = pure () ∷ IO ()

select ∷ LaneRequest → IO [[String]]
select req = either (\e → expectationFailure e ≫ pure []) examples
                    (lanesSpec synthetic req)

spec ∷ Spec
spec = do
    it "runs every lane in order when no lane is selected" $
        select AllLanes `shouldReturn`
            [ ["shared", "a"], ["shared", "b"], ["solo", "c"]
            , ["old", "d"], ["fresh", "e"] ]
    it "runs exactly the selected lane" $ do
        select (OneLane "shared") `shouldReturn` [["shared", "a"], ["shared", "b"]]
        select (OneLane "solo") `shouldReturn` [["solo", "c"]]
    it "puts an unassigned group in exactly the default lane" $ do
        lanes ← forM (laneNames synthetic) $ \n → (,) n <$> select (OneLane n)
        [ n | (n, xs) ← lanes, ["fresh", "e"] `elem` xs ] `shouldBe` ["rest"]
        concatMap snd lanes `shouldMatchList` [ ["shared", "a"], ["shared", "b"]
                                             , ["solo", "c"], ["old", "d"], ["fresh", "e"] ]
    it "refuses an unknown lane, naming the known ones" $
        case lanesSpec synthetic (OneLane "nope") of
            Right _ → expectationFailure "an unknown lane was accepted"
            Left err → do
                err `shouldSatisfy` ("unknown headless lane \"nope\"" `isInfixOf`)
                err `shouldSatisfy` ("shared, solo, rest (default)" `isInfixOf`)
    it "strips its own flags and leaves Hspec's" $ do
        parseLaneArgs ["--dry-run", "--lane", "solo", "--match", "x"]
            `shouldBe` Right (OneLane "solo", ["--dry-run", "--match", "x"])
        parseLaneArgs ["--lane=solo"] `shouldBe` Right (OneLane "solo", [])
        parseLaneArgs ["--list-lanes"] `shouldBe` Right (ListLanes, [])
        parseLaneArgs ["--lane-self-test", "--dry-run"] `shouldBe` Right (SelfTest, ["--dry-run"])
        parseLaneArgs ["--format=failed-examples"]
            `shouldBe` Right (AllLanes, ["--format=failed-examples"])
    it "rejects a missing, empty or repeated lane flag" $
        forM_ [ ["--lane"], ["--lane="], ["--lane", "a", "--lane", "b"]
              , ["--lane", "a", "--list-lanes"], ["--lane-self-test", "--lane", "a"] ] $
            \args → parseLaneArgs args `shouldSatisfy` either (const True) (const False)
    it "writes inventory paths as ASCII-only JSON" $
        inventoryLine ["Unit \"x\"", "≥ 1"] "a\\b 😀" `shouldBe`
            "headless-inventory-item [\"Unit \\\"x\\\"\",\"\\u2265 1\",\"a\\\\b \\ud83d\\ude00\"]"
