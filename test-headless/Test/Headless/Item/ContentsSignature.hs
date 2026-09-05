-- | Pure tests for 'itemContentsSig' (#1597): the recursive signature
--   that decides whether two ITEM-containers are interchangeable enough
--   to merge into one inventory row.
--
--   Asserted one field at a time, in the style the PARENT row key is
--   already pinned in 'Test.Headless.UI.ItemList.Model' — because the
--   two are one contract seen at two depths. A bandage's quality and
--   realized weight split a row while the bandage sits in a unit's
--   inventory, so they must split a row when the same bandage sits
--   inside a kit. The two deliberate exclusions ('iiInstanceId',
--   'iiTemp') are asserted in the same style, since silently GAINING
--   either one is the regression this gate exists to catch.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item.ContentsSignature"'@.
module Test.Headless.Item.ContentsSignature (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Item.Types
    ( ItemInstance(..), emptyItemManager, itemContentsSig, itemTotalWeight )

-- | A child item with every keyed field at a known baseline, so a case
--   can diverge exactly one of them and nothing else.
child ∷ ItemInstance
child = ItemInstance
    { iiDefName     = "bandage"
    , iiCurrentFill = 2
    , iiQuality     = 80
    , iiCondition   = 90
    , iiWeight      = 0.25
    , iiSharpness   = 30
    , iiContents    = []
    , iiInstanceId  = 1
    , iiTemp        = Nothing
    , iiBulk        = Just 0.5
    , iiStorage     = Nothing
    }

-- | A container holding exactly the children given. Its OWN fields are
--   irrelevant to its signature — only its contents are keyed — so they
--   stay at the same baseline throughout.
kit ∷ Text → [ItemInstance] → ItemInstance
kit name contents = child
    { iiDefName    = name
    , iiContents   = contents
    , iiInstanceId = 900
    }

-- | The signature of a container holding one child shaped by @mutate@.
sigWith ∷ (ItemInstance → ItemInstance) → Text
sigWith mutate = itemContentsSig (kit "first_aid_kit" [mutate child])

-- | The same, one level deeper: a toolbox holding a kit holding the
--   child. Every rule the flat case pins must survive the recursion.
deepSigWith ∷ (ItemInstance → ItemInstance) → Text
deepSigWith mutate =
    itemContentsSig (kit "toolbox" [kit "first_aid_kit" [mutate child]])

spec ∷ Spec
spec = do

    describe "empty contents" $ do
        it "an item holding nothing signs as the empty text" $
            itemContentsSig child `shouldBe` T.empty

        it "a container emptied of its contents signs as the empty text" $
            itemContentsSig (kit "first_aid_kit" []) `shouldBe` T.empty

    describe "represented child fields (each splits a row)" $ do
        it "a child differing only in quality gives unequal signatures" $
            sigWith id `shouldNotBe` sigWith (\c → c { iiQuality = 40 })

        it "a child differing only in weight gives unequal signatures" $
            sigWith id `shouldNotBe` sigWith (\c → c { iiWeight = 0.75 })

        it "a child differing only in fill gives unequal signatures" $
            sigWith id `shouldNotBe` sigWith (\c → c { iiCurrentFill = 1 })

        it "a child differing only in condition gives unequal signatures" $
            sigWith id `shouldNotBe` sigWith (\c → c { iiCondition = 50 })

        it "a child differing only in sharpness gives unequal signatures" $
            sigWith id `shouldNotBe` sigWith (\c → c { iiSharpness = 10 })

        it "a child differing only in its own nested contents gives unequal signatures" $
            sigWith id `shouldNotBe` sigWith (\c → c { iiContents = [child] })

    describe "excluded child fields (neither splits a row)" $ do
        it "a child differing only in tracked temperature gives EQUAL signatures" $
            sigWith id `shouldBe` sigWith (\c → c { iiTemp = Just 60 })

        it "two tracked temperatures still give EQUAL signatures" $
            sigWith (\c → c { iiTemp = Just 5 })
                `shouldBe` sigWith (\c → c { iiTemp = Just 60 })

        it "a child differing only in instance id gives EQUAL signatures" $
            sigWith id `shouldBe` sigWith (\c → c { iiInstanceId = 4242 })

    describe "one level deeper (the signature recurses)" $ do
        it "a grandchild differing only in quality gives unequal signatures" $
            deepSigWith id `shouldNotBe` deepSigWith (\c → c { iiQuality = 40 })

        it "a grandchild differing only in weight gives unequal signatures" $
            deepSigWith id `shouldNotBe` deepSigWith (\c → c { iiWeight = 0.75 })

        it "a grandchild differing only in fill gives unequal signatures" $
            deepSigWith id `shouldNotBe` deepSigWith (\c → c { iiCurrentFill = 1 })

        it "a grandchild differing only in condition gives unequal signatures" $
            deepSigWith id `shouldNotBe` deepSigWith (\c → c { iiCondition = 50 })

        it "a grandchild differing only in sharpness gives unequal signatures" $
            deepSigWith id `shouldNotBe` deepSigWith (\c → c { iiSharpness = 10 })

        it "a grandchild differing only in tracked temperature gives EQUAL signatures" $
            deepSigWith id `shouldBe` deepSigWith (\c → c { iiTemp = Just 60 })

        it "a grandchild differing only in instance id gives EQUAL signatures" $
            deepSigWith id `shouldBe` deepSigWith (\c → c { iiInstanceId = 4242 })

    -- The aggregate row key already carries a container's recursive
    -- TOTAL weight, so a lone child getting heavier splits the row
    -- through that path too. This is the case it cannot see: the same
    -- total, distributed differently among distinguishable children.
    -- Only 'iiWeight' inside the signature separates these two.
    describe "equal total weight, different distribution" $ do
        let light = child { iiDefName = "bandage",   iiWeight = 1 }
            heavy = child { iiDefName = "splint",    iiWeight = 3 }
            kitA  = kit "first_aid_kit"
                        [ light, heavy ]
            kitB  = kit "first_aid_kit"
                        [ light { iiWeight = 3 }, heavy { iiWeight = 1 } ]

        it "the two kits weigh exactly the same in total" $
            itemTotalWeight emptyItemManager kitA
                `shouldBe` itemTotalWeight emptyItemManager kitB

        it "and still sign differently, so their rows do not merge" $
            itemContentsSig kitA `shouldNotBe` itemContentsSig kitB

    describe "order independence (unchanged)" $
        it "the same children in a different order give equal signatures" $ do
            let a = child { iiDefName = "bandage" }
                b = child { iiDefName = "splint", iiWeight = 3 }
            itemContentsSig (kit "first_aid_kit" [a, b])
                `shouldBe` itemContentsSig (kit "first_aid_kit" [b, a])
