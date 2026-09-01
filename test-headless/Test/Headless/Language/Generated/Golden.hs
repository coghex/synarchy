-- | The pinned golden outputs (#710 requirement 15, #1092
--   requirement 4). Split into two exports rather than one so the
--   façade can keep both blocks exactly where they sit today: the
--   historical versions immediately after version 4's contracts,
--   and version 5's own block after the #1100 orthography and font
--   groups.
module Test.Headless.Language.Generated.Golden
    ( historicalSpec
    , currentSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as M
import Language.Generated.Types
import Language.Generated.Orthography
import Language.Generated.Bound
import Language.Generated.Profile
import Language.Generated.Render
import Language.Generated.Report (boundSlotExpressions)
import Test.Headless.Language.Generated.Support

historicalSpec ∷ Ctx → Spec
historicalSpec Ctx{..} = do
    -- Golden outputs (#710 requirement 15): a change to any of these
    -- pins requires incrementing the language-generator version rather
    -- than silently changing an existing version's output.
    describe "golden outputs (pinned, generator version 1)" $ do
        it "seed 0" $ nativeRenderings 0 `shouldBe`
            [ Right "Jowwem", Right "Sinmoyiawga", Right "Hahaslegaen"
            , Right "Binotlomehyoyimbo", Right "Selibohsaamj" ]

        it "seed 1" $ nativeRenderings 1 `shouldBe`
            [ Right "Uyjac", Right "Gut-yez", Right "Bub-ulay"
            , Right "Dez-ulurla", Right "Yudz-zag" ]

        it "seed 42" $ nativeRenderings 42 `shouldBe`
            [ Right "Rregeg", Right "Jigpa-gyigez", Right "Jiggji-rorjar"
            , Right "Gapzraz-recpog", Right "Zoccce-payi'g" ]

        it "seed 12345" $ nativeRenderings 12345 `shouldBe`
            [ Right "Vurkussuv", Right "Ravvusjirik", Right "Vuvrujakisaj"
            , Right "Wivjasijrivarwir", Right "Ravsikirjas" ]

    -- A SEPARATE block, not a replacement: version 1's pins above stay
    -- exactly as they were (including "Rregeg", the identical-consonant
    -- onset #1094 deliberately preserves in the historical version),
    -- and version 2 gets its own.
    describe "golden outputs (pinned, generator version 2)" $ do
        it "seed 0" $ nativeRenderingsV2 0 `shouldBe`
            [ Right "Lifor", Right "Ilbicbyfviv", Right "Ehsoveslev"
            , Right "Yrejohkifce", Right "Enisnyhcyihirv" ]

        it "seed 1" $ nativeRenderingsV2 1 `shouldBe`
            [ Right "Gun", Right "Asap-pyp", Right "Ynan-tub"
            , Right "Nup-hahny", Right "Bybb-fuf" ]

        it "seed 42" $ nativeRenderingsV2 42 `shouldBe`
            [ Right "Kovta", Right "Tikkyi-revvro", Right "Roybiy-bravri"
            , Right "Tevogr-yartey", Right "Vyirek-rebor'b" ]

        it "seed 12345" $ nativeRenderingsV2 12345 `shouldBe`
            [ Right "Kipahkug", Right "Payihgipayyig", Right "Guruhkuyagyih"
            , Right "Piyugyarpagyur", Right "Yiyagaykuyr" ]

        it "every canonical version-2 name satisfies the output \
           \contract (requirement 6)" $ do
            let allRenderings = concatMap nativeRenderingsV2 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

    -- Version 3's own block, added ALONGSIDE the two above rather than
    -- replacing either (#1095 requirement 6): #1092 keeps every
    -- historical version constructible, so their pins — including
    -- version 1's triple-bearing "Zoccce-payi'g", the defect #1095 fixes
    -- going forward — must keep passing unchanged.
    describe "golden outputs (pinned, generator version 3)" $ do
        it "seed 0" $ nativeRenderingsV3 0 `shouldBe`
            [ Right "Ihec", Right "Vokreryjy", Right "Senycyrosbin"
            , Right "Hovlenefolentysoce", Right "Nytivyvcehybycov" ]

        it "seed 1" $ nativeRenderingsV3 1 `shouldBe`
            [ Right "Tyh", Right "Fyn-ytap", Right "Azapat-put"
            , Right "Byg-anagyzny", Right "Ubupugub-yftyk" ]

        it "seed 42" $ nativeRenderingsV3 42 `shouldBe`
            [ Right "Yokvya", Right "Tabvib-gigbi", Right "Vaktok-bkivra"
            , Right "Kgagatar-vigkor", Right "Vrevte-ragi'b" ]

        it "seed 12345" $ nativeRenderingsV3 12345 `shouldBe`
            [ Right "Ruyri", Right "Puypurugaripkap", Right "Rurkipiyuya"
            , Right "Pukapigipipuk", Right "Kurkaghikurkuyr" ]

        it "every canonical version-3 name satisfies the output \
           \contract (requirement 6)" $ do
            let allRenderings = concatMap nativeRenderingsV3 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

    -- Version 4's own block, added ALONGSIDE the three above rather than
    -- replacing any of them (#1096 requirement 1): #1092 keeps every
    -- historical version constructible, so their pins — version 1's
    -- triple-bearing "Zoccce-payi'g" included — must keep passing
    -- unchanged while this version's dependent slots shorten.
    describe "golden outputs (pinned, generator version 4)" $ do
        it "seed 0" $ nativeRenderingsV4 0 `shouldBe`
            [ Right "Nelyhlon", Right "Kiivenocetethoj", Right "Ysseyhcovyh"
            , Right "Fifytojceiboce", Right "Likihisibtivbev" ]

        it "seed 1" $ nativeRenderingsV4 1 `shouldBe`
            [ Right "Ufupyn", Right "Ahaz-yfypug", Right "Agafak-upag"
            , Right "Zas-fynyny", Right "Ypabab-tah" ]

        it "seed 42" $ nativeRenderingsV4 42 `shouldBe`
            [ Right "Gatir", Right "Vbare-gokig", Right "Kta-terbka"
            , Right "Kevayr-gikre", Right "Yere-btokre'b" ]

        it "seed 12345" $ nativeRenderingsV4 12345 `shouldBe`
            [ Right "Yupar", Right "Ripgakruhupakrahgak", Right "Gagpagupupahirkih"
            , Right "Gahyuyigiyaga", Right "Yakyahapugahruyr" ]

        it "every canonical version-4 name satisfies the output \
           \contract (requirement 6)" $ do
            let allRenderings = concatMap nativeRenderingsV4 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

        -- The one golden that shows the feature: a selected concept
        -- rendered bare, then in each dependent slot of the matrix, in
        -- a real generated language rather than a fixture.
        it "seed 0's bound forms, and one concept through every slot" $ do
            let p  = buildProfileV4 (LangSeed 0)
                lr = rootsFor p
                c  = cid "DAWN"
            M.toList (lrBound lr) `shouldBe`
                [ (cid "CURSE", "syr"), (cid "DAWN", "bois")
                , (cid "MIDNIGHT", "se"), (cid "PROPHET", "sic")
                , (cid "RELIC", "ihovy"), (cid "SPIDER", "hilili")
                , (cid "SPIRE", "yly"), (cid "STAG", "cenin") ]
            M.lookup c (lrFree lr) `shouldBe` Just "boisfen"
            map (\(_, e) → renderNative p lr e)
                (boundSlotExpressions c (headAgainst c)) `shouldBe`
                [ Right "Boisfen", Right "Ocyrobois", Right "Ocyrobois"
                , Right "Ocyroboisoce", Right "Ocyroboisov" ]

currentSpec ∷ Ctx → Spec
currentSpec Ctx{..} = do
    -- Version 5's own golden block, added ALONGSIDE the four above
    -- rather than replacing any of them (#1092 requirement 4): every
    -- historical version stays constructible, so their pins must keep
    -- passing unchanged while this version's names gain their marks.
    describe "golden outputs (pinned, generator version 5)" $ do
        it "seed 0" $ nativeRenderingsV5 0 `shouldBe`
            [ Right "Ovnisij", Right "Soteskebobo", Right "Ocicycjev"
            , Right "Inokohysesoce", Right "Rerojonivfov" ]

        it "seed 1" $ nativeRenderingsV5 1 `shouldBe`
            [ Right "Fuh", Right "Gaf-f\x00E4k", Right "Uz\x00E4g-k\x00E4h"
            , Right "\x00C4\&ftap-puzn\x00E4", Right "Kanab-\x00E4kyg" ]

        it "seed 42" $ nativeRenderingsV5 42 `shouldBe`
            [ Right "Tobra", Right "Kivk\x0105-kogo", Right "Vyibey-kyogi"
            , Right "Gtibk\x0105r-tekva", Right "Bavvko-bgove'b" ]

        it "seed 12345" $ nativeRenderingsV5 12345 `shouldBe`
            [ Right "Pakur", Right "H\x00E0hg\x00F9ypuhuhugikkip"
            , Right "R\x00F9y\x00E0r\x00E0hargakgu"
            , Right "Yupupugug\x00E0gg\x00E0ry\x00E0k", Right "Pak\x00F9h\x00F9hkipr" ]

        -- The one golden that shows the feature: three languages of the
        -- same sample, one unmarked and two carrying different families,
        -- with the inventories their names are drawn from.
        it "seed 0 is unmarked while seeds 1 and 42 carry their own \
           \families" $ do
            let p0 = buildProfileV5 (LangSeed 0)
                p1 = buildProfileV5 (LangSeed 1)
                p42 = buildProfileV5 (LangSeed 42)
            (profileExtendedChars p0, profileDiacritic p0)
                `shouldBe` ("", Nothing)
            (profileExtendedChars p1, profileDiacritic p1)
                `shouldBe` ("\x00E4", Just DiaDiaeresis)
            (profVowels p1, profConsonants p1)
                `shouldBe` ("uya\x00E4", "pbnhgzfkst")
            (profileExtendedChars p42, profileDiacritic p42)
                `shouldBe` ("\x0105", Just DiaOgonek)
            (profVowels p42, profConsonants p42)
                `shouldBe` ("oaei\x0105", "gkyvbtr")
