-- | The RELATIONAL domain of a unit's authored @body_parts@ list
--   (#2348): ids are unique, every @parent@ names a part of the same
--   unit, every parent chain reaches a root, and a @targetable: false@
--   part has a parent.
--
--   @data/units/*.yaml@ used to be accepted verbatim: @id@ and @parent@
--   decoded as free text and the unit decoder ran no check over the
--   list at all. Neither consumer can defend itself, and the two
--   disagree about what a repeat means — target selection scores every
--   authored ENTRY (@Combat.Resolution.Strike@ pairs each part with
--   each attack kind), while damage resolution resolves the chosen id
--   through a @HashMap@ that keeps the LAST entry
--   (@Combat.Resolution.Common.bodyPartIndex@). One hit is therefore
--   drawn with one duplicate's @area_weight@ and @tactical_value@ and
--   then resolved against the other's tissue, vital flag and layers. A
--   part whose @parent@ resolves to nothing, meanwhile, is never
--   allocated damage and never severed, while startup still reports the
--   unit loaded.
--
--   The fix is at the AUTHORING boundary and nowhere else (requirement
--   4 keeps @bodyPartIndex@ and the selection path exactly as they
--   are), so this spec gates the DECODER. Four parts:
--
--     * __rejection__ — every requirement-1 and requirement-2 branch,
--       asserted through the real 'loadUnitYamlOutcome' on a real file,
--       because whole-FILE rejection (the established
--       "Engine.Asset.YamlList" contract) is half of what is under
--       test: the loader must hand back 'Nothing' AND warn.
--     * __the diagnostic__ — the warning names the FILE (from
--       @Engine.Asset.YamlList.loadYamlListOutcome@), the UNIT, the
--       offending part id(s), and, for a parent fault, the chain it
--       followed, rendered closed for a cycle (requirement 3).
--     * __order and shape__ (requirement 4) — an accepted definition
--       keeps its authored list ORDER, and every hierarchy shape the
--       shipped humanoids already use stays accepted.
--     * __the shipped corpus__ — all eight @data/units/*.yaml@ still
--       load, with their part counts unchanged and the invariants read
--       back off the decoded parts themselves.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Asset.UnitBodyGraph"'@.
module Test.Headless.Asset.UnitBodyGraph (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory
    (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Engine.Asset.YamlUnits
    (UnitYamlDef(..), UnitYamlBodyPart(..), loadUnitYamlOutcome)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )

-- * Fixtures
--
--   Raw source text rather than constructed values: an @id@ authored
--   twice, and a @parent@ naming a part that is not there, are
--   properties of the FILE, and a fixture built from Haskell values
--   would have to bypass the decoder that is under test to express
--   them.

-- | A file holding one unit named @probe_unit@ whose @body_parts@
--   entries are exactly @parts@. @name@ and @sprite@ are the only two
--   fields the decoder requires, so nothing else here can be the reason
--   a fixture is rejected.
probeWith ∷ [String] → String
probeWith parts = unlines $
    [ "units:"
    , "  - name: probe_unit"
    , "    sprite: units/probe/probe.png"
    , "    body_parts:"
    ] ⧺ parts

-- | A second, entirely VALID unit appended after the first — the
--   witness for whole-FILE rejection. A per-definition skip would leave
--   this one registered and spawnable.
validSibling ∷ [String]
validSibling =
    [ "  - name: probe_sound"
    , "    sprite: units/probe/sound.png"
    , "    body_parts:"
    , "      - { id: torso }"
    , "      - { id: head, parent: torso }"
    ]

-- * Assertions

-- | Load @src@ through the REAL loader and require whole-file
--   rejection: the outcome is 'Nothing' — which is what makes the
--   startup loader treat the file as a parse failure (#2203) and
--   register nothing from it — plus exactly one 'CatAsset' 'LevelWarn'
--   naming the file, the unit, and every token in @tokens@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so a token cannot be satisfied by some
--   longer word that happens to contain it. The scrub takes the quotes
--   and brackets the message and the exception wrapper add, and the
--   BACKSLASH, because 'Data.Yaml.ParseException' renders the aeson
--   failure through @show@ — so a double-quoted value arrives escaped.
--   It leaves @-@ and @>@ alone, since a rendered chain is written with
--   @->@ and 'rejectsChain' below matches it verbatim.
rejectsNaming ∷ [String] → String → Expectation
rejectsNaming tokens = rejectsWith $ \msg →
    let ws      = words (map scrub msg)
        wanted  = "probe_unit" : tokens
        missing = [t | t ← wanted, t `notElem` ws]
    in if null missing
         then Nothing
         else Just $ "the warning does not name " ⧺ show missing
  where
    scrub c = if c `elem` ("'\"(),:;=[]\\" ∷ String) then ' ' else c

-- | 'rejectsNaming' plus the exact rendered chain, matched as a
--   SUBSTRING. A chain is an ordered fact — @\'a\' -> \'b\' -> \'a\'@
--   and @\'b\' -> \'a\' -> \'b\'@ name the same three hops but only one
--   of them is the loop the author wrote — and word tokens cannot
--   express order, so the rendering is pinned literally.
rejectsChain ∷ [String] → String → String → Expectation
rejectsChain tokens chain = rejectsWith $ \msg →
    let ws      = words (map scrub msg)
        missing = [t | t ← "probe_unit" : tokens, t `notElem` ws]
    in if not (null missing)
         then Just $ "the warning does not name " ⧺ show missing
         else if chain `isInfixOf` msg
           then Nothing
           else Just $ "the warning does not render the chain " ⧺ show chain
  where
    scrub c = if c `elem` ("'\"(),:;=[]\\" ∷ String) then ' ' else c

-- | Require whole-file rejection and hand the single captured warning
--   to @judge@, which returns 'Nothing' when the message is acceptable
--   or 'Just' the complaint. The file path is asserted here, once, for
--   every caller.
rejectsWith ∷ (String → Maybe String) → String → Expectation
rejectsWith judge src =
    withTempYaml "probe_units.yaml" src $ \path → do
        (logger, entriesRef) ← callbackLogger
        outcome ← loadUnitYamlOutcome logger path
        outcome `shouldBe` Nothing
        entries ← readIORef entriesRef
        case entries of
            [entry] → do
                leLevel entry `shouldBe` LevelWarn
                leCategory entry `shouldBe` CatAsset
                let msg = T.unpack (leMessage entry)
                unless (path `isInfixOf` msg) $ expectationFailure $
                    "rejected, but the warning does not name the file "
                    ⧺ path ⧺ ": " ⧺ msg
                case judge msg of
                    Nothing  → pure ()
                    Just why → expectationFailure $
                        "rejected, but " ⧺ why ⧺ ": " ⧺ msg
            other → expectationFailure $
                "expected exactly one captured log entry, got "
                ⧺ show (length other)

-- | Load @src@ and hand the decoded @probe_unit@ back, failing loudly
--   if the file was rejected — an accepted fixture is only evidence
--   when the definition it produced is inspected.
acceptedProbe ∷ String → (UnitYamlDef → Expectation) → Expectation
acceptedProbe src check =
    withTempYaml "probe_units.yaml" src $ \path → do
        (logger, _) ← callbackLogger
        outcome ← loadUnitYamlOutcome logger path
        case outcome of
            Just [def] → check def
            other      → expectationFailure $
                "expected exactly one accepted definition, got "
                ⧺ show (fmap length other)

spec ∷ Spec
spec = do
    describe "duplicate part ids (requirement 1)" $ do
        it "rejects a repeated id, which targeting and damage \
           \resolution read as two parts and one part respectively" $
            rejectsNaming ["repeats", "head"] $ probeWith
                [ "      - { id: torso }"
                , "      - { id: head, parent: torso }"
                , "      - { id: head, parent: torso }" ]

        it "names EVERY repeated id, so a file with three broken ids is \
           \one edit rather than three loader runs" $
            rejectsNaming ["repeats", "head", "arm"] $ probeWith
                [ "      - { id: torso }"
                , "      - { id: head, parent: torso }"
                , "      - { id: arm,  parent: torso }"
                , "      - { id: head, parent: torso }"
                , "      - { id: arm,  parent: torso }" ]

        -- The BROKEN parent is on the LAST @arm@ deliberately: an index
        -- built from an ambiguous list keeps the last entry, so this is
        -- the arrangement in which a graph walk run too early WOULD find
        -- a fault and report it. With the duplicates on the other order
        -- the walk finds nothing and the assertion holds vacuously.
        it "reports the duplicate ids alone when a parent fault is also \
           \present — a chain walked while an id is ambiguous depends \
           \on which duplicate was arbitrarily picked, so it is not \
           \reported at all" $
            rejectsWith judgeDuplicatesOnly $ probeWith
                [ "      - { id: torso }"
                , "      - { id: arm, parent: torso }"
                , "      - { id: arm, parent: ghost_shoulder }" ]

    describe "parent references (requirement 1)" $ do
        it "rejects a parent that names no part of the same unit, \
           \naming the whole followed chain and where it ran out" $
            rejectsChain ["hand", "ghost_upper_arm", "missing"]
                         "'hand' -> 'forearm' -> 'ghost_upper_arm'" $
                probeWith
                    [ "      - { id: torso }"
                    , "      - { id: hand, parent: forearm }"
                    , "      - { id: forearm, parent: ghost_upper_arm }" ]

        it "rejects a self-parent, rendering the loop CLOSED so the \
           \author sees the cycle rather than a one-part chain" $
            rejectsChain ["root"] "'loop_part' -> 'loop_part'" $
                probeWith
                    [ "      - { id: torso }"
                    , "      - { id: loop_part, parent: loop_part }" ]

        it "rejects a two-part cycle, rendering the loop closed" $
            rejectsChain ["root"] "'alpha' -> 'beta' -> 'alpha'" $
                probeWith
                    [ "      - { id: torso }"
                    , "      - { id: alpha, parent: beta }"
                    , "      - { id: beta,  parent: alpha }" ]

        it "renders the loop ALONE when a valid part hangs off a cycle, \
           \rather than the lasso the walk happened to enter it by" $
            rejectsWith judgeBareLoop $ probeWith
                [ "      - { id: torso }"
                , "      - { id: finger, parent: alpha, targetable: false }"
                , "      - { id: alpha, parent: beta }"
                , "      - { id: beta,  parent: alpha }" ]

    describe "parentless subparts (requirement 2)" $ do
        it "rejects a targetable: false part with no parent, which \
           \nothing aims at and nothing allocates to" $
            rejectsNaming ["targetable", "false", "gut"] $ probeWith
                [ "      - { id: torso }"
                , "      - { id: gut, targetable: false }" ]

        it "names every parentless subpart in the list" $
            rejectsNaming ["targetable", "gut", "marrow"] $ probeWith
                [ "      - { id: torso }"
                , "      - { id: gut, targetable: false }"
                , "      - { id: marrow, targetable: false }" ]

        it "accepts a parentless TARGETABLE part — a macro-part is a \
           \root, and requiring a parent of one would reject every \
           \shipped torso" $
            acceptedProbe (probeWith [ "      - { id: torso }" ]) $ \def →
                map uybpId (uydBodyParts def) `shouldBe` ["torso"]

    describe "whole-file rejection (requirement 3)" $
        it "drops EVERY unit in the file, not just the offending one — \
           \the established Engine.Asset.YamlList contract, and what \
           \keeps the valid sibling out of the registry too" $
            rejectsNaming ["repeats", "head"] $ unlines $
                lines (probeWith
                    [ "      - { id: torso }"
                    , "      - { id: head, parent: torso }"
                    , "      - { id: head, parent: torso }" ])
                ⧺ validSibling

    describe "accepted definitions are unchanged (requirement 4)" $ do
        it "keeps the AUTHORED list order — deliberately neither \
           \alphabetical nor parent-before-child, so a sort or a \
           \topological reshuffle is caught" $
            acceptedProbe
                (probeWith
                    [ "      - { id: hand, parent: forearm, targetable: false }"
                    , "      - { id: torso }"
                    , "      - { id: forearm, parent: torso }"
                    , "      - { id: abdomen, parent: torso }" ]) $ \def →
                map uybpId (uydBodyParts def)
                    `shouldBe` ["hand", "torso", "forearm", "abdomen"]

        it "accepts every hierarchy shape the shipped humanoids already \
           \use: a targetable part under a targetable one, a targetable \
           \part under a NON-targetable one, and a non-targetable part \
           \under a non-targetable one" $
            acceptedProbe
                (probeWith
                    [ "      - { id: torso }"
                    , "      - { id: head, parent: torso }"
                    , "      - { id: gut, parent: torso, targetable: false }"
                    , "      - { id: liver, parent: gut, targetable: false }"
                    , "      - { id: horn, parent: gut }" ]) $ \def →
                map (\p → (uybpId p, uybpParent p, uybpTargetable p))
                    (uydBodyParts def)
                  `shouldBe`
                    [ ("torso", Nothing,      True)
                    , ("head",  Just "torso", True)
                    , ("gut",   Just "torso", False)
                    , ("liver", Just "gut",   False)
                    , ("horn",  Just "gut",   True) ]

        it "accepts a FOREST — several roots is not a fault, and the \
           \requirement is that each chain terminates, not that they \
           \all terminate at the same part" $
            acceptedProbe
                (probeWith
                    [ "      - { id: torso }"
                    , "      - { id: head, parent: torso }"
                    , "      - { id: tail }"
                    , "      - { id: tail_tip, parent: tail }" ]) $ \def →
                map uybpId (uydBodyParts def)
                    `shouldBe` ["torso", "head", "tail", "tail_tip"]

        it "accepts an EMPTY body_parts list, which the three \
           \minimal shipped units author" $
            acceptedProbe
                (unlines [ "units:"
                         , "  - name: probe_unit"
                         , "    sprite: units/probe/probe.png" ]) $ \def →
                uydBodyParts def `shouldBe` []

        it "accepts an explicit parent: null as a root, which is how \
           \every shipped torso spells it" $
            acceptedProbe
                (probeWith
                    [ "      - { id: torso, parent: null }"
                    , "      - { id: head, parent: torso }" ]) $ \def →
                map uybpParent (uydBodyParts def)
                    `shouldBe` [Nothing, Just "torso"]

    describe "the shipped corpus" $ do
        forM_ shippedUnits $ \(file, name, partCount) →
            it (T.unpack file <> " still loads with " <> show partCount
                <> " body parts") $ do
                (logger, _) ← callbackLogger
                outcome ← loadUnitYamlOutcome logger (shippedPath file)
                case outcome of
                    Just [def] → do
                        uydName def `shouldBe` name
                        length (uydBodyParts def) `shouldBe` partCount
                    other → expectationFailure $
                        "expected one definition, got "
                        ⧺ show (fmap length other)

        it "every shipped part list satisfies the invariants the \
           \decoder now enforces — read back off the decoded parts \
           \themselves rather than off the decoder's own helpers, so a \
           \check that silently loosened is still caught" $ do
            (logger, _) ← callbackLogger
            defs ← forM shippedUnits $ \(file, _, _) →
                loadUnitYamlOutcome logger (shippedPath file)
            concat (catMaybes defs) `shouldSatisfy` all wellFormed

-- | The closed-loop rule, read off the message. @finger@ is not part of
--   the cycle — it merely hangs off it, and the walk reaches the loop
--   through it — so a chain that still carries it is the lasso rather
--   than the loop. Asserting the loop's rendering alone would not catch
--   that: @\'finger\' -> \'alpha\' -> \'beta\' -> \'alpha\'@ CONTAINS the
--   loop as a substring.
judgeBareLoop ∷ String → Maybe String
judgeBareLoop msg
    | not ("'alpha' -> 'beta' -> 'alpha'" `isInfixOf` msg) =
        Just "the warning does not render the closed loop"
    | "finger" `isInfixOf` msg =
        Just "the warning reports the lasso it entered the loop by"
    | otherwise = Nothing

-- | The duplicate-first rule, read off the message: it must name the
--   repeated id and must NOT mention the unknown parent that a graph
--   walk over an ambiguous list would have reported.
judgeDuplicatesOnly ∷ String → Maybe String
judgeDuplicatesOnly msg
    | not ("arm" `elem` ws)      = Just "the warning does not name the id 'arm'"
    | not ("repeats" `elem` ws)  = Just "the warning does not say it repeats"
    | "ghost_shoulder" `isInfixOf` msg =
        Just "the warning reports a parent fault found over ambiguous ids"
    | otherwise                  = Nothing
  where
    ws = words (map scrub msg)
    scrub c = if c `elem` ("'\"(),:;=[]\\" ∷ String) then ' ' else c

-- | Requirements 1 and 2 spelled out independently of the decoder, over
--   a decoded part list.
wellFormed ∷ UnitYamlDef → Bool
wellFormed def =
    length ids ≡ length (Map.keys parentOf)
        ∧ all resolves parts
        ∧ all terminates ids
        ∧ all hasParentIfSubpart parts
  where
    parts    = uydBodyParts def
    ids      = map uybpId parts
    parentOf = Map.fromList [(uybpId p, uybpParent p) | p ← parts]

    resolves p = case uybpParent p of
        Nothing  → True
        Just par → Map.member par parentOf

    hasParentIfSubpart p = uybpTargetable p ∨ isJust (uybpParent p)

    -- Bounded by the part count: a chain that has taken more hops than
    -- there are parts has revisited one, which is a cycle.
    terminates start = go (length ids) start
      where
        go budget cur
            | budget < (0 ∷ Int) = False
            | otherwise = case Map.lookup cur parentOf of
                Just (Just par) → go (budget - 1) par
                _               → True

shippedPath ∷ Text → FilePath
shippedPath file = "data" </> "units" </> T.unpack file <> ".yaml"

-- | Every shipped @data/units/*.yaml@ with the unit it declares and the
--   body-part count that must not move. Three of the eight author no
--   parts at all — the minimal definitions #1261 promoted — and they
--   are listed for the same reason as the rest: an empty list is a
--   legal shape this change must keep accepting.
shippedUnits ∷ [(Text, Text, Int)]
shippedUnits =
    [ ("acolyte",           "acolyte",           63)
    , ("bear_brown",        "bear_brown",         8)
    , ("nomad_primitive",   "nomad_primitive",   63)
    , ("red_squirrel",      "red_squirrel",       8)
    , ("technomule",        "technomule",         8)
    , ("tiller",            "tiller",             0)
    , ("unknown_unit",      "unknown_unit",       0)
    , ("white_tailed_deer", "white_tailed_deer",  0)
    ]

-- * Harness

callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

withTempYaml ∷ FilePath → String → (FilePath → IO a) → IO a
withTempYaml name contents action = do
    tmp ← getTemporaryDirectory
    let dir  = tmp </> "synarchy-unit-body-graph-spec"
        path = dir </> name
    createDirectoryIfMissing True dir
    writeFile path contents
    action path `finally` removeDirectoryRecursive dir
