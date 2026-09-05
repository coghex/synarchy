{-# LANGUAGE ScopedTypeVariables #-}
-- | The "Tutorial definitions" gate (issue #957, phase 1 of the
--   tutorial epic #956).
--
--   Two halves:
--
--   * 'spec' — pure. Decodes the shipped
--     @data/tutorials/first_session.yaml@ through the real loader and
--     asserts the authored hierarchy, ordering, and per-objective
--     fields; then drives 'validateTutorialDoc' over one fixture per
--     validation failure class, asserting the PRECISE error rather
--     than merely that something failed.
--   * 'luaSpec' — rides the shared headless engine. A smoke assertion
--     that the read-only Lua surface the tutorial runtime will consume
--     (requirement 6) actually exposes the same hierarchy and order,
--     driven through the real capability projection so the wiring
--     itself is covered, not just the internal registry.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Tutorial definitions"'@.
module Test.Headless.Tutorial.Definitions (spec, luaSpec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import qualified HsLua as Lua
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability, toCoreCapability)
import Engine.Core.Capability.ContentRegistries
  (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Asset.YamlTutorials
import Engine.Scripting.Lua.API.Tutorial
  (loadTutorialDirFn, getTutorialTreeFn)
import Tutorial.Types
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- | The shipped tree, as the engine loads it.
firstSessionPath ∷ FilePath
firstSessionPath = "data/tutorials/first_session.yaml"

-- | Decode a YAML document from a string and validate it, as one step.
--   Fixtures below are written inline so each failure class is one
--   readable, self-contained document.
validateText ∷ Text → Either TutorialLoadError TutorialTree
validateText src = case Yaml.decodeEither' (TE.encodeUtf8 src) of
    Left err  → error ("fixture failed to parse: " <> show err)
    Right doc → validateTutorialDoc doc

-- | A minimal valid document, as lines, with the objective list left
--   to the caller. Every fixture below is this shape with one thing
--   deliberately broken.
docWith ∷ Text → [Text] → Text
docWith treeId objLines =
    T.unlines (("id: " <> treeId) : "objectives:" : objLines)

-- | One objective's YAML lines. @extra@ appends relationship lines.
obj ∷ Text → Text → Int → [Text] → [Text]
obj oid kind order extra =
    [ "  - id: " <> oid
    , "    kind: " <> kind
    , "    label: \"" <> oid <> " label\""
    , "    tooltip: \"" <> oid <> " tooltip\""
    , "    evaluator: " <> oid <> "_eval"
    , "    order: " <> T.pack (show order)
    ] ⧺ map ("    " <>) extra

-- | @children: [a, b]@ / @subobjectives: [a, b]@ as one flow-sequence
--   line.
refs ∷ Text → [Text] → Text
refs field ids = field <> ": [" <> T.intercalate ", " ids <> "]"

-- | The objective ids of a node's children, in exposed order.
childIds ∷ TutorialNode → [Text]
childIds = map (toId ∘ tnObjective) ∘ tnChildren

-- | The objective ids of a node's subobjectives, in exposed order.
subIds ∷ TutorialNode → [Text]
subIds = map (toId ∘ tnObjective) ∘ tnSubobjectives

-- | Whatever the validator produced, or an hspec failure naming the
--   error.
expectTree ∷ Either TutorialLoadError TutorialTree → IO TutorialTree
expectTree (Right t) = return t
expectTree (Left e)  =
    (expectationFailure ∘ T.unpack) ("unexpected validation failure: "
        <> describeTutorialLoadError e)
    >> fail "unreachable"

-- | The sole element of a list, or an hspec failure naming what was
--   expected. Keeps the tree walk below free of incomplete patterns.
expectOne ∷ String → [α] → IO α
expectOne what xs = case xs of
    [x] → return x
    _   → expectationFailure
            ("expected exactly one " <> what <> ", got "
              <> show (length xs))
          >> fail "unreachable"

-- | Decode the shipped tutorial file, failing the example on a parse
--   error rather than throwing.
loadShippedTree ∷ IO TutorialTree
loadShippedTree = do
    parsed ← Yaml.decodeFileEither firstSessionPath
    case parsed of
        Left (err ∷ Yaml.ParseException) →
            expectationFailure (show err) >> fail "unreachable"
        Right doc → expectTree (validateTutorialDoc doc)

spec ∷ Spec
spec = describe "Tutorial definitions" $ do

    describe "the shipped first_session tree" $ do
        it "loads and exposes the authored hierarchy" $ do
            tree ← loadShippedTree
            ttId tree `shouldBe` activeTutorialTreeId
            let root = ttRoot tree
            toId (tnObjective root) `shouldBe` "first_session_place_portal"
            toLabel (tnObjective root) `shouldBe` "Place portal"
            toKind (tnObjective root) `shouldBe` TutorialFull
            childIds root `shouldBe` ["first_session_secure_water"]
            subIds root `shouldBe` []

            water ← expectOne "child of the root" (tnChildren root)
            toLabel (tnObjective water) `shouldBe` "Secure water source"
            toKind (tnObjective water) `shouldBe` TutorialFull
            childIds water `shouldBe` ["first_session_prepare_expedition"]

            expedition ← expectOne "child of the water objective"
                                   (tnChildren water)
            toLabel (tnObjective expedition) `shouldBe` "Prepare an expedition"
            toKind (tnObjective expedition) `shouldBe` TutorialComposite
            childIds expedition `shouldBe` []
            -- Ordered live subobjectives — water (order 1) then food
            -- (order 2), the authored order.
            subIds expedition `shouldBe`
                [ "first_session_prepare_water"
                , "first_session_prepare_food" ]
            map (toLabel ∘ tnObjective) (tnSubobjectives expedition)
                `shouldBe` ["Prepare water", "Prepare food"]
            map (toKind ∘ tnObjective) (tnSubobjectives expedition)
                `shouldBe` [TutorialSubobjective, TutorialSubobjective]

        it "gives every objective a label, tooltip, and evaluator key" $ do
            tree ← loadShippedTree
            let objectives = map tnObjective (tutorialNodeList (ttRoot tree))
            length objectives `shouldBe` 5
            map toEvaluator objectives `shouldBe`
                [ "place_portal", "secure_water_source", "prepare_expedition"
                , "prepare_water", "prepare_food" ]
            filter (T.null ∘ toLabel) objectives `shouldBe` []
            filter (T.null ∘ toTooltip) objectives `shouldBe` []

    describe "display order" $
        it "sorts a sibling group by order, breaking ties by id" $ do
            tree ← expectTree ∘ validateText $ docWith "first_session"
                (  obj "root" "full" 1 [refs "children" ["zeta", "beta", "alpha"]]
                ⧺ obj "zeta"  "full" 1 []
                ⧺ obj "beta"  "full" 3 []
                ⧺ obj "alpha" "full" 1 [] )
            -- alpha and zeta tie at order 1; beta's order 3 puts it last
            -- regardless of document position.
            childIds (ttRoot tree) `shouldBe` ["alpha", "zeta", "beta"]

    describe "rejects invalid data" $ do
        it "a tree id other than first_session" $
            validateText (docWith "second_session" (obj "root" "full" 1 []))
                `shouldBe` Left (TutorialWrongTreeId "second_session")

        it "an empty objective list" $
            validateText "id: first_session\nobjectives: []\n"
                `shouldBe` Left TutorialNoObjectives

        it "a duplicate objective id" $
            validateText (docWith "first_session"
                (  obj "root" "full" 1 [refs "children" ["dup"]]
                ⧺ obj "dup"  "full" 1 []
                ⧺ obj "dup"  "full" 2 [] ))
                `shouldBe` Left (TutorialDuplicateId "dup")

        it "a missing label" $
            validateText (T.unlines
                [ "id: first_session"
                , "objectives:"
                , "  - id: root"
                , "    kind: full"
                , "    tooltip: t"
                , "    evaluator: e"
                , "    order: 1" ])
                `shouldBe` Left (TutorialMissingField "root" "label")

        it "a blank tooltip" $
            validateText (T.unlines
                [ "id: first_session"
                , "objectives:"
                , "  - id: root"
                , "    kind: full"
                , "    label: l"
                , "    tooltip: \"   \""
                , "    evaluator: e"
                , "    order: 1" ])
                `shouldBe` Left (TutorialMissingField "root" "tooltip")

        it "a missing evaluator key" $
            validateText (T.unlines
                [ "id: first_session"
                , "objectives:"
                , "  - id: root"
                , "    kind: full"
                , "    label: l"
                , "    tooltip: t"
                , "    order: 1" ])
                `shouldBe` Left (TutorialMissingField "root" "evaluator")

        it "a missing display order" $
            validateText (T.unlines
                [ "id: first_session"
                , "objectives:"
                , "  - id: root"
                , "    kind: full"
                , "    label: l"
                , "    tooltip: t"
                , "    evaluator: e" ])
                `shouldBe` Left (TutorialMissingField "root" "order")

        it "an unknown objective kind" $
            validateText (docWith "first_session" (obj "root" "milestone" 1 []))
                `shouldBe` Left (TutorialUnknownKind "root" "milestone")

        it "a reference to an objective that does not exist" $
            validateText (docWith "first_session"
                (obj "root" "full" 1 [refs "children" ["ghost"]]))
                `shouldBe` Left (TutorialUnknownReference "root" "ghost")

        it "one objective declaring both children and subobjectives" $ do
            let result = validateText (docWith "first_session"
                    (  obj "root" "composite" 1
                         [ refs "children" ["kid"]
                         , refs "subobjectives" ["sub"] ]
                    ⧺ obj "kid" "full" 1 []
                    ⧺ obj "sub" "subobjective" 1 [] ))
            case result of
                Left (TutorialInvalidRelationship oid why) → do
                    oid `shouldBe` "root"
                    why `shouldSatisfy` T.isInfixOf "mutually exclusive"
                other → expectationFailure ("expected a relationship error, got "
                                              <> show other)

        it "a full objective declaring subobjectives" $ do
            let result = validateText (docWith "first_session"
                    (  obj "root" "full" 1 [refs "subobjectives" ["sub"]]
                    ⧺ obj "sub" "subobjective" 1 [] ))
            case result of
                Left (TutorialInvalidRelationship oid _) → oid `shouldBe` "root"
                other → expectationFailure ("expected a relationship error, got "
                                              <> show other)

        it "a composite objective with no subobjectives" $ do
            let result = validateText (docWith "first_session"
                    (obj "root" "composite" 1 []))
            case result of
                Left (TutorialInvalidRelationship oid _) → oid `shouldBe` "root"
                other → expectationFailure ("expected a relationship error, got "
                                              <> show other)

        it "a subobjective named as a child" $ do
            let result = validateText (docWith "first_session"
                    (  obj "root" "full" 1 [refs "children" ["sub"]]
                    ⧺ obj "sub" "subobjective" 1 [] ))
            case result of
                Left (TutorialInvalidRelationship oid why) → do
                    oid `shouldBe` "root"
                    why `shouldSatisfy` T.isInfixOf "as a child"
                other → expectationFailure ("expected a relationship error, got "
                                              <> show other)

        it "a full objective named as a subobjective" $ do
            let result = validateText (docWith "first_session"
                    (  obj "root" "composite" 1 [refs "subobjectives" ["kid"]]
                    ⧺ obj "kid" "full" 1 [] ))
            case result of
                Left (TutorialInvalidRelationship oid why) → do
                    oid `shouldBe` "root"
                    why `shouldSatisfy` T.isInfixOf "as a subobjective"
                other → expectationFailure ("expected a relationship error, got "
                                              <> show other)

        it "a tree whose every objective has a parent (no root)" $
            validateText (docWith "first_session"
                (  obj "a" "full" 1 [refs "children" ["b"]]
                ⧺ obj "b" "full" 1 [refs "children" ["a"]] ))
                `shouldBe` Left TutorialNoRoot

        it "more than one root" $
            validateText (docWith "first_session"
                (  obj "root"  "full" 1 []
                ⧺ obj "other" "full" 1 [] ))
                `shouldBe` Left (TutorialMultipleRoots ["other", "root"])

        it "an objective hanging off no reachable parent" $
            -- `stray` is referenced (so it is not a second root) but its
            -- referrer is itself unreachable from the real root.
            validateText (docWith "first_session"
                (  obj "root"  "full" 1 []
                ⧺ obj "limbo" "full" 1 [refs "children" ["stray"]]
                ⧺ obj "stray" "full" 1 [refs "children" ["limbo"]] ))
                `shouldBe` Left (TutorialUnreachable ["limbo", "stray"])

        it "a reference cycle reachable from the root" $
            validateText (docWith "first_session"
                (  obj "root" "full" 1 [refs "children" ["a"]]
                ⧺ obj "a"    "full" 1 [refs "children" ["b"]]
                ⧺ obj "b"    "full" 1 [refs "children" ["a"]] ))
                `shouldBe` Left (TutorialCycle ["a", "b", "a"])

        it "an objective with two parents" $
            validateText (docWith "first_session"
                (  obj "root" "full" 1 [refs "children" ["a", "b"]]
                ⧺ obj "a"    "full" 1 [refs "children" ["shared"]]
                ⧺ obj "b"    "full" 2 [refs "children" ["shared"]]
                ⧺ obj "shared" "full" 1 [] ))
                `shouldBe` Left (TutorialMultipleParents "shared" ["a", "b"])

        it "the registry never holds a partial tree" $ do
            activeTutorialTree emptyTutorialRegistry `shouldBe` Nothing
            -- A failed validation yields no tree at all, so there is
            -- nothing a caller could publish half of.
            validateText (docWith "first_session"
                (obj "root" "full" 1 [refs "children" ["ghost"]]))
                `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | The read-only Lua exposure (requirement 6) plus the directory
--   contract, driven through the real @content-registries@ projection
--   of the live engine.
--
--   The loader verb is a DIRECTORY verb, so this is where "exactly one
--   tree" is actually testable: presence (a missing or yaml-less
--   directory), uniqueness (two files that both validate), and
--   all-or-nothing (one good file beside one bad one) all fail the
--   whole load and leave @engine.getTutorialTree()@ nil, whatever
--   order the OS lists the directory in.
luaSpec ∷ SpecWith EngineEnv
luaSpec = describe "Tutorial definitions (Lua exposure)" $ do
    it "engine.getTutorialTree() reports the hierarchy in display order" $
      \env → do
        let core = toCoreCapability env
            regs = toContentRegistriesCapability env
        -- Leave the shared engine as we found it: this ref is ours
        -- alone, and no other spec reads it.
        writeIORef (crTutorialRegistryRef regs) emptyTutorialRegistry
        result ← Lua.run $ do
            Lua.openlibs
            loaded ← loadDir core regs shippedTutorialDir
            _ ← getTutorialTreeFn regs
            treeIsTable ← Lua.istable (-1)
            tid ← field "id"
            -- root
            _ ← Lua.getfield (-1) "root"
            rootId ← field "id"
            rootKind ← field "kind"
            rootLabel ← field "label"
            -- root.children[1]
            _ ← Lua.getfield (-1) "children"
            childCount ← Lua.rawlen (-1)
            _ ← Lua.rawgeti (-1) 1
            waterId ← field "id"
            -- .children[1].children[1]
            _ ← Lua.getfield (-1) "children"
            _ ← Lua.rawgeti (-1) 1
            expId ← field "id"
            expKind ← field "kind"
            -- the composite's ordered subobjectives
            _ ← Lua.getfield (-1) "subobjectives"
            subCount ← Lua.rawlen (-1)
            _ ← Lua.rawgeti (-1) 1
            sub1 ← field "label"
            Lua.pop 1
            _ ← Lua.rawgeti (-1) 2
            sub2 ← field "label"
            return ( loaded, treeIsTable, tid, rootId, rootKind, rootLabel
                   , childCount, waterId, expId, expKind, subCount
                   , sub1, sub2 )
        let ( loaded, treeIsTable, tid, rootId, rootKind, rootLabel
              , childCount, waterId, expId, expKind, subCount
              , sub1, sub2 ) = result
        loaded `shouldBe` Just 1
        treeIsTable `shouldBe` True
        tid `shouldBe` Just "first_session"
        rootId `shouldBe` Just "first_session_place_portal"
        rootKind `shouldBe` Just "full"
        rootLabel `shouldBe` Just "Place portal"
        childCount `shouldBe` 1
        waterId `shouldBe` Just "first_session_secure_water"
        expId `shouldBe` Just "first_session_prepare_expedition"
        expKind `shouldBe` Just "composite"
        subCount `shouldBe` 2
        sub1 `shouldBe` Just "Prepare water"
        sub2 `shouldBe` Just "Prepare food"
        writeIORef (crTutorialRegistryRef regs) emptyTutorialRegistry

    it "accepts a directory holding exactly one valid tree" $ \env →
        withTutorialDir "one" [("first_session.yaml", validTutorialYaml)] $
          \dir → loadAndQuery env dir `shouldReturn` (Just 1, Just "first_session")

    it "rejects a directory with no tutorial files" $ \env →
        -- A non-YAML file is ignored the same way every other content
        -- directory ignores one, so this is still "no tree present".
        withTutorialDir "empty" [("README.md", "not a tutorial\n")] $
          \dir → loadAndQuery env dir `shouldReturn` (Just 0, Nothing)

    it "rejects a directory that does not exist" $ \env →
        -- A NEVER-CREATED child of a directory this invocation owns,
        -- not that directory itself: an existing but empty directory
        -- loads zero trees too, so only a path nothing ever created
        -- distinguishes "absent" from "present and empty".
        withExclusiveTempDirectory "synarchy_957_absent_tutorials" $ \owned →
            loadAndQuery env (owned ⊘ "absent")
                `shouldReturn` (Just 0, Nothing)

    it "rejects two files that both declare a valid tree" $ \env →
        -- Without this, the winner would be whichever file the OS
        -- listed last.
        withTutorialDir "two"
            [ ("a_first_session.yaml", validTutorialYaml)
            , ("b_first_session.yaml", validTutorialYaml) ] $
          \dir → loadAndQuery env dir `shouldReturn` (Just 0, Nothing)

    it "rejects a valid file sitting beside an invalid one" $ \env →
        withTutorialDir "mixed"
            [ ("a_valid.yaml",   validTutorialYaml)
            , ("b_invalid.yaml", invalidTutorialYaml) ] $
          \dir → loadAndQuery env dir `shouldReturn` (Just 0, Nothing)

    it "rejects the same pair in the opposite directory order" $ \env →
        -- The mirror of the case above: the outcome must not depend on
        -- which file is read first.
        withTutorialDir "mixed-rev"
            [ ("a_invalid.yaml", invalidTutorialYaml)
            , ("b_valid.yaml",   validTutorialYaml) ] $
          \dir → loadAndQuery env dir `shouldReturn` (Just 0, Nothing)

    it "a failed load leaves no tree from an earlier successful one" $
      \env → do
        let regs = toContentRegistriesCapability env
        _ ← loadAndQuery env shippedTutorialDir
        before ← readIORef (crTutorialRegistryRef regs)
        activeTutorialTree before `shouldSatisfy` isJust
        withTutorialDir "stale" [("broken.yaml", invalidTutorialYaml)] $
          \dir → loadAndQuery env dir `shouldReturn` (Just 0, Nothing)
        writeIORef (crTutorialRegistryRef regs) emptyTutorialRegistry
  where
    -- Read a string field of the table on top of the stack, leaving the
    -- table itself in place.
    field ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Text)
    field name = do
        _ ← Lua.getfield (-1) name
        v ← Lua.tostring (-1)
        Lua.pop 1
        return (TE.decodeUtf8Lenient ⊚ v)

    -- One engine.loadTutorialDir(dir) call, leaving the stack exactly
    -- as it found it. `loadTutorialDirFn` reads its argument at the
    -- ABSOLUTE index 1, so the stack must be restored between calls.
    loadDir ∷ CoreCapability → ContentRegistriesCapability → FilePath
            → Lua.LuaE Lua.Exception (Maybe Lua.Integer)
    loadDir core regs path = do
        top ← Lua.gettop
        Lua.pushstring (TE.encodeUtf8 (T.pack path))
        _ ← loadTutorialDirFn core regs
        r ← Lua.tointeger (-1)
        Lua.settop top
        return r

    -- engine.getTutorialTree()'s tree id, or Nothing when it returns nil.
    queryTreeId ∷ ContentRegistriesCapability
                → Lua.LuaE Lua.Exception (Maybe Text)
    queryTreeId regs = do
        top ← Lua.gettop
        _ ← getTutorialTreeFn regs
        isNil ← Lua.isnil (-1)
        r ← if isNil then return Nothing else field "id"
        Lua.settop top
        return r

    -- Load one directory and read the result back, both through the
    -- real Lua verbs.
    loadAndQuery ∷ EngineEnv → FilePath
                 → IO (Maybe Lua.Integer, Maybe Text)
    loadAndQuery env dir = do
        let core = toCoreCapability env
            regs = toContentRegistriesCapability env
        Lua.run $ do
            Lua.openlibs
            ok ← loadDir core regs dir
            tid ← queryTreeId regs
            return (ok, tid)

-- | The shipped tutorial directory, as boot loads it.
shippedTutorialDir ∷ FilePath
shippedTutorialDir = "data/tutorials"

-- | A throwaway tutorial directory holding exactly the given files.
withTutorialDir ∷ String → [(FilePath, String)] → (FilePath → IO α) → IO α
withTutorialDir name files act =
    withExclusiveTempDirectory ("synarchy_957_tutorials_" <> name) $ \dir → do
        forM_ files $ \(f, contents) → writeFile (dir ⊘ f) contents
        act dir

-- | A minimal complete tree — enough to validate, small enough to read.
validTutorialYaml ∷ String
validTutorialYaml = unlines
    [ "id: first_session"
    , "objectives:"
    , "  - id: root"
    , "    kind: full"
    , "    label: l"
    , "    tooltip: t"
    , "    evaluator: e"
    , "    order: 1"
    ]

-- | Parses as YAML, fails validation (`root` names an objective that
--   does not exist) — the failure class a stray or half-edited file in
--   @data/tutorials/@ would hit.
invalidTutorialYaml ∷ String
invalidTutorialYaml = unlines
    [ "id: first_session"
    , "objectives:"
    , "  - id: root"
    , "    kind: full"
    , "    label: l"
    , "    tooltip: t"
    , "    evaluator: e"
    , "    order: 1"
    , "    children: [ghost]"
    ]
