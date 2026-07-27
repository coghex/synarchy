{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
-- | YAML loader for @data/tutorials/*.yaml@ (#957, phase 1 of the
--   tutorial epic #956).
--
--   One file IS one complete tree — no wrapping list, like
--   "Engine.Asset.YamlLootTables" — so every WHOLE-TREE check runs at
--   one file's load and needs no cross-file finalization step (the boot
--   loader queues one @engine.loadTutorialYaml@ call per file and could
--   not provide one). The all-or-nothing unit is the tree: a file that
--   fails any check below publishes NOTHING, so the registry is either
--   the fully validated tree or explicitly empty, never partial.
--
--   Presentation and evaluator fields are parsed as OPTIONAL at the
--   Aeson layer and required by 'validateTutorialDoc' instead, so a
--   missing one reports as an actionable
--   \"objective X is missing required field Y\" naming the offending
--   objective rather than as an anonymous decoder failure.
module Engine.Asset.YamlTutorials
  ( TutorialYamlObjective(..)
  , TutorialYamlDoc(..)
  , TutorialLoadError(..)
  , describeTutorialLoadError
  , validateTutorialDoc
  , loadTutorialYaml
  ) where

import UPrelude
import GHC.Generics (Generic)
import Data.List (sortOn, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logError, LogCategory(..))
import Tutorial.Types

-- | One objective as authored. Objectives are a FLAT list joined by id
--   references (not inline nesting), which is what makes \"unknown
--   reference\", \"multiple parents\", and \"cycle\" real, checkable
--   failure classes rather than shapes the format cannot express.
data TutorialYamlObjective = TutorialYamlObjective
  { tyoId            ∷ !Text
  , tyoKind          ∷ !(Maybe Text)
  , tyoLabel         ∷ !(Maybe Text)
  , tyoTooltip       ∷ !(Maybe Text)
  , tyoEvaluator     ∷ !(Maybe Text)
  , tyoOrder         ∷ !(Maybe Int)
  , tyoChildren      ∷ ![Text]
  , tyoSubobjectives ∷ ![Text]
  } deriving (Show, Eq, Generic)

instance FromJSON TutorialYamlObjective where
  parseJSON = withObject "TutorialYamlObjective" $ \v → TutorialYamlObjective
    ⊚ v .:  "id"
    ⊛ v .:? "kind"
    ⊛ v .:? "label"
    ⊛ v .:? "tooltip"
    ⊛ v .:? "evaluator"
    ⊛ v .:? "order"
    ⊛ v .:? "children"      .!= []
    ⊛ v .:? "subobjectives" .!= []

-- | A whole tutorial file: one tree id plus its flat objective list.
data TutorialYamlDoc = TutorialYamlDoc
  { tydId         ∷ !Text
  , tydObjectives ∷ ![TutorialYamlObjective]
  } deriving (Show, Eq, Generic)

instance FromJSON TutorialYamlDoc where
  parseJSON = withObject "TutorialYamlDoc" $ \v → TutorialYamlDoc
    ⊚ v .:  "id"
    ⊛ v .:? "objectives" .!= []

-- | Every way a tutorial file can fail validation. One constructor per
--   failure class named by the issue, so a test can assert the PRECISE
--   class rather than string-matching a rendered message.
data TutorialLoadError
  = TutorialWrongTreeId !Text
    -- ^ Tree id is not 'activeTutorialTreeId'. This slice supports
    --   exactly one active tree; any other id would load silently
    --   inert.
  | TutorialNoObjectives
    -- ^ Malformed tree: no objectives at all.
  | TutorialDuplicateId !Text
  | TutorialMissingField !Text !Text
    -- ^ Objective id, missing (or blank) required field name.
  | TutorialUnknownKind !Text !Text
    -- ^ Objective id, unrecognized kind spelling.
  | TutorialUnknownReference !Text !Text
    -- ^ Referring objective id, the id it names that does not exist.
  | TutorialInvalidRelationship !Text !Text
    -- ^ Objective id, what is wrong with its kind\/relationship pair.
  | TutorialNoRoot
    -- ^ Every objective is referenced by some parent, so the tree has
    --   no entry point.
  | TutorialMultipleRoots ![Text]
    -- ^ More than one objective is referenced by no parent. An
    --   objective defined but hanging off no parent shows up here.
  | TutorialUnreachable ![Text]
    -- ^ Objectives not reachable from the single root.
  | TutorialCycle ![Text]
    -- ^ The reference path that closes back on itself.
  | TutorialMultipleParents !Text ![Text]
    -- ^ Objective id, the several objectives naming it.
  deriving (Show, Eq, Generic)

-- | Render an error as the actionable one-line message the loader
--   logs. Always names the offending objective(s) where one exists.
describeTutorialLoadError ∷ TutorialLoadError → Text
describeTutorialLoadError err = case err of
  TutorialWrongTreeId tid →
    "tree id '" <> tid <> "' is not the one active tutorial tree '"
    <> activeTutorialTreeId <> "'"
  TutorialNoObjectives →
    "tree declares no objectives"
  TutorialDuplicateId oid →
    "duplicate objective id '" <> oid <> "'"
  TutorialMissingField oid field →
    "objective '" <> oid <> "' is missing required field '" <> field <> "'"
  TutorialUnknownKind oid k →
    "objective '" <> oid <> "' has unknown kind '" <> k
    <> "' (expected full, composite, or subobjective)"
  TutorialUnknownReference oid target →
    "objective '" <> oid <> "' references unknown objective '" <> target <> "'"
  TutorialInvalidRelationship oid why →
    "objective '" <> oid <> "' " <> why
  TutorialNoRoot →
    "no root objective: every objective is referenced by a parent"
  TutorialMultipleRoots roots →
    "expected exactly one root objective, found " <> tshow (length roots)
    <> ": " <> commas roots
  TutorialUnreachable oids →
    "objectives unreachable from the root: " <> commas oids
  TutorialCycle path →
    "reference cycle: " <> T.intercalate " -> " path
  TutorialMultipleParents oid parents →
    "objective '" <> oid <> "' is referenced by " <> tshow (length parents)
    <> " parents: " <> commas parents
  where
    commas = T.intercalate ", " ∘ map (\t → "'" <> t <> "'")
    tshow  = T.pack ∘ show

-- | Validate a parsed tutorial file into the one tree it declares.
--
--   The checks run in a fixed order, chosen so each failure class is
--   reachable with its own fixture and reports the most specific
--   diagnosis available:
--
--   1. tree id, 2. non-empty, 3. duplicate ids, 4. per-objective
--   required fields and kind, 5. unknown references, 6. kind\/
--   relationship combinations, 7. exactly one root (and its kind),
--   8. reachability from that root, 9. cycles, 10. multiple parents.
--
--   Roots come before cycles deliberately: an all-referenced document
--   necessarily contains a cycle, so checking cycles first would make
--   'TutorialNoRoot' unreachable. Cycles come before multiple parents
--   for the mirror reason — a cycle reachable from the root always
--   gives some node a second parent, and \"cycle\" is the more
--   actionable of the two diagnoses.
validateTutorialDoc ∷ TutorialYamlDoc → Either TutorialLoadError TutorialTree
validateTutorialDoc doc = do
  -- 1 / 2. The tree itself.
  unless (tydId doc ≡ activeTutorialTreeId) $
    Left (TutorialWrongTreeId (tydId doc))
  when (null objs) $ Left TutorialNoObjectives

  -- 3. Duplicate ids, in document order.
  case firstDuplicate (map tyoId objs) of
    Just d  → Left (TutorialDuplicateId d)
    Nothing → Right ()

  -- 4. Presentation / evaluator / kind, per objective.
  validated ← traverse validateObjective objs
  let byId = HM.fromList [ (toId o, o) | o ← validated ]
      kindOf oid = toKind ⊚ HM.lookup oid byId

  -- 5. Every reference names a defined objective.
  forM_ objs $ \o →
    forM_ (refsOf o) $ \r →
      unless (HM.member r byId) $
        Left (TutorialUnknownReference (tyoId o) r)

  -- 6. Kind and relationship must agree — with each other, and with
  --    the kind of whatever each list names.
  forM_ objs $ \o → checkRelationships kindOf o

  -- 7. Exactly one root, and it must be a full-objective kind.
  let refCounts = HM.fromListWith (+)
        [ (r, 1 ∷ Int) | o ← objs, r ← refsOf o ]
      roots = [ toId o | o ← validated
              , HM.lookupDefault 0 (toId o) refCounts ≡ 0 ]
  rootId ← case roots of
    []  → Left TutorialNoRoot
    [r] → Right r
    rs  → Left (TutorialMultipleRoots (sort rs))
  when (kindOf rootId ≡ Just TutorialSubobjective) $
    Left (TutorialInvalidRelationship rootId
      ("is the tree root but has kind 'subobjective'; the root must be"
       <> " a full or composite objective"))

  -- 8. Nothing defined may hang off the tree.
  let refsById = HM.fromList [ (tyoId o, refsOf o) | o ← objs ]
      reached  = reachable refsById rootId
      orphans  = sort [ toId o | o ← validated
                      , not (HS.member (toId o) reached) ]
  unless (null orphans) $ Left (TutorialUnreachable orphans)

  -- 9 / 10. Acyclic, single-parent.
  case findCycle refsById rootId of
    Just path → Left (TutorialCycle path)
    Nothing   → Right ()
  forM_ validated $ \o →
    when (HM.lookupDefault 0 (toId o) refCounts > 1) $
      Left (TutorialMultipleParents (toId o)
             (sort [ tyoId p | p ← objs, toId o `elem` refsOf p ]))

  Right TutorialTree
    { ttId   = tydId doc
    , ttRoot = buildNode byId refsMap rootId
    }
  where
    objs = tydObjectives doc
    refsOf o = tyoChildren o ⧺ tyoSubobjectives o
    refsMap = HM.fromList
      [ (tyoId o, (tyoChildren o, tyoSubobjectives o)) | o ← objs ]

-- | Required-field and kind checking for one objective.
validateObjective ∷ TutorialYamlObjective
                  → Either TutorialLoadError TutorialObjective
validateObjective o = do
  let oid = tyoId o
  when (T.null (T.strip oid)) $ Left (TutorialMissingField oid "id")
  kindTxt ← require oid "kind" (tyoKind o)
  kind ← maybe (Left (TutorialUnknownKind oid kindTxt)) Right
                (parseObjectiveKind kindTxt)
  label ← require oid "label" (tyoLabel o)
  tooltip ← require oid "tooltip" (tyoTooltip o)
  evaluator ← require oid "evaluator" (tyoEvaluator o)
  order ← maybe (Left (TutorialMissingField oid "order")) Right (tyoOrder o)
  Right TutorialObjective
    { toId        = oid
    , toKind      = kind
    , toLabel     = label
    , toTooltip   = tooltip
    , toEvaluator = evaluator
    , toOrder     = order
    }
  where
    -- A blank string is a missing field, not a valid one: a nameless
    -- objective or an empty label is not authored data anyone can act
    -- on.
    require oid field mv = case mv of
      Just t | not (T.null (T.strip t)) → Right t
      _                                 → Left (TutorialMissingField oid field)

-- | Check 6: an objective's kind against the relationships it declares,
--   and against the kinds of the objectives those relationships name.
checkRelationships ∷ (Text → Maybe TutorialObjectiveKind)
                   → TutorialYamlObjective
                   → Either TutorialLoadError ()
checkRelationships kindOf o = do
  when (not (null kids) ∧ not (null subs)) $ bad
    ("declares both children and subobjectives; the two relationships"
     <> " are mutually exclusive")
  case kindOf oid of
    Just TutorialFull →
      unless (null subs) $ bad
        "has kind 'full' but declares subobjectives; use kind 'composite'"
    Just TutorialComposite → do
      when (null subs) $ bad
        "has kind 'composite' but declares no subobjectives"
      unless (null kids) $ bad
        "has kind 'composite' but declares children"
    Just TutorialSubobjective → do
      unless (null kids) $ bad
        "has kind 'subobjective' but declares children"
      unless (null subs) $ bad
        "has kind 'subobjective' but declares subobjectives"
    -- Unreachable: check 4 already rejected an unparseable kind, and
    -- check 5 already rejected a reference to a missing objective.
    Nothing → Right ()
  forM_ kids $ \c →
    when (kindOf c ≡ Just TutorialSubobjective) $ bad
      ("names subobjective '" <> c <> "' as a child; a subobjective may"
       <> " only appear in a subobjectives list")
  forM_ subs $ \s →
    unless (kindOf s ≡ Just TutorialSubobjective) $ bad
      ("names '" <> s <> "' as a subobjective, but its kind is not"
       <> " 'subobjective'")
  where
    oid  = tyoId o
    kids = tyoChildren o
    subs = tyoSubobjectives o
    bad  = Left ∘ TutorialInvalidRelationship oid

-- | Ids reachable from a start id, cycle-safe.
reachable ∷ HM.HashMap Text [Text] → Text → HS.HashSet Text
reachable refs start = go HS.empty [start]
  where
    go seen [] = seen
    go seen (x:rest)
      | HS.member x seen = go seen rest
      | otherwise        = go (HS.insert x seen)
                              (HM.lookupDefault [] x refs ⧺ rest)

-- | Depth-first cycle search from the root. Returns the path from the
--   repeated id back to itself, so the message points at the loop.
findCycle ∷ HM.HashMap Text [Text] → Text → Maybe [Text]
findCycle refs start = go [] HS.empty start
  where
    go path onPath x
      | HS.member x onPath = Just (dropWhile (≢ x) (reverse path) ⧺ [x])
      | otherwise =
          firstJust [ go (x : path) (HS.insert x onPath) c
                    | c ← HM.lookupDefault [] x refs ]
    firstJust xs = listToMaybe (catMaybes xs)

-- | Build the validated tree from the root down. Total on a document
--   that passed every check above: single root, single parent,
--   acyclic, all references resolvable.
buildNode ∷ HM.HashMap Text TutorialObjective
          → HM.HashMap Text ([Text], [Text])
          → Text
          → TutorialNode
buildNode byId refs oid = TutorialNode
  { tnObjective     = obj
  , tnChildren      = map (buildNode byId refs) (ordered kids)
  , tnSubobjectives = map (buildNode byId refs) (ordered subs)
  }
  where
    obj = HM.lookupDefault placeholder oid byId
    (kids, subs) = HM.lookupDefault ([], []) oid refs
    -- Sibling display order, ties broken by id so the exposed order is
    -- deterministic even for duplicate `order` values.
    ordered = sortOn $ \i →
      let o = HM.lookupDefault placeholder i byId
      in (toOrder o, toId o)
    placeholder = TutorialObjective oid TutorialFull oid "" "" 0

-- | First value that repeats, in list order.
firstDuplicate ∷ [Text] → Maybe Text
firstDuplicate = go HS.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | HS.member x seen = Just x
      | otherwise        = go (HS.insert x seen) xs

-- | Parse and validate one tutorial YAML file. Returns 'Nothing' with
--   an actionable logged ERROR — naming the file and the offending
--   objective — on either a parse failure or a validation failure, so
--   the caller publishes nothing and boot carries on with an
--   explicitly empty tutorial registry. Mirrors 'loadLootTableYaml''s
--   warn-and-skip convention, at error level: unlike a malformed loot
--   table, a malformed tutorial tree means the game ships with no
--   onboarding at all.
loadTutorialYaml ∷ LoggerState → FilePath → IO (Maybe TutorialTree)
loadTutorialYaml logger path = do
  result ← Yaml.decodeFileEither path
  case result of
    Left err → do
      logError logger CatAsset $ "Failed to parse tutorial YAML "
        <> T.pack path <> ": " <> T.pack (show (err ∷ Yaml.ParseException))
      return Nothing
    Right doc → case validateTutorialDoc doc of
      Left e → do
        logError logger CatAsset $ "Invalid tutorial definition in "
          <> T.pack path <> ": " <> describeTutorialLoadError e
        return Nothing
      Right tree → do
        logDebug logger CatAsset $ "Loaded tutorial tree '" <> ttId tree
          <> "' from " <> T.pack path
        return (Just tree)
