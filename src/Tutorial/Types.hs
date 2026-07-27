{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
-- | The validated in-memory form of a tutorial definition tree (#957,
--   phase 1 of the tutorial epic #956).
--
--   Pure data: structure, presentation text, ordering, and the stable
--   evaluator keys the Lua tutorial runtime will dispatch on later.
--   Nothing here models progress, completion, or gameplay state — those
--   belong to the later runtime children and are explicitly out of this
--   slice.
--
--   A value of 'TutorialTree' only ever exists having passed
--   'Engine.Asset.YamlTutorials.validateTutorialDoc', so consumers may
--   rely on the invariants that validator enforces: exactly one root,
--   every objective reachable from it exactly once, no cycles, and the
--   'tnChildren' \/ 'tnSubobjectives' relationships mutually exclusive
--   per node.
module Tutorial.Types
  ( TutorialObjectiveKind(..)
  , objectiveKindText
  , parseObjectiveKind
  , TutorialObjective(..)
  , TutorialNode(..)
  , TutorialTree(..)
  , TutorialRegistry(..)
  , activeTutorialTreeId
  , emptyTutorialRegistry
  , singleTutorialRegistry
  , activeTutorialTree
  , tutorialNodeList
  ) where

import UPrelude
import GHC.Generics (Generic)

-- | An objective's explicit kind. Authored in YAML alongside the
--   structural relationship so the two can DISAGREE — an invalid
--   kind/relationship combination is a load-time error rather than a
--   silent coercion (requirement 5).
--
--   * 'TutorialFull' — a full objective. May gate further full
--     objectives through @children@; never has subobjectives.
--   * 'TutorialComposite' — a full objective whose completion is
--     composed of live component requirements. Must have
--     @subobjectives@; never has children.
--   * 'TutorialSubobjective' — a live component requirement of a
--     composite. A leaf, and only ever named from a @subobjectives@
--     list.
data TutorialObjectiveKind
  = TutorialFull
  | TutorialComposite
  | TutorialSubobjective
  deriving (Show, Eq, Generic)

-- | The YAML spelling of a kind — also what the Lua-facing surface
--   reports, so authored data and the runtime agree on one vocabulary.
objectiveKindText ∷ TutorialObjectiveKind → Text
objectiveKindText TutorialFull         = "full"
objectiveKindText TutorialComposite    = "composite"
objectiveKindText TutorialSubobjective = "subobjective"

-- | Inverse of 'objectiveKindText'. 'Nothing' for an unrecognized
--   spelling — the loader turns that into an actionable error naming
--   the offending objective.
parseObjectiveKind ∷ Text → Maybe TutorialObjectiveKind
parseObjectiveKind "full"         = Just TutorialFull
parseObjectiveKind "composite"    = Just TutorialComposite
parseObjectiveKind "subobjective" = Just TutorialSubobjective
parseObjectiveKind _              = Nothing

-- | One objective's own definition — everything except its place in
--   the tree. 'toEvaluator' is a stable key, not a predicate: this
--   slice declares the keys and implements none of them.
data TutorialObjective = TutorialObjective
  { toId        ∷ !Text
    -- ^ Globally unique stable id. Survives label/tooltip rewording,
    --   so persisted progress (a later child) can key on it.
  , toKind      ∷ !TutorialObjectiveKind
  , toLabel     ∷ !Text
    -- ^ Player-facing one-line label.
  , toTooltip   ∷ !Text
    -- ^ Player-facing hover text.
  , toEvaluator ∷ !Text
    -- ^ Stable key the Lua tutorial runtime dispatches on.
  , toOrder     ∷ !Int
    -- ^ Display order WITHIN this objective's sibling group. Ties break
    --   by id, so the exposed order is deterministic regardless.
  } deriving (Show, Eq, Generic)

-- | An objective plus its place in the tree. At most one of the two
--   relationship lists is non-empty (the validator rejects a node
--   declaring both); both are kept in display order.
data TutorialNode = TutorialNode
  { tnObjective     ∷ !TutorialObjective
  , tnChildren      ∷ ![TutorialNode]
    -- ^ Full objectives gated by this objective's completion.
  , tnSubobjectives ∷ ![TutorialNode]
    -- ^ Live component requirements of this composite objective.
  } deriving (Show, Eq, Generic)

-- | A whole tutorial tree, identified by its stable tree id.
data TutorialTree = TutorialTree
  { ttId   ∷ !Text
  , ttRoot ∷ !TutorialNode
  } deriving (Show, Eq, Generic)

-- | Engine-wide tutorial registry. This slice supports exactly one
--   active tree, so the registry is an explicit
--   loaded-or-unavailable slot rather than a map: a tree that fails
--   validation leaves 'Nothing' here (never a partially built tree),
--   which is the state the Lua surface reports as \"no tutorial\".
newtype TutorialRegistry = TutorialRegistry
  { trTree ∷ Maybe TutorialTree
  } deriving (Show, Eq)

-- | The one tree id this slice accepts. A @data/tutorials/@ file
--   declaring any other id is a load-time error — that is what keeps
--   \"exactly one active tree\" enforceable instead of letting extra
--   trees load silently inert.
activeTutorialTreeId ∷ Text
activeTutorialTreeId = "first_session"

emptyTutorialRegistry ∷ TutorialRegistry
emptyTutorialRegistry = TutorialRegistry Nothing

-- | The registry holding exactly the given tree. Loading a tree
--   REPLACES whatever was there — one active tree, always.
singleTutorialRegistry ∷ TutorialTree → TutorialRegistry
singleTutorialRegistry = TutorialRegistry ∘ Just

activeTutorialTree ∷ TutorialRegistry → Maybe TutorialTree
activeTutorialTree = trTree

-- | Depth-first flatten in display order (children before
--   subobjectives, each group already ordered). Every objective
--   appears exactly once — the validator guarantees a single parent
--   per node.
tutorialNodeList ∷ TutorialNode → [TutorialNode]
tutorialNodeList n =
  n : concatMap tutorialNodeList (tnChildren n ⧺ tnSubobjectives n)
