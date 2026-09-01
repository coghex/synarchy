-- | Generated-language rendering (#710): deterministic profile
--   generation, concept-root assignment/collision resolution, and
--   native-name rendering over #709's semantic proper names. Mirrors
--   'Test.Headless.Language.Semantic''s shape — the production concept
--   catalogue read straight from @data/language/concepts.yaml@, pinned
--   golden outputs, and no engine/Lua/random state anywhere.
--
--   The contracts themselves live in one owner per generator version
--   (#2067); this module is the façade, and holds no version-specific
--   test body of its own. It performs the ONE catalogue load and
--   threads the resulting 'Ctx' into every owner as an ordinary
--   argument, so each expensive canonical sample is built once and
--   stays lazy.
--
--   The composition below IS the suite's execution order, and the two
--   golden blocks are why 'Golden' exports two specs rather than one:
--   the historical pins sit immediately after version 4's contracts,
--   and version 5's own pins after the #1100 orthography and font
--   groups.
module Test.Headless.Language.Generated (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Language.Generated.Support (loadProductionCatalogue)
import qualified Test.Headless.Language.Generated.Core as Core
import qualified Test.Headless.Language.Generated.Onset as Onset
import qualified Test.Headless.Language.Generated.Boundary as Boundary
import qualified Test.Headless.Language.Generated.Bound as Bound
import qualified Test.Headless.Language.Generated.Golden as Golden
import qualified Test.Headless.Language.Generated.Orthography as Orthography

spec ∷ Spec
spec = describe "Generated language names" $ do
    ctx ← runIO loadProductionCatalogue
    Core.spec ctx               -- #710:  version 1 and the core contracts
    Onset.spec ctx              -- #1094: version 2's onset relation
    Boundary.spec ctx           -- #1095: version 3's boundary phonology
    Bound.spec ctx              -- #1096: version 4's bound morphemes
    Golden.historicalSpec ctx   -- pinned versions 1-4
    Orthography.spec ctx        -- #1100: version 5's extended orthography
    Golden.currentSpec ctx      -- pinned version 5
