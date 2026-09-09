-- | "Portable container knowledge" (#2512, epic #1231 PLC-7): what the
--   player remembers about a PORTABLE container, keyed by the item's
--   own 'Item.Types.iiInstanceId' and carried across pages and owners
--   with it.
--
--   The facade this suite registers. Its five parts each own one layer
--   of the slice, so a failure names the layer rather than a
--   thousand-line module:
--
--     * "Test.Headless.Item.PortableKnowledge.Model" — the four states,
--       the two independently-stamped observations, and what an
--       observation does and does not touch (pure).
--     * "Test.Headless.Item.PortableKnowledge.Locate" — the
--       session-wide live-instance locator, across every container the
--       save system enumerates.
--     * "Test.Headless.Item.PortableKnowledge.Persistence" — the
--       optional @"portable-knowledge"@ component through the real
--       production codec, and the historical-observation rules.
--     * "Test.Headless.Item.PortableKnowledge.Lifecycle" — the live
--       'World.State.Types.wmPortableKnowledge' owner across visibility
--       changes, a real save, a real load, and Exit to Menu.
--     * "Test.Headless.Item.PortableKnowledge.LuaApi" — the four
--       registered @item.*@ verbs, through a real Lua backend.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "Portable container knowledge"'@
module Test.Headless.Item.PortableKnowledge (spec) where

import UPrelude
import Test.Hspec
import qualified Test.Headless.Item.PortableKnowledge.Lifecycle as Lifecycle
import qualified Test.Headless.Item.PortableKnowledge.Locate as Locate
import qualified Test.Headless.Item.PortableKnowledge.LuaApi as LuaApi
import qualified Test.Headless.Item.PortableKnowledge.Model as Model
import qualified Test.Headless.Item.PortableKnowledge.Persistence as Persistence

spec ∷ Spec
spec = describe "Portable container knowledge" $ do
    Model.spec
    Locate.spec
    Persistence.spec
    Lifecycle.spec
    LuaApi.spec
