-- | The "save components" gate (issue #760, save-overhaul B2): the
--   Haskell-owned persistence component split that replaced B1's single
--   transitional @"session"@ payload. Pure — no engine, no IO. Every
--   'SessionSnapshot' below is a synthetic literal, the same pattern
--   'Test.Headless.Save.Snapshot' uses one layer up.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save components"'@.
--
--   Split by persistence owner under #2043: the test bodies live in
--   'Test.Headless.World.Save.Components.Registry',
--   'Test.Headless.World.Save.Components.Compatibility',
--   'Test.Headless.World.Save.Components.Assembly' and
--   'Test.Headless.World.Save.Components.Integrity', over the shared
--   fixtures in 'Test.Headless.World.Save.Components.Fixture'. This
--   module composes them and is the only one @test-headless/Spec.hs@
--   registers, so the gate's name and command are unchanged.
module Test.Headless.World.Save.Components (spec) where

import Test.Hspec
import qualified Test.Headless.World.Save.Components.Registry as Registry
import qualified Test.Headless.World.Save.Components.Compatibility as Compatibility
import qualified Test.Headless.World.Save.Components.Assembly as Assembly
import qualified Test.Headless.World.Save.Components.Integrity as Integrity

spec ∷ Spec
spec = do
    Registry.spec
    Compatibility.spec
    Assembly.spec
    Integrity.spec
