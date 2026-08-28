-- | Shared fixture helper for the checked location-geometry
--   construction (#1796).
--
--   'Location.Instance.buildLocationInstances' and its siblings return
--   @'Either' 'LocationGeometryError'@ so an unrepresentable placement
--   is refused before any instance exists. The overwhelming majority of
--   this suite's fixtures place ordinary chunks a handful of tiles from
--   the origin, where that construction cannot fail — and threading an
--   'Either' through every one of those top-level bindings would bury
--   the cases that DO exercise failure (which use the constructors
--   directly, and assert on the 'Left').
--
--   So: fixtures unwrap through here, which fails the example loudly
--   and with full attribution if a fixture ever stops being
--   representable. Never use it to assert success — a test proving the
--   checked path accepts something must pattern-match the 'Either'
--   itself.
module Test.Headless.Location.Fixture ( expectGeometry ) where

import UPrelude
import qualified Data.Text as T
import Location.Instance (LocationGeometryError, locationGeometryErrorText)

expectGeometry ∷ Either LocationGeometryError α → α
expectGeometry = either (error ∘ T.unpack ∘ render) id
  where
    render err = "location fixture geometry is not representable: "
                    <> locationGeometryErrorText err
