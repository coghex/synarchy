-- | The one deterministic page ordering every page-scoped save component
--   encodes its slices in (issue #2150).
--
--   A component's payload is a LIST of per-page slices, so the order that
--   list is built in is part of the bytes: two encodes of the same session
--   must produce the same file, and the compatibility fixtures pin exactly
--   that. 'orderedPages' is that order — ascending 'WorldPageId', taken
--   from the snapshot's unordered page map, which has no order of its own.
--
--   It lives here, OUTSIDE @src\/World\/Save\/Component\/@, deliberately.
--   Three @make ci@ gates glob that directory as their component-source
--   discovery set — @tools\/save_compat_audit_common.py@,
--   @tools\/enum_append_only_audit_model.py@ and
--   @tools\/persistence_inventory_audit.py@ — and every file they find is
--   read as a component owner. A helper that declares no DTO and no codec
--   is not one, so it is kept out of their read set rather than added to
--   an exclusion list in each.
--
--   This is the narrow dependency-neutral owner requirement 6 of #2150
--   asks for while #2135 has not yet established a canonical page-scoped
--   ordering helper: when it does, this module's body becomes a re-export
--   of that one and its consumers do not move again. Until then the
--   entity owners ("World.Save.Component.EntitySnapshots",
--   ".EntitySimulation", ".EntitySystems") share this single definition
--   instead of each carrying a copy of its @sortOn pgsPageId@.
module World.Save.PageOrder
    ( orderedPages
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))

orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages
