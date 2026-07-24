{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Central unit-domain types. #575 split this file's declarations out
--   into sibling modules so this stays a thin re-export facade with an
--   unchanged public API:
--
--     * Wound / Scar records live in 'Unit.Types.Wound'.
--     * Combat body/weapon records live in 'Unit.Types.Combat'.
--     * The unit template (Animation / StatModifier / NamePool /
--       UnitDef) lives in 'Unit.Types.Def'.
--     * The spawned unit instance lives in 'Unit.Types.Instance'.
--     * Unit identity and the top-level registry live in
--       'Unit.Types.Manager'.
--
--   All are re-exported here so every existing import site keeps
--   working unchanged.
module Unit.Types
    ( Animation(..)
    , StatModifier(..)
    , Wound(..)
    , woundEffSeverity
    , bloodMassRatio
    , Scar(..)
    , BodyPart(..)
    , NaturalWeapon(..)
    , StrikeProfile(..)
    , NaturalResistance(..)
    , defaultNaturalResistance
    , NamePool(..)
    , UnitDef(..)
    , UnitInstance(..)
    , UnitManager(..)
    , UnitId(..)
    , TrailState(..)
    , emptyTrailState
    , emptyUnitManager
    , nextUnitId
    , unitsOnPages
    , unitsOnPage
    ) where

import Unit.Types.Wound
import Unit.Types.Combat
import Unit.Types.Def
import Unit.Types.Instance
import Unit.Types.Manager
import Unit.Types.Trail
