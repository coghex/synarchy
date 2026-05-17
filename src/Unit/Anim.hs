{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure helpers for resolving sim state → animation name.
--   The renderer's `pickFrame` does the actual texture lookup; this
--   module only decides which animation key the renderer should ask for.
module Unit.Anim
    ( activityToStateKey
    , resolveStateAnim
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Sim.Types (UnitActivity(..))
import Unit.Types (UnitDef(..))

-- | Map a sim activity to the conventional state key used to look up
--   animations. These keys are the contract between sim and YAML: a
--   unit YAML may map them to any animation name via `state_animations`.
activityToStateKey ∷ UnitActivity → Text
activityToStateKey Idle      = "idle"
activityToStateKey Walking   = "walk"
activityToStateKey Collapsed = "collapsed"
activityToStateKey Reviving  = "reviving"
activityToStateKey Drinking  = "drinking"
activityToStateKey Picking   = "pickup"

-- | Resolve a state key (e.g. "idle") to an animation name (e.g.
--   "breathing-idle") using the unit's `udStateAnims` map. If no
--   mapping exists, return the state key itself — the renderer will
--   look that up in `udAnimations` and fall back to T-pose if it isn't
--   defined there either.
resolveStateAnim ∷ UnitDef → Text → Text
resolveStateAnim def stateKey =
    HM.lookupDefault stateKey stateKey (udStateAnims def)
