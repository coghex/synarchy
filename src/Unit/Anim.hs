{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure helpers for resolving sim state → animation name.
--   The renderer's `pickFrame` does the actual texture lookup; this
--   module only decides which animation key the renderer should ask for.
module Unit.Anim
    ( poseStateKey
    , stateKey
    , poseTag
    , resolveStateAnim
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Sim.Types (Pose(..), UnitActivity(..))
import Unit.Types (UnitDef(..))

-- | Lowercase pose name used in composite state keys.
poseTag ∷ Pose → Text
poseTag Standing  = "standing"
poseTag Crouching = "crouching"
poseTag Crawling  = "crawling"
poseTag Collapsed = "collapsed"
poseTag Dead      = "dead"
poseTag Climbing  = "climbing"
poseTag Falling   = "falling"

-- | State key for a pose + activity, used to look up the animation
--   in `udStateAnims`. The convention is `<pose>-<activity>` for
--   in-pose activities, and `<from>-to-<to>` for transitions.
stateKey ∷ Pose → UnitActivity → Text
stateKey p Idle               = poseTag p <> "-idle"
stateKey p Walking            = poseTag p <> "-walk"
stateKey p Running            = poseTag p <> "-run"
stateKey p Drinking           = poseTag p <> "-drink"
stateKey p Eating             = poseTag p <> "-eat"
stateKey p Picking            = poseTag p <> "-pickup"
stateKey p (TransitioningTo q) = poseTag p <> "-to-" <> poseTag q

-- | Convenience: equivalent to `stateKey p Idle`. The idle pose key is
--   used as the default state for a freshly-loaded unit.
poseStateKey ∷ Pose → Text
poseStateKey p = stateKey p Idle

-- | Resolve a state key (e.g. "standing-idle") to an animation name
--   (e.g. "idle-standing") using the unit's `udStateAnims` map. If no
--   mapping exists, return the state key itself — the renderer will
--   look that up in `udAnimations` and fall back to T-pose if it isn't
--   defined there either.
resolveStateAnim ∷ UnitDef → Text → Text
resolveStateAnim def key =
    HM.lookupDefault key key (udStateAnims def)
