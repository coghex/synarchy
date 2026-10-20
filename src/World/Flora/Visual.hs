-- | Pure semantic texture selection. Contract: flora_visual_state_contract.md.
module World.Flora.Visual
    ( FloraVisualSelector
    , floraVisualSelector
    , visualContext, visualPhase, visualStage, visualCondition, visualCause
    , FloraTextureCandidate(..)
    , FloraTextureResolution(..)
    , resolveFloraVisual
    ) where

import UPrelude
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle)
import World.Flora.Types

-- | A request, distinct from an authored wildcard pattern. Construction keeps
-- legacy PhaseDead out of the phase axis and removes causes from living state.
data FloraVisualSelector = FloraVisualSelector
    !FloraContext !(Maybe LifePhaseTag) !(Maybe AnnualStageTag)
    !FloraCondition !(Maybe FloraDeathCause)
    deriving (Show, Eq)

visualContext ∷ FloraVisualSelector → FloraContext
visualContext (FloraVisualSelector ctx _ _ _ _) = ctx

visualPhase ∷ FloraVisualSelector → Maybe LifePhaseTag
visualPhase (FloraVisualSelector _ phase _ _ _) = phase

visualStage ∷ FloraVisualSelector → Maybe AnnualStageTag
visualStage (FloraVisualSelector _ _ stage _ _) = stage

visualCondition ∷ FloraVisualSelector → FloraCondition
visualCondition (FloraVisualSelector _ _ _ condition _) = condition

visualCause ∷ FloraVisualSelector → Maybe FloraDeathCause
visualCause (FloraVisualSelector _ _ _ _ cause) = cause

floraVisualSelector ∷ FloraContext → Maybe LifePhaseTag → Maybe AnnualStageTag
                    → FloraCondition → Maybe FloraDeathCause → FloraVisualSelector
floraVisualSelector ctx phase stage condition cause = FloraVisualSelector
    ctx (phase ⌦ \p → if p ≡ PhaseDead then Nothing else Just p) stage condition
    (if condition ≡ ConditionDead then cause else Nothing)

data FloraTextureCandidate
    = VariantCandidate !FloraVariantSelector
    | LegacyOverride !AnnualCycleKey
    | LegacyStage !AnnualStageTag
    | LegacyPhase !LifePhaseTag
    | SpeciesBase
    deriving (Show, Eq)

data FloraTextureResolution = FloraTextureResolution
    { ftrTexture ∷ !TextureHandle
    , ftrWinner  ∷ !FloraTextureCandidate
    , ftrTried   ∷ ![FloraTextureCandidate]
      -- ^ Ordered attempts, including misses and the winner as the last entry.
    } deriving (Show, Eq)

-- | Finite, deterministic and total, with the declared base as the final
-- candidate. No IO or global state. A handle of zero is still an authored
-- handle; suppression belongs to the render caller, not this resolver.
resolveFloraVisual ∷ FloraSpecies → FloraVisualSelector → FloraTextureResolution
resolveFloraVisual sp request = walk [] (variants ⧺ legacy)
  where
    table = HM.union (fsTextureVariants sp) normalizedDead
    normalizedDead = HM.fromList $
        [ (wildcardVariantSelector { fvsCondition = Just ConditionDead }
          , lpTexture phase)
        | Just phase ← [HM.lookup PhaseDead (fsPhases sp)] ]
        ⧺ [ (wildcardVariantSelector { fvsCondition = Just ConditionDead
                                     , fvsStage = Just stage }, tex)
          | (AnnualCycleKey PhaseDead stage, tex) ← HM.toList (fsCycleOverrides sp) ]
    -- Descending (condition, phase, cause, stage) specificity, with context
    -- attempts INSIDE each semantic key. The context-less pattern already
    -- failed in the cultivated attempt, so it need not be repeated in wild.
    contexts = case visualContext request of
        ContextWild       → [Just ContextWild, Nothing]
        ContextCultivated → [Just ContextCultivated, Nothing, Just ContextWild]
    choices Nothing = [Nothing]
    choices value = [value, Nothing]
    patterns =
        [ FloraVariantSelector ctx phase stage condition cause
        | condition ← choices (Just (visualCondition request))
        , phase ← choices (visualPhase request)
        , cause ← choices (visualCause request)
        , isNothing cause ∨ condition ≡ Just ConditionDead
        , stage ← choices (visualStage request)
        , ctx ← contexts ]
    variants = [(VariantCandidate key, HM.lookup key table) | key ← patterns]
    -- Legacy living declarations are a compatibility unit, AFTER all variant
    -- candidates. Their annual-stage-above-phase order is intentionally not
    -- the variant specificity order.
    legacy =
        [ (LegacyOverride key, HM.lookup key (fsCycleOverrides sp))
        | Just phase ← [visualPhase request], Just stage ← [visualStage request]
        , let key = AnnualCycleKey phase stage ]
        ⧺ [ (LegacyStage tag, asTexture <$> find ((≡ tag) . asTag) (fsAnnualCycle sp))
          | Just tag ← [visualStage request] ]
        ⧺ [ (LegacyPhase tag, lpTexture <$> HM.lookup tag (fsPhases sp))
          | Just tag ← [visualPhase request] ]
    walk tried [] = FloraTextureResolution (fsBaseTexture sp) SpeciesBase
                       (reverse (SpeciesBase : tried))
    walk tried ((candidate, Nothing):rest) = walk (candidate : tried) rest
    walk tried ((candidate, Just texture):_) =
        FloraTextureResolution texture candidate (reverse (candidate : tried))
