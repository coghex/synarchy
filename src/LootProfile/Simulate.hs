{-# LANGUAGE Strict #-}
-- | The loot-profile distribution SIMULATION (#2502, epic #1231
--   PLC-13; design D-21).
--
--   PLC-10 has to tune a shipped profile against a shipped crate, and
--   "spawn a few and look" is not a measurement. This module answers
--   the four questions that tuning actually asks — how often the crate
--   comes up empty, how full it ends up, how often capacity is what
--   stopped it, and which items lose lots to capacity — over @N@
--   samples of the REAL realization.
--
--   __Same behavior as production, local effects only.__ Each sample
--   mints its shell through 'materializeItem' and realizes it through
--   'realizeLootProfile', exactly as a placed crate would. What differs
--   is only where the ids come from: every allocator here is a local
--   counter, so a simulation advances neither the engine's instance-id
--   counter nor any shared generator, and two runs of the same
--   arguments against the same world seed report the same numbers.
--
--   __Sample @i@ is context @(worldSeed, i, 0)@__, one-based
--   (requirement 8). Sampling the instance-id axis is what makes the
--   report a distribution rather than one crate measured @N@ times:
--   the realization is a deterministic function of its context, so
--   holding the context fixed would answer the same crate @N@ times.
module LootProfile.Simulate
    ( SimSummary(..)
    , simulateLootProfile
    , occupancyBin
    , histogramBins
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (newIORef)
import Data.List (sort)
import Engine.Core.Log (LoggerState)
import Item.Materialize (materializeItem, pristineItem)
import Item.Types
    ( ItemDef(..), ItemInstance(..), ItemManager, ItemStorage(..)
    , itemTotalWeight, lookupItemDef )
import LootProfile.Realize
import LootProfile.Types (LootProfileDef)

-- | How many occupancy bins a histogram has: @[0,10)@ … @[90,100]@.
histogramBins ∷ Int
histogramBins = 10

-- | One simulation's report. Counts rather than fractions, because a
--   count is exact and a fixed-vector test compares it without a
--   tolerance; the Lua surface divides by 'ssSamples' where the design
--   asks for a fraction.
data SimSummary = SimSummary
    { ssSamples        ∷ !Int
      -- ^ @N@, the denominator of every measure below.
    , ssNaturallyEmpty ∷ !Int
      -- ^ Samples that realized SUCCESSFULLY and admitted no generated
      --   lot. Authored shell contents do not make a sample non-empty:
      --   the question is whether the profile produced cargo.
    , ssSaturated      ∷ !Int
      -- ^ Samples that admitted at least one lot AND rejected at least
      --   one lot which — using that rejected lot's own already
      --   materialized candidate, never a reroll — would have fitted an
      --   otherwise empty copy of the same shell. That is the design's
      --   definition: a lot too big for the empty crate is not evidence
      --   that the crate filled up.
    , ssWeightBins     ∷ ![Int]
      -- ^ Occupancy of the shell's internal WEIGHT capacity after
      --   realization, in ten percentage bins, over all 'ssSamples'
      --   samples — refused ones included, where "after realization"
      --   is the unchanged shell.
    , ssBulkBins       ∷ ![Int]
      -- ^ The same for internal BULK capacity: direct children's own
      --   external bulk, charged at the immediate parent only (D-5).
    , ssRejectedByItem ∷ ![(Text, Int)]
      -- ^ Rejected lots grouped by item definition over all samples,
      --   ascending by item name so two runs print the same list.
    , ssRefused        ∷ !Int
      -- ^ Samples whose realization REFUSED (design D-23). Zero for
      --   every registered profile — the loader resolves every entry's
      --   item and the container was checked for @storage:@ before the
      --   first sample — and reported anyway, because it is the only
      --   thing that makes 'ssNaturallyEmpty' readable as a fraction of
      --   successful samples.
    } deriving (Show, Eq)

-- | Simulate @N@ realizations of one profile into one container
--   definition.
--
--   'Nothing' — the @nil@ the Lua verb answers — for a non-positive
--   sample count, an item definition that is not registered, and one
--   that declares no @storage:@. Those three are caller mistakes, not
--   distributions with zero samples.
simulateLootProfile
    ∷ ItemManager
    → LoggerState
    → Int            -- ^ the world page's generation seed
    → LootProfileDef
    → Text           -- ^ the container item definition
    → Int            -- ^ @N@
    → IO (Maybe SimSummary)
simulateLootProfile itemMgr logger worldSeed profile container sampleCount
    | sampleCount ≤ 0 = pure Nothing
    | otherwise = case idStorage =≪ lookupItemDef container itemMgr of
        Nothing      → pure Nothing
        Just storage → do
            samples ← mapM (sample storage) [1 .. sampleCount]
            pure (Just (summarize sampleCount samples))
  where
    sample storage i = do
        let ctx = RealizeContext { rcWorldSeed  = worldSeed
                                 , rcInstanceId = i
                                 , rcSlot       = 0 }
        -- One local allocator per sample, feeding BOTH the shell mint
        -- and the realization's commits. It is monotonic, so no
        -- committed lot can collide with the shell's authored contents.
        alloc  ← localAllocator 0
        rngRef ← newIORef (realizeGen ctx StreamShell [])
        mShell ← materializeItem itemMgr logger rngRef alloc pristineItem
                                 container
        case mShell of
            -- Unreachable: the definition resolved above.
            Nothing    → pure Nothing
            Just shell → Just <$> measure storage ctx shell alloc

    measure storage ctx shell alloc = do
        outcome ← realizeLootProfile itemMgr logger alloc ctx profile shell
        let (final, mReport) = case outcome of
                RealizeRefused _        → (shell, Nothing)
                RealizeDone shell' rep  → (shell', Just rep)
            rejected  = maybe [] reportRejected mReport
            admitted  = maybe [] reportAdmitted mReport
            emptyCopy = shell { iiContents = [] }
        pure SampleMeasure
            { smWeightBin = occupancyBin (isWeightCapacity storage)
                                (sum (map (itemTotalWeight itemMgr)
                                          (iiContents final)))
            , smBulkBin   = occupancyBin (isBulkCapacity storage)
                                (sum (map (fromMaybe 0 ∘ iiBulk)
                                          (iiContents final)))
            , smEmpty     = isJust mReport ∧ null admitted
            , smSaturated = not (null admitted)
                              ∧ any (fitsEmpty emptyCopy) rejected
            , smRejected  = map lrItem rejected
            , smRefused   = isNothing mReport
            }

    -- Saturation eligibility, from the SAME candidate values that were
    -- already rolled: a lot that an empty crate would have taken was
    -- lost to what the crate already held, which is the only thing
    -- "saturated" is allowed to mean.
    fitsEmpty emptyCopy lot =
        case admitCandidates itemMgr emptyCopy (lrCandidates lot) of
            Right _ → True
            Left _  → False

-- | What one sample contributes, kept separate from the aggregate so
--   the folding below reads as arithmetic rather than as a second
--   measurement.
data SampleMeasure = SampleMeasure
    { smWeightBin ∷ !Int
    , smBulkBin   ∷ !Int
    , smEmpty     ∷ !Bool
    , smSaturated ∷ !Bool
    , smRejected  ∷ ![Text]
    , smRefused   ∷ !Bool
    }

summarize ∷ Int → [Maybe SampleMeasure] → SimSummary
summarize n mSamples = SimSummary
    { ssSamples        = n
    , ssNaturallyEmpty = count smEmpty
    , ssSaturated      = count smSaturated
    , ssWeightBins     = bins smWeightBin
    , ssBulkBins       = bins smBulkBin
    , ssRejectedByItem = sort (HM.toList tally)
    , ssRefused        = count smRefused
    }
  where
    samples   = catMaybes mSamples
    count f   = length [ () | s ← samples, f s ]
    bins f    = [ length [ () | s ← samples, f s ≡ b ]
                | b ← [0 .. histogramBins - 1] ]
    tally     = foldl' bump HM.empty (concatMap smRejected samples)
    bump acc item = HM.insertWith (+) item (1 ∷ Int) acc

-- | Which of the ten bins an occupancy falls in: @[0,10)@, @[10,20)@,
--   …, @[90,100]@.
--
--   The top bin is CLOSED and also catches an occupancy above 100%,
--   which is reachable only from authored default contents that already
--   exceed the capacity — PLC-4 never admits past it. A non-positive
--   capacity cannot occur (the @storage:@ parser rejects one) and is
--   read here as "no room at all".
occupancyBin ∷ Float → Float → Int
occupancyBin capacity used
    | used ≤ 0     = 0
    | capacity ≤ 0 = histogramBins - 1
    | otherwise    = min (histogramBins - 1)
                         (max 0 (floor (fromIntegral histogramBins
                                        * used / capacity)))
