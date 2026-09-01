{-# LANGUAGE Strict #-}
-- | LOAD reconciliation for structure construction designations (#1844
--   requirements 19–21).
--
--   A save records what a designation WAS, not whether it still makes
--   sense. Between the save and the load, a structure pack's art can go
--   away, its @build:@ costs can change or disappear, and the item
--   definitions a receipt names can be removed. Publishing a session
--   with such a designation in it would put a job on the map that no
--   worker can ever finish and that the player cannot tell apart from a
--   real one.
--
--   So every saved structure designation is reconciled HERE, in staging,
--   against the currently registered content — before anything is
--   visible or actionable — and the outcomes are deliberately not all
--   the same:
--
--     * MISSING ART or MISSING BUILD METADATA self-clears the
--       designation, its progress slope and its saved job, refunding its
--       persisted receipt exactly once. The load succeeds: a receipt IS
--       a lossless refund, so there is nothing to lose by publishing
--       without the job.
--     * A LEGACY PAID designation (a pre-#1844 payload's bare
--       @cdMaterialsPaid = True@, which records that materials left an
--       inventory but not WHICH) has its receipt reconstructed from the
--       currently registered build metadata. If that metadata is gone
--       the whole load is REJECTED, because the alternatives are
--       inventing a refund and silently losing the player's materials.
--     * A receipt naming an item definition that no longer exists also
--       REJECTS the load, for the same reason: the refund it promises
--       cannot be paid.
--     * UNLOADED TERRAIN is not a verdict. The designation is retained
--       as non-actionable and its terrain-dependent checks — including
--       requirement 4's @cdZ@ surface comparison — run when its chunk
--       publishes, through the ordinary
--       "World.Construct.Revalidate" hook. That is the common case: a
--       load publishes with almost nothing resident.
--
--   Refunds land in the STAGED page's own ground items, not the live
--   session's. A load stages a replacement session and swaps it in one
--   quiesced window, so anything deposited through a live-session verb
--   here would be deposited into the session being REPLACED and lost at
--   publication.
module World.Construct.Reconcile
    ( ConstructReconcileError(..)
    , reconcileStagedConstructDesignations
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import Item.Types (ItemManager(..))
import Structure.ArtCatalog
    (BuildCost(..), StructureArtCatalog, packKindBuild)
import World.Construct.Plan
    (PlanOutcome(..), PlanResult(..), PlanOp(..), resolveStructurePlan)
import World.Construct.Receipt
    (ConstructPayment(..), mkMaterialReceipt, receiptEntries)
import World.Construct.Revalidate
    ( ConstructRefundDeps(..), clearConstructDesignationSlope
    , constructPlanWorldWith, refundConstructDesignation )
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..) )
import World.Types

-- | Why a load cannot be published. Rendered by the caller into its own
--   staging error, so this module owns the REASON and not the wrapper.
newtype ConstructReconcileError = ConstructReconcileError Text
    deriving (Eq, Show)

-- | Reconcile one staged page's construction designations in place.
--
--   Runs against the STAGED page's own refs, which is why it takes a
--   'WorldState' rather than a page snapshot: a self-clear has to reset
--   the progress slope on the staged tiles and deposit the refund into
--   the staged ground items, and both of those are exactly what the
--   live path does — through the same two helpers.
reconcileStagedConstructDesignations
    ∷ ConstructRefundDeps → StructureArtCatalog → LoggerState → WorldState
    → IO (Either ConstructReconcileError ())
reconcileStagedConstructDesignations deps cat logger ws = do
    designations ← readIORef (wsConstructDesignationsRef ws)
    if HM.null designations then pure (Right ()) else do
        pw ← constructPlanWorldWith cat ws
        let im = crdItems deps
            -- Deterministic order: a rejection must name the same
            -- designation on every run, and a refund must deposit its
            -- ground items in the same order every time.
            entries = L.sortOn fst (HM.toList designations)

            -- Step 1: settle each designation's PAYMENT record, which is
            -- the only step that can refuse the whole load.
            settle (key, cd) = case cdTarget cd of
                CtBuilding _ → Right (key, cd)
                CtStructure piece → case cdPayment cd of
                    CpUnpaid → Right (key, cd)
                    CpPaid r → do
                        checkReceipt key r
                        Right (key, cd)
                    CpLegacyPaid → case packKindBuild cat (spPack piece)
                                                         (spKind piece) of
                        Nothing → Left $ ConstructReconcileError $
                            "construction designation at " <> tileText key
                            <> " was paid by a pre-receipt save, and '"
                            <> spPack piece <> "/" <> spKind piece
                            <> "' no longer has build metadata to "
                            <> "reconstruct its refund from"
                        Just cost → do
                            let r = mkMaterialReceipt (bcMaterials cost)
                            checkReceipt key r
                            Right (key, cd { cdPayment = CpPaid r })

            checkReceipt key r = case missingMaterials r of
                [] → Right ()
                ms → Left $ ConstructReconcileError $
                    "construction designation at " <> tileText key
                    <> " holds a material receipt naming item "
                    <> "definitions that no longer exist: "
                    <> T.intercalate ", " ms

            missingMaterials r =
                [ name | (name, _) ← receiptEntries r
                       , not (HM.member name (imDefs im)) ]

        case mapM settle entries of
            Left err → pure (Left err)
            Right settled → do
                -- Step 2: a restored designation has no live claimant —
                -- #1329 empties the claim registry on EVERY load — so a
                -- 'CsPlacing' state cannot be completed by anyone.
                -- Leaving it would also make the designation permanently
                -- invisible to revalidation, which skips that state by
                -- design. Demote to pending; a 'CsClaimed' one is left
                -- alone, because the AI's own stale-claim sweep already
                -- adopts and releases those.
                let detached =
                        [ (k, if cdStatus cd ≡ CsPlacing
                                then cd { cdStatus = CsPending } else cd)
                        | (k, cd) ← settled ]
                    -- Step 3: the catalogue verdict. Terrain-dependent
                    -- outcomes are NOT acted on here — almost nothing is
                    -- resident at staging time, so 'PlanUnresolvedTerrain'
                    -- is the normal answer and the chunk-publication hook
                    -- is what resolves it later.
                    verdict (k, cd) = case cdTarget cd of
                        CtBuilding _ → Nothing
                        CtStructure piece →
                            let r = resolveStructurePlan pw
                                        (PlanForAttempt (cdAttempt cd))
                                        (cdZ cd) piece k
                            in if prOutcome r ≡ PlanMissingArt
                                   ∨ catalogueRefusal piece
                                 then Just (k, cd, prReason r)
                                 else Nothing
                    -- Build metadata is a catalogue fact, so it is
                    -- checked directly rather than read off an outcome
                    -- that terrain could have short-circuited first.
                    catalogueRefusal piece = isNothing
                        (packKindBuild cat (spPack piece) (spKind piece))
                    cleared = [ x | Just x ← map verdict detached ]
                    clearedKeys = HS.fromList [ k | (k, _, _) ← cleared ]
                writeIORef (wsConstructDesignationsRef ws) $ HM.fromList
                    [ (k, cd) | (k, cd) ← detached
                              , not (k `HS.member` clearedKeys) ]
                forM_ cleared $ \(key, cd, reason) → do
                    clearConstructDesignationSlope ws key cd
                    refundConstructDesignation deps ws key cd
                    logWarn logger CatWorld $
                        "Load: construction designation at " <> tileText key
                        <> " self-cleared (" <> reason <> ")"
                pure (Right ())
  where
    tileText (gx, gy) = "(" <> tshow gx <> "," <> tshow gy <> ")"
