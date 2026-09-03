{-# LANGUAGE ScopedTypeVariables #-}
-- | The autosave rotation's DURABILITY boundary (issue #2229).
--
--   Every move a cycle makes — 'World.Save.Autosave.clearRetired''s
--   removal, and the retire/shift/promote renames plus the single
--   removal inside @performRotation@ — changes @saves\/@'s own entry
--   list. Before #2229 none of them synced it, so a rotation reported as
--   complete could be wholly absent after a crash even though the
--   generations themselves were durable.
--
--   These cases pin WHICH points sync and WHAT a failure at each one
--   leaves behind, through the
--   'World.Save.Autosave.finalizeAutosaveRotationWithSync' /
--   'World.Save.Autosave.prepareAutosaveCycleWithSync' seams. Fault
--   injection rather than a real @fsync@ failure: a directory sync does
--   not fail on demand on a healthy filesystem, and what is under test
--   is this module's ordering and reporting, never the kernel's.
--
--   No engine: every fixture is a REAL published generation on real
--   disk (rotation renames and removes directories, so it cannot stay
--   in memory), written by the same transaction production uses. The
--   module reads @saves\/@ through the bare relative path
--   'World.Save.Serialize.savesDirectory', so each case runs chdir'd
--   into its own scratch root — the same sequential-suite reasoning
--   'Test.Headless.World.Save.Storage' documents for @withSavesRoot@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "autosave rotation durability"'@.
module Test.Headless.Save.AutosaveRotation (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Exception (finally)
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesDirectoryExist, withCurrentDirectory )
import System.FilePath ((</>))

import Engine.Core.Log
    (initLogger, defaultLogConfig, LogConfig(..), LogBackend(..), LoggerState)
import World.Save.Autosave
    ( autosaveIncomingSlotName, autosaveRetiredSlotName, autosaveSlotName
    , finalizeAutosaveRotationWithSync, prepareAutosaveCycleWithSync )
import World.Save.Serialize
    (listSaves, savesDirectory, SaveListing(..))
import World.Save.Storage (publishGeneration, renderPublishFailure)
import World.Save.Envelope (encodeSessionSnapshot)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types (SaveMetadata(..), BuildingSnapshot(..), UnitSnapshot(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Edit.Types (emptyWorldEdits)
import Craft.Bills (emptyCraftBills)
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types (emptyPowerNodes)
import Building.Knowledge (emptyContainerKnowledge)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Flora.Identity (firstPlantedFloraCursor)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

-- ---------------------------------------------------------------------
-- Fixture (the minimal* pattern every save gate in this suite uses)
-- ---------------------------------------------------------------------

page1 ∷ WorldPageId
page1 = WorldPageId "page1"

minimalPage ∷ PageSnapshot
minimalPage = PageSnapshot
    { pgsPageId       = page1
    , pgsGenParams    = defaultWorldGenParams { wgpSeed = 7 }
    , pgsCameraX      = 0
    , pgsCameraY      = 0
    , pgsTimeHour     = 12
    , pgsTimeMinute   = 0
    , pgsDateYear     = 1
    , pgsDateMonth    = 1
    , pgsDateDay      = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsConstructNextAttempt  = firstConstructAttemptId
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 1 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 1 }
    , pgsUnitSimStates = HM.empty
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.empty
    , pgsPendingChopMigration = HM.empty
    , pgsPendingFloraHarvests = HM.empty
    , pgsPlantedFloraCursor = firstPlantedFloraCursor
    , pgsCraftBills   = emptyCraftBills
    , pgsTransferOrders = emptyTransferOrders
    , pgsPowerNodes   = emptyPowerNodes
    , pgsTillDesignations = HM.empty
    , pgsCropPlots    = HM.empty
    , pgsPlantDesignations = HM.empty
    , pgsContainerKnowledge = emptyContainerKnowledge
    , pgsIdentity     = Nothing
    , pgsGeneratedId  = Just (fixtureGeneratedWorldIdForPage page1)
    }

minimalGlobals ∷ SessionGlobals
minimalGlobals = SessionGlobals
    { sgGameTime       = 0
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 1
    , sgNextUnitId     = 1
    , sgActivePage     = page1
    , sgVisiblePages   = [page1]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just page1
        , lcsX = 10, lcsY = 20, lcsZoom = 2, lcsFacing = FaceSouth
        }
    }

-- | Publish a REAL autosave generation into @saves\/\<slot\>@. The
--   timestamp is the fixture's only distinguishing feature, so a case
--   below can say exactly WHICH generation ended up in a given slot
--   after a rotation — the whole point of the ordering assertions.
--
--   'SaveRequestMeta''s third field is the durable
--   'World.Save.Types.smAutosave' classification, and rotation refuses
--   over any slot in range that is not classified an autosave, so it
--   must be 'True' for every fixture here.
publishAutosave ∷ Text → Text → IO ()
publishAutosave slot ts = do
    let snap = case captureSessionSnapshot minimalGlobals [minimalPage] of
            Right s   → s
            Left errs → error ("publishAutosave: invalid fixture: " <> show errs)
        meta  = snapshotSaveMetadata (SaveRequestMeta slot ts True) snap
        bytes = encodeSessionSnapshot meta snap []
        dir   = savesDirectory </> T.unpack slot
    r ← publishGeneration dir slot meta bytes HS.empty HS.empty
    case r of
        Right _ → pure ()
        Left f  → expectationFailure
            ("autosave fixture: " <> T.unpack (renderPublishFailure f))

-- ---------------------------------------------------------------------
-- Scratch root and the injected sync
-- ---------------------------------------------------------------------

-- | An isolated resource root, chdir'd into and wiped clean before and
--   after use. "World.Save.Autosave" resolves every slot through the
--   bare relative 'savesDirectory', so this is the only way to exercise
--   it against real directories without touching the repository's own
--   @saves\/@.
withAutosaveRoot ∷ IO a → IO a
withAutosaveRoot action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-autosave-rotation-spec"
    reset root
    createDirectoryIfMissing True (root </> savesDirectory)
    withCurrentDirectory root action `finally` reset root
  where
    reset root = do
        isDir ← doesDirectoryExist root
        when isDir (removeDirectoryRecursive root)

-- | Records every directory the cycle syncs, in call order, and throws
--   on the @failAt@'th call (1-based; 0 never fails).
countingSync ∷ IORef [FilePath] → Int → FilePath → IO ()
countingSync seen failAt path = do
    modifyIORef' seen (⧺ [path])
    calls ← readIORef seen
    when (length calls ≡ failAt) $
        ioError (userError "injected directory-sync failure")

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

slotPath ∷ Text → FilePath
slotPath name = savesDirectory </> T.unpack name

-- | Which slot each generation ended up in, keyed by the timestamp
--   'publishAutosave' stamped it with.
slotTimestamps ∷ LoggerState → IO [(Text, Text)]
slotTimestamps logger = do
    listings ← listSaves logger HS.empty
    pure [ (slName l, smTimestamp (slMetadata l)) | l ← listings ]

-- | A FULL two-deep family plus a freshly staged generation — the shape
--   that actually retires something.
fullFamily ∷ IO ()
fullFamily = do
    publishAutosave (autosaveSlotName 1) "newer"
    publishAutosave (autosaveSlotName 2) "older"
    publishAutosave autosaveIncomingSlotName "staged"

spec ∷ Spec
spec = do
    describe "saves/ is synced at every point the family shape changes" $ do
        it "syncs saves/ after the renames and AGAIN after the retired \
           \generation is removed" $
            withAutosaveRoot $ do
                logger ← quietLogger
                fullFamily
                seen ← newIORef []
                r ← finalizeAutosaveRotationWithSync
                        (countingSync seen 0) logger HS.empty 2
                r `shouldBe` Right ()
                synced ← readIORef seen
                synced `shouldBe` [savesDirectory, savesDirectory]
                -- ...and the rotation itself is the one #913 specifies.
                stamps ← slotTimestamps logger
                lookup (autosaveSlotName 1) stamps `shouldBe` Just "staged"
                lookup (autosaveSlotName 2) stamps `shouldBe` Just "newer"
                doesDirectoryExist (slotPath autosaveRetiredSlotName)
                    `shouldReturn` False

        it "syncs saves/ exactly ONCE when the family was not full -- \
           \nothing aged out, so there is no removal entry to persist" $
            withAutosaveRoot $ do
                logger ← quietLogger
                publishAutosave (autosaveSlotName 1) "newer"
                publishAutosave autosaveIncomingSlotName "staged"
                seen ← newIORef []
                r ← finalizeAutosaveRotationWithSync
                        (countingSync seen 0) logger HS.empty 3
                r `shouldBe` Right ()
                readIORef seen `shouldReturn` [savesDirectory]
                stamps ← slotTimestamps logger
                lookup (autosaveSlotName 1) stamps `shouldBe` Just "staged"
                lookup (autosaveSlotName 2) stamps `shouldBe` Just "newer"

    describe "a sync failure fails the autosave, naming the directory, \
             \without weakening the resumable-family guarantee" $ do
        it "the POST-RENAME sync failing leaves the retired generation \
           \UNDELETED -- the same partially shifted family an \
           \interruption already produces, which the next cycle finishes" $
            withAutosaveRoot $ do
                logger ← quietLogger
                fullFamily
                seen ← newIORef []
                r ← finalizeAutosaveRotationWithSync
                        (countingSync seen 1) logger HS.empty 2
                case r of
                    Left reason → do
                        reason `shouldSatisfy`
                            T.isInfixOf (T.pack savesDirectory)
                        reason `shouldSatisfy`
                            T.isInfixOf "autosave rotation failed"
                    Right () → expectationFailure
                        "expected the post-rename sync failure to fail \
                        \the rotation"
                -- Nothing was destroyed: the aged-out generation is
                -- still on disk under the retired name.
                doesDirectoryExist (slotPath autosaveRetiredSlotName)
                    `shouldReturn` True
                -- ...and only that one sync was ever attempted, so the
                -- removal really did not run.
                readIORef seen `shouldReturn` [savesDirectory]
                stamps ← slotTimestamps logger
                lookup (autosaveRetiredSlotName) stamps `shouldBe` Just "older"

        it "the POST-REMOVAL sync failing leaves the already-synced \
           \numbered family complete and resumable" $
            withAutosaveRoot $ do
                logger ← quietLogger
                fullFamily
                seen ← newIORef []
                r ← finalizeAutosaveRotationWithSync
                        (countingSync seen 2) logger HS.empty 2
                case r of
                    Left reason → reason `shouldSatisfy`
                        T.isInfixOf (T.pack savesDirectory)
                    Right () → expectationFailure
                        "expected the post-removal sync failure to fail \
                        \the rotation"
                readIORef seen `shouldReturn` [savesDirectory, savesDirectory]
                -- The family reached its final shape before that sync,
                -- and step 4's sync already made it durable.
                stamps ← slotTimestamps logger
                lookup (autosaveSlotName 1) stamps `shouldBe` Just "staged"
                lookup (autosaveSlotName 2) stamps `shouldBe` Just "newer"
                doesDirectoryExist (slotPath autosaveIncomingSlotName)
                    `shouldReturn` False

        it "clearRetired syncs saves/ after discarding a leftover \
           \retired generation, and a failure there REFUSES the cycle \
           \naming the directory" $
            withAutosaveRoot $ do
                logger ← quietLogger
                publishAutosave (autosaveSlotName 1) "newer"
                publishAutosave autosaveRetiredSlotName "leftover"
                seen ← newIORef []
                r ← prepareAutosaveCycleWithSync
                        (countingSync seen 1) logger HS.empty 2
                case r of
                    Left reason → do
                        reason `shouldSatisfy`
                            T.isInfixOf (T.pack savesDirectory)
                        reason `shouldSatisfy` T.isInfixOf "autosave refused"
                    Right () → expectationFailure
                        "expected the clearRetired sync failure to refuse \
                        \the cycle"
                readIORef seen `shouldReturn` [savesDirectory]

        it "a clean prepare over a leftover retired generation syncs \
           \saves/ once and discards it" $
            withAutosaveRoot $ do
                logger ← quietLogger
                publishAutosave (autosaveSlotName 1) "newer"
                publishAutosave autosaveRetiredSlotName "leftover"
                seen ← newIORef []
                r ← prepareAutosaveCycleWithSync
                        (countingSync seen 0) logger HS.empty 2
                r `shouldBe` Right ()
                readIORef seen `shouldReturn` [savesDirectory]
                doesDirectoryExist (slotPath autosaveRetiredSlotName)
                    `shouldReturn` False
