#!/usr/bin/env python3
"""The real-codec bridge for the save-compatibility tool (issue #2049,
requirement 7).

A LEAF service (requirement 15): the ONE owner of every GHCi-backed
operation this tool performs, and of the `cabal repl` subprocess
protocol behind them. It imports only the shared definitions owner.

It owns all three GHCi templates and the three operations that run
them:

  - fixed-timestamp normalization (`normalize_fixture_timestamp`);
  - decoded fixture-descriptor dumping (`dump_fixture_descriptors`);
  - canonical-summary dumping (`dump_canonical_summary`).

Everything else CALLS these; nothing else re-implements one
(requirement 16). In particular save_compat_audit_manifest's
`verify_fixture_descriptors` -- which turns a raw descriptor dump into
manifest violations -- is the manifest audit's, not this module's: it
takes a manifest dict and emits violation strings, so it belongs with
the audit that aggregates them, and it reaches the real bytes only
through `dump_fixture_descriptors` here.

Requirement 8: every operation here keeps its existing production path
-- `cabal repl test:synarchy-test-headless`, the 1800-second timeout,
its own success marker (`NORMALIZE_OK` / `DESCRIPTOR_DUMP_OK` /
`DUMP_OK`), a 60-line diagnostic tail on failure, and guaranteed
temporary-file cleanup. These are NOT interchangeable with
save_compat_audit_register's `_run_real_codec_validation`, which is a
different invocation entirely (`cabal test synarchy-test-headless
--test-options=--match "save migrations"`, judged by its return code,
40-line tail) and stays with the registration owner it validates for.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import json
import subprocess
import tempfile
from pathlib import Path

import save_compat_audit_common as common

# A small, permanent GHCi program (run via `cabal repl` subprocess) that
# derives a fixture's canonical-summary JSON DIRECTLY from its real,
# decoded SessionSnapshot/SaveMetadata -- not from live engine queries,
# several of which (hour/minute of day, in particular) have no debug-
# console verb to read at all. Mirrors EXACTLY the schema
# test-headless/Test/Headless/World/Save/Compat/Baselines.hs's ExpectedSummary/
# ExpectedPage/Expected* Aeson types parse -- the two must be kept in
# sync by hand if that schema ever grows a field.
GHCI_DUMP_SUMMARY_TEMPLATE = r"""
:set -XOverloadedStrings -XTypeApplications
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as T
import Data.List (sortOn)
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Snapshot
import World.Save.Types
import World.Page.Types (WorldPageId(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))

bytes <- BS.readFile "{fixture_path}"

:{{
let luaNames = HS.fromList ["unit_ai", "building_spawn"]
    decoded = decodeSessionEnvelope luaNames luaNames bytes
:}}

:{{
case decoded of
  Left err -> putStrLn ("DUMP_FAILED: decode: " ++ T.unpack err)
  Right (meta, snap, luaComponents, isMig) -> do
    let dumpItem i = Aeson.object
          [ "defName" .= iiDefName i, "instanceId" .= iiInstanceId i
          , "currentFill" .= iiCurrentFill i, "quality" .= iiQuality i
          , "condition" .= iiCondition i, "weight" .= iiWeight i
          , "contents" .= map dumpItem (iiContents i) ]
        dumpBuilding (bid, b) = Aeson.object
          [ "id" .= unBuildingId bid, "defName" .= bisDefName b
          , "anchorX" .= bisAnchorX b, "anchorY" .= bisAnchorY b
          , "gridZ" .= bisGridZ b, "buildProgress" .= bisBuildProgress b ]
        dumpUnit (uid, u) = Aeson.object
          [ "id" .= unUnitId uid, "defName" .= uisDefName u
          , "gridX" .= uisGridX u, "gridY" .= uisGridY u
          , "gridZ" .= uisGridZ u, "facing" .= T.pack (show (uisFacing u))
          , "activity" .= uisActivity u, "pose" .= uisPose u
          , "inventory" .= map dumpItem (uisInventory u) ]
        dumpSim (uid, s) = Aeson.object
          [ "unitId" .= unUnitId uid, "realX" .= usRealX s, "realY" .= usRealY s
          , "gridZ" .= usGridZ s, "pose" .= T.pack (show (usPose s))
          , "state" .= T.pack (show (usState s))
          , "facing" .= T.pack (show (usFacing s)) ]
        dumpBill b = Aeson.object
          [ "id" .= unBillId (cbId b), "station" .= unBuildingId (cbStation b)
          , "recipe" .= cbRecipe b, "remaining" .= cbRemaining b
          , "claimant" .= fmap unUnitId (cbClaimant b)
          , "mode" .= T.pack (show (cbMode b)) ]
        dumpNode n = Aeson.object
          [ "id" .= unPowerNodeId (pnId n), "building" .= unBuildingId (pnBuilding n)
          , "role" .= T.pack (show (pnRole n)), "peakWatts" .= pnPeakWatts n
          , "capacityWh" .= pnCapacityWh n, "storedWh" .= pnStoredWh n ]
        dumpPage (WorldPageId pid, page) = Aeson.object
          [ "pageId" .= pid
          , "buildingCount" .= HM.size (bsnInstances (pgsBuildings page))
          , "unitCount" .= HM.size (usnInstances (pgsUnits page))
          , "unitSimStateCount" .= HM.size (pgsUnitSimStates page)
          , "craftBillCount" .= HM.size (cbsBills (pgsCraftBills page))
          , "powerNodeCount" .= HM.size (pnsNodes (pgsPowerNodes page))
          , "groundItemCount" .= HM.size (gisItems (pgsGroundItems page))
          , "timeHour" .= pgsTimeHour page, "timeMinute" .= pgsTimeMinute page
          , "dateYear" .= pgsDateYear page, "dateMonth" .= pgsDateMonth page
          , "dateDay" .= pgsDateDay page
          , "mapMode" .= T.pack (show (pgsMapMode page))
          , "buildings" .= map dumpBuilding
              (sortOn (unBuildingId . fst)
                 (HM.toList (bsnInstances (pgsBuildings page))))
          , "units" .= map dumpUnit
              (sortOn (unUnitId . fst) (HM.toList (usnInstances (pgsUnits page))))
          , "unitSimStates" .= map dumpSim
              (sortOn (unUnitId . fst) (HM.toList (pgsUnitSimStates page)))
          , "craftBills" .= map dumpBill
              (sortOn cbId (HM.elems (cbsBills (pgsCraftBills page))))
          , "powerNodes" .= map dumpNode
              (sortOn pnId (HM.elems (pnsNodes (pgsPowerNodes page))))
          ]
        cam = snapLiveCamera snap
        WorldPageId activePageText = snapActivePage snap
        summary = Aeson.object
          [ "metadata" .= Aeson.object
              [ "seed" .= smSeed meta, "worldSize" .= smWorldSize meta
              , "plateCount" .= smPlateCount meta, "worldName" .= smWorldName meta
              , "worldGloss" .= smWorldGloss meta ]
          , "gameTime" .= snapGameTime snap
          , "nextItemId" .= snapNextItemId snap
          , "nextBuildingId" .= snapNextBuildingId snap
          , "nextUnitId" .= snapNextUnitId snap
          , "camera" .= Aeson.object
              [ "ownerPage" .= fmap (\(WorldPageId p) -> p) (lcsOwnerPage cam)
              , "x" .= lcsX cam, "y" .= lcsY cam, "zoom" .= lcsZoom cam
              , "facing" .= T.pack (show (lcsFacing cam)) ]
          , "activePage" .= activePageText
          , "visiblePages" .= map (\(WorldPageId p) -> p) (snapVisiblePages snap)
          , "pages" .= map dumpPage
              (sortOn (\(WorldPageId p, _) -> p) (HM.toList (snapPages snap)))
          , "luaComponentCount" .= length luaComponents
          , "isMigratedLegacyBaseline" .= isMig
          ]
    BSL.writeFile "{output_path}" (Aeson.encode summary)
    putStrLn "DUMP_OK"
:}}
"""

# A small, permanent GHCi program (run via `cabal repl`, mirroring
# GHCI_DUMP_SUMMARY_TEMPLATE's own subprocess pattern) that overwrites
# ONLY a freshly-generated fixture's "metadata" component's smTimestamp
# field with common.FIXED_GENERATED_TIMESTAMP, leaving every other
# component's version/required/payload bytes completely untouched.
#
# Round-11 review: engine.saveWorld (the real production save path
# --generate-session deliberately reuses, per requirement 21's "a real
# generation mode") always stamps the CURRENT WALL-CLOCK time into
# smTimestamp (Engine.Scripting.Lua.API.Save's getCurrentTime call,
# by design -- an ordinary player save needs each save to carry a
# distinct real timestamp). That means two --generate-session runs
# over IDENTICAL seed/world-size/plate-count/spawn arguments produce
# DIFFERENT envelope bytes and sha256s purely from wall-clock drift,
# defeating the reproducibility requirement 21 itself demands (a
# fixture's checksum must depend only on its declared generation
# inputs, not on when the command happened to run). This step
# normalizes that ONE field post-generation, via the real envelope
# codec (decode the raw manifest/payloads, rebuild every component's
# spec verbatim except metadata's, re-encode) rather than a hand-rolled
# binary patch -- so the fix stays correct through any future envelope
# framing change, exactly like every other fixture-generation step in
# this file.
GHCI_NORMALIZE_TIMESTAMP_TEMPLATE = r"""
:set -XOverloadedStrings
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import World.Save.Envelope.Codec
import World.Save.Envelope.Types
import World.Save.Envelope (currentEnvelopeVersion, metadataComponentId)
import World.Save.Component (componentKnownIds)
import World.Save.Types (SaveMetadata(..))

bytes <- BS.readFile "{fixture_path}"

:{{
let knownAll = HS.insert metadataComponentId
                 (HS.insert (ComponentId "lua.unit_ai")
                    (HS.insert (ComponentId "lua.building_spawn") componentKnownIds))
-- Structural re-encode only: knownAll widens what may APPEAR, while
-- the reader-required set stays EMPTY. Reusing knownAll for both would
-- demand that whatever fixture is being normalized carry every
-- component the current build knows about -- including any OPTIONAL one
-- added after the fixture was captured (#1087's container-knowledge),
-- which by definition it need not.
in case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion knownAll HS.empty bytes of
     Left e -> putStrLn ("NORMALIZE_FAILED: decode: " ++ show e)
     Right decoded ->
       case S.decode
              (HM.lookupDefault BS.empty metadataComponentId (dePayloads decoded))
              :: Either String SaveMetadata of
         Left e -> putStrLn ("NORMALIZE_FAILED: metadata decode: " ++ e)
         Right meta -> do
           let fixedMeta = meta {{ smTimestamp = "{fixed_timestamp}" }}
               newSpecs =
                 [ ( cdId d, cdVersion d, cdRequired d
                   , if cdId d == metadataComponentId
                        then S.encode fixedMeta
                        else HM.lookupDefault BS.empty (cdId d) (dePayloads decoded) )
                 | d <- emComponents (deManifest decoded) ]
           case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion newSpecs of
             Left e -> putStrLn ("NORMALIZE_FAILED: encode: " ++ show e)
             Right outBytes -> do
               BS.writeFile "{fixture_path}" outBytes
               putStrLn "NORMALIZE_OK"
:}}
"""


def normalize_fixture_timestamp(fixture_path: Path) -> tuple[bool, str]:
    """Run GHCI_NORMALIZE_TIMESTAMP_TEMPLATE via a `cabal repl` subprocess
    to overwrite fixture_path's metadata smTimestamp with
    common.FIXED_GENERATED_TIMESTAMP, in place. Returns (ok, diagnostic-tail-on-
    failure)."""
    script = GHCI_NORMALIZE_TIMESTAMP_TEMPLATE.format(
        fixture_path=str(fixture_path),
        fixed_timestamp=common.FIXED_GENERATED_TIMESTAMP)
    try:
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=script, cwd=common.REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    output = (proc.stdout or "") + (proc.stderr or "")
    if "NORMALIZE_OK" not in output:
        return False, "\n".join(output.splitlines()[-60:])
    return True, ""


# A small, permanent GHCi program (run via `cabal repl`, mirroring the
# other GHCI_*_TEMPLATE constants' subprocess pattern) that decodes a
# batch of REAL tracked fixture files' RAW envelope manifests -- their
# actual on-disk (id, version, required) descriptors, exactly as the
# real codec sees them -- and writes them all out as one JSON object
# keyed by fixture path. A single, UNIVERSAL known-id set (every
# Haskell/live-Lua modern id, plus BOTH retired legacy ids "session"
# and "lua-state") is used for every fixture regardless of which shape
# it actually is, since this only needs the envelope's STRUCTURAL
# manifest -- no application-level decode/migration -- to succeed for
# any of B1/B2/B3/C3's tracked shapes (round-12 review).
#
# Round-12 review: tools/save_compat_audit.py's version-coverage checks
# (audit_component_versions) previously trusted a baseline's declared
# components[] versions as-is, entirely from the manifest JSON -- never
# cross-checked against what a fixture's OWN bytes actually contain.
# Bumping only the manifest's declared version (with no fixture change
# at all) satisfied every coverage check while validating nothing.
# verify_fixture_descriptors (below) uses this dump to grind that
# claim against real, decoded descriptors before trusting it.
GHCI_DUMP_DESCRIPTORS_TEMPLATE = r"""
:set -XOverloadedStrings
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as AK
import World.Save.Envelope.Codec
import World.Save.Envelope.Types
import World.Save.Envelope (currentEnvelopeVersion, metadataComponentId)
import World.Save.Component (componentKnownIds)
import World.Save.Compat.SessionV90 (sessionComponentId)

:{
let universalKnown = HS.insert metadataComponentId
        (HS.insert sessionComponentId
            (HS.insert (ComponentId "lua-state")
                (HS.insert (ComponentId "lua.unit_ai")
                    (HS.insert (ComponentId "lua.building_spawn")
                        componentKnownIds))))
    cidText (ComponentId t) = t
    dumpOne path = do
      bytes <- BS.readFile path
      pure $ case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                     universalKnown HS.empty bytes of
        Left e -> (path, Left (show e))
        Right decoded -> (path, Right
          [ Aeson.object
              [ "id" .= cidText (cdId d), "version" .= cdVersion d
              , "required" .= cdRequired d ]
          | d <- emComponents (deManifest decoded) ])
in do
  results <- mapM dumpOne ("__FIXTURE_PATHS__" :: [FilePath])
  let failed = [ (p, e) | (p, Left e) <- results ]
  if not (null failed)
    then putStrLn ("DESCRIPTOR_DUMP_FAILED: " ++ show failed)
    else do
      let obj = Aeson.object
            [ AK.fromString p .= descs | (p, Right descs) <- results ]
      BSL.writeFile "__OUTPUT_PATH__" (Aeson.encode obj)
      putStrLn "DESCRIPTOR_DUMP_OK"
:}
"""


def dump_fixture_descriptors(
        fixture_paths: list[Path]) -> tuple[dict[str, list[dict]] | None, str]:
    """Run GHCI_DUMP_DESCRIPTORS_TEMPLATE via a single `cabal repl`
    subprocess to decode every path in fixture_paths' RAW envelope
    manifest. Returns (path-string -> [{"id","version","required"}, ...]
    for every fixture, "") on success, or (None, diagnostic) on any
    decode/subprocess failure."""
    if not fixture_paths:
        return {}, ""
    haskell_list = "[" + ",".join(
        json.dumps(str(p)) for p in fixture_paths) + "]"
    with tempfile.NamedTemporaryFile(
            suffix=".json", dir=common.REPO_ROOT, delete=False) as tf:
        output_path = Path(tf.name)
    try:
        script = (GHCI_DUMP_DESCRIPTORS_TEMPLATE
            .replace('"__FIXTURE_PATHS__"', haskell_list)
            .replace("__OUTPUT_PATH__", str(output_path)))
        try:
            proc = subprocess.run(
                ["cabal", "repl", "test:synarchy-test-headless"],
                input=script, cwd=common.REPO_ROOT, capture_output=True, text=True,
                timeout=1800)
        except FileNotFoundError:
            return None, "'cabal' was not found on PATH"
        output = (proc.stdout or "") + (proc.stderr or "")
        if "DESCRIPTOR_DUMP_OK" not in output or not output_path.exists():
            return None, "\n".join(output.splitlines()[-60:])
        return json.loads(output_path.read_text(encoding="utf-8")), ""
    finally:
        output_path.unlink(missing_ok=True)


def dump_canonical_summary(fixture_path: Path, output_path: Path) -> tuple[bool, str]:
    """Run GHCI_DUMP_SUMMARY_TEMPLATE via a `cabal repl` subprocess to
    derive fixture_path's canonical summary and write it to output_path.
    Returns (ok, diagnostic-tail-on-failure)."""
    script = GHCI_DUMP_SUMMARY_TEMPLATE.format(
        fixture_path=str(fixture_path), output_path=str(output_path))
    try:
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=script, cwd=common.REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    output = (proc.stdout or "") + (proc.stderr or "")
    if "DUMP_OK" not in output or not output_path.exists():
        return False, "\n".join(output.splitlines()[-60:])
    return True, ""
