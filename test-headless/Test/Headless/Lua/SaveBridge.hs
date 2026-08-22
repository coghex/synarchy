{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- | The Haskell/Lua save-bridge payload gate (issue #1103): the two
--   payload records "Engine.Scripting.Lua.API.Save.Bridge" builds from
--   NAMED Lua fields — 'LuaComponentSpec' and 'LuaRefEdge' — asserted
--   field by field against distinct sentinel values, driven through the
--   real @scripts/lib/save_modules.lua@ registry in a standalone HsLua
--   VM (no engine, no world/unit threads).
--
--   Why this exists as its own gate rather than leaning on the two
--   neighbouring suites: "Test.Headless.Lua.SaveModules" asserts the
--   LUA side of the same edges (that @snapshotAll@/@prepareLoad@ keep
--   every field the hook set), and "Test.Headless.World.Save.Integrity"
--   hand-constructs 'LuaRefEdge' values to exercise
--   @luaReferenceErrors@. Neither ever runs the stack reader that turns
--   one into the other, so a swap between the edge's three adjacent
--   'Text' fields — @component@, @kind@, @path@ — would pass both. That
--   reader is exactly where issue #1103's hazard lived, so the sentinels
--   below are all mutually distinct and every selector is asserted
--   separately: an assertion on the record as a whole would report a
--   transposition as one opaque mismatch.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "persistence reference integrity"'@.
module Test.Headless.Lua.SaveBridge (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Engine.Core.Log
    (LogConfig(..), LoggerState, defaultLogConfig, initLogger)
import Engine.Scripting.Lua.API.Save.Bridge (collectLuaComponents)
import World.Save.Payload (LuaComponentSpec(..), LuaRefEdge(..))

-- | Distinct sentinels for every field either record carries. No two
--   share a value, and the three 'Text' edge fields are not even
--   prefixes of one another, so ANY permutation among them fails a
--   named assertion below rather than passing silently.
sentinelComponent, sentinelKind, sentinelPath, sentinelPage ∷ Text
sentinelComponent = "bridge_sentinel_component"
sentinelKind      = "sentinel_kind"
sentinelPath      = "unit[3].sentinelPathField"
sentinelPage      = "sentinel_page_id"

sentinelId, sentinelOwner ∷ Int
sentinelId    = 4242
sentinelOwner = 77

sentinelVersion ∷ Word32
sentinelVersion = 7

-- | Register ONE persistent component whose @snapshot()@ and
--   @references()@ report nothing but sentinels, and stash the exact
--   canonical payload bytes @snapshotAll@ will encode as a global so the
--   Haskell side can compare against them instead of guessing.
setupChunk ∷ Text
setupChunk = T.intercalate "\n"
    [ "engine = { logWarn = function(...) end, logInfo = function(...) end }"
    , "local saveModules = require('scripts.lib.save_modules')"
    , "local codec = require('scripts.lib.data_codec')"
    , "local snapshotValue = { marker = 'sentinel-payload-marker' }"
    , "EXPECTED_PAYLOAD = codec.encode(snapshotValue)"
    , "saveModules.register('" <> sentinelComponent <> "', {"
    , "  version = " <> tshow sentinelVersion <> ","
    , "  inputVersions = {" <> tshow sentinelVersion <> "},"
    , "  required = true, scope = 'global', deps = {},"
    , "  snapshot = function() return snapshotValue end,"
    , "  decode = function(v, d) return d end,"
    , "  validate = function(d) return nil end,"
    , "  apply = function(d) end,"
    , "  references = function(d) return {{ kind = '" <> sentinelKind <> "',"
    , "      id = " <> tshow sentinelId <> ","
    , "      owner = " <> tshow sentinelOwner <> ","
    , "      path = '" <> sentinelPath <> "',"
    , "      page = '" <> sentinelPage <> "' }} end })"
    ]

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig { lcEnableByDefault = False }

-- | Run 'setupChunk' in a fresh interpreter, then the REAL
--   'collectLuaComponents' bridge call against it. Also returns the
--   @EXPECTED_PAYLOAD@ global so the payload assertion compares against
--   @data_codec@'s own output rather than a re-derived guess.
runBridge
    ∷ IO (Either Text ([LuaComponentSpec], [LuaRefEdge]), Maybe BS.ByteString)
runBridge = do
    logger ← quietLogger
    Lua.run @Lua.Exception $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 setupChunk)
        case status of
            Lua.OK → do
                collected ← collectLuaComponents logger
                _ ← Lua.getglobal (Lua.Name "EXPECTED_PAYLOAD")
                payload ← Lua.tostring (-1)
                Lua.pop 1
                return (collected, payload)
            _ → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                return ( Left ("setup chunk failed: "
                              <> maybe "<no message>" TE.decodeUtf8Lenient err)
                       , Nothing )

spec ∷ Spec
spec = describe "Haskell/Lua bridge payload records (issue #1103)" $ do
    it "collectLuaComponents decodes a reference edge into LuaRefEdge \
       \with every named Lua field landing in its OWN selector -- \
       \component, kind and path are all Text and adjacent, so this is \
       \the assertion a transposition has to get past" $ do
        (collected, _) ← runBridge
        case collected of
            Left err → expectationFailure (T.unpack err)
            Right (_, edges) → case edges of
                [e] → do
                    lreComponent e `shouldBe` sentinelComponent
                    lreKind      e `shouldBe` sentinelKind
                    lrePath      e `shouldBe` sentinelPath
                    lreId        e `shouldBe` sentinelId
                    lreOwner     e `shouldBe` Just sentinelOwner
                    lrePage      e `shouldBe` Just sentinelPage
                _ → expectationFailure
                        ("expected exactly one reference edge, got "
                         <> show (length edges) <> ": " <> show edges)

    it "collectLuaComponents decodes a component record into \
       \LuaComponentSpec with each named Lua field in its own selector, \
       \the payload being data_codec's own canonical bytes" $ do
        (collected, expectedPayload) ← runBridge
        case collected of
            Left err → expectationFailure (T.unpack err)
            Right (specs, _) → case specs of
                [s] → do
                    lcsId       s `shouldBe` sentinelComponent
                    lcsVersion  s `shouldBe` sentinelVersion
                    lcsRequired s `shouldBe` True
                    lcsPayload  s `shouldBe`
                        fromMaybe "<EXPECTED_PAYLOAD global missing>"
                                  expectedPayload
                _ → expectationFailure
                        ("expected exactly one component spec, got "
                         <> show (length specs) <> ": " <> show specs)
