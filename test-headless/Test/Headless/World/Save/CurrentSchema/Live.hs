{-# LANGUAGE TypeApplications #-}
-- | The live evidence behind the current-schema record (issue #2916):
--   the compiled Haskell registry, the metadata and framing values, the
--   retired legacy routes, and the Lua save-module registry evaluated by
--   loading the production registrations in the headless Lua harness.
--
--   Every accepted-version list below is PROBED through the real decode
--   entry point (not read from an advertisement alone), and a mismatch
--   between the two is a problem. Nothing here reads source text.
module Test.Headless.World.Save.CurrentSchema.Live
    ( liveCarriers
    , liveRoutes
    , liveExtras
    , liveProblems
    , advertisedMismatch
    , customProblems
    , luaEvidence
    , LuaEvidence(..)
    ) where

import UPrelude
import Data.Aeson (Value(..), object, (.=), eitherDecodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Numeric (showHex)
import qualified HsLua as Lua

import World.Save.Component (saveComponentRegistry)
import World.Save.Wire (WireRoot(..), WireNode(..), WireKind(..))
import World.Save.Wire.Custom (customBoundaries)
import qualified Data.Map.Strict as Map
import World.Save.Component.Types
import World.Save.Compat.MetadataV1 (SaveMetadataV1(..))
import World.Save.Compat.SessionV90 (sessionComponentId, sessionComponentVersion)
import World.Save.Envelope
import World.Save.Envelope.Codec (DecodedEnvelope(..), encodeEnvelope)
import World.Save.Envelope.Types
import Test.Headless.World.Save.CurrentSchema.Record

-- Haskell registry ---------------------------------------------------

probeRange ∷ [Word32] → [Word32]
probeRange advertised = [0 .. maximum (0 : advertised) + 3]

-- | The versions the real per-component decode entry point
--   ('rcPrepare', the one 'assembleSnapshot' runs) accepts, found by
--   handing it a descriptor at each version with an empty payload and
--   reading whether it reports the unsupported-version failure.
probeComponent ∷ RegisteredComponent → [Word32]
probeComponent rc =
    [ v | v ← probeRange (rcInputVers rc), accepts v ]
  where
    cid = rcId rc
    accepts v = case rcPrepare rc (envelopeAt v) of
        Left errs → not (any unsupported errs)
        Right _   → True
    unsupported e = "unsupported save version" `T.isPrefixOf` ceMessage e
    envelopeAt v = DecodedEnvelope
        { deVersion = currentEnvelopeVersion
        , deManifest = EnvelopeManifest
            [ ComponentDescriptor cid v True 0 0 0 ]
        , dePayloads = HM.singleton cid BS.empty }

componentCarrier ∷ RegisteredComponent → CarrierIn
componentCarrier rc = CarrierIn
    { ciId = "haskell:" <> cidText (rcId rc)
    , ciClass = "component"
    , ciVersion = rcVersion rc
    , ciAccepted = probeComponent rc
    , ciRequired = rcRequired rc
    , ciRoot = Just (rcWire rc)
    , ciOwners = []
    , ciDetail = object [ "advertised" .= L.sort (rcInputVers rc) ] }
  where cidText (ComponentId t) = t

-- Metadata and framing -----------------------------------------------

encodeAt ∷ Word32 → [ComponentSpec'] → BS.ByteString
encodeAt envVersion specs =
    either (error . show) id
        (encodeEnvelope defaultEnvelopeLimits envVersion specs)

type ComponentSpec' = (ComponentId, Word32, Bool, BS.ByteString)

-- | Every REQUIRED Haskell component, empty: the metadata-only listing
--   still demands the reader's whole required set be present.
requiredFiller ∷ [ComponentSpec']
requiredFiller =
    [ (rcId rc, rcVersion rc, True, BS.empty)
    | rc ← saveComponentRegistry, rcRequired rc ]

-- | The versions the real metadata-only decode accepts for the
--   @"metadata"@ component.
probeMetadata ∷ [Word32]
probeMetadata =
    [ v | v ← probeRange metadataComponentInputVersions
        , accepts v ]
  where
    accepts v = case decodeSaveEnvelopeMetadata HS.empty
                         (encodeAt currentEnvelopeVersion
                            ((metadataComponentId, v, True, BS.empty) : requiredFiller)) of
        Left t  → not ("unsupported save version: metadata component"
                       `T.isPrefixOf` t)
        Right _ → True

-- | The envelope-framing versions the real decode accepts.
probeFraming ∷ [Word32]
probeFraming = [ v | v ← probeRange [currentEnvelopeVersion], accepts v ]
  where
    accepts v = case decodeSaveEnvelopeMetadata HS.empty
                         (encodeAt v
                            ((metadataComponentId, metadataComponentVersion
                             , True, BS.empty) : requiredFiller)) of
        Left t  → not ("unsupported save version: envelope framing"
                       `T.isPrefixOf` t)
        Right _ → True

hex ∷ BS.ByteString → Text
hex = T.pack . concatMap (\b → pad (showHex b "")) . BS.unpack
  where pad s = replicate (2 - length s) '0' ⧺ s

-- | The header length, MEASURED from a real encoded envelope rather than
--   read from the codec's private constant: total length less the
--   cereal manifest (whose length the header itself states), its 8-byte
--   checksum and the payload.
measuredHeaderSize ∷ Int
measuredHeaderSize =
    let payload = "ab" ∷ BS.ByteString
        bytes = encodeAt currentEnvelopeVersion
                  [(ComponentId "probe", 1, True, payload)]
        manifestLen = fromIntegral (decodeW64 (BS.take 8 (BS.drop 8 bytes)))
    in BS.length bytes - manifestLen - 8 - BS.length payload

-- | The framing's compiled constants and a byte-for-byte probe of the
--   hand-written header layout and the cereal-encoded manifest around a
--   fixed component. DECLARED, not derived: the layout is hand-written
--   bytes, so this probe is what makes a change to it visible.
framingDetail ∷ Value
framingDetail = object
    [ "currentEnvelopeVersion" .= currentEnvelopeVersion
    , "envelopeMagic" .= envelopeMagic
    , "headerSize" .= measuredHeaderSize
    , "fnv1a64Probe" .= tshow (fnv1a64 "synarchy-current-schema")
    , "layoutProbeHex" .= hex (encodeAt currentEnvelopeVersion
          [ (ComponentId "probe", 1, True, "ab")
          , (ComponentId "probe-2", 2, False, "c") ])
    , "contract" .= ( "header: magic(4) envelopeVersion(4) manifestLength(8), \
                      \big endian and hand written; then the cereal-encoded \
                      \EnvelopeManifest, its fnv1a64 checksum(8), and the \
                      \component payloads. Hand-written bytes are declared \
                      \and probed, not derived." ∷ Text) ]

-- Legacy routes ------------------------------------------------------

legacyV1Metadata ∷ BS.ByteString
legacyV1Metadata = S.encode (SaveMetadataV1 "n" 1 2 3 "t" Nothing Nothing)

-- | A correctly SHAPED B1 envelope: {metadata v1, session v90}, both
--   required, with an empty session payload. The route is recognised by
--   its shape; it then fails at the session decode, so the probes tell
--   "recognised" from "not recognised" by how far each entry point got.
b1Probe ∷ BS.ByteString
b1Probe = encodeAt currentEnvelopeVersion
    [ (metadataComponentId, legacyMetadataComponentVersion, True, legacyV1Metadata)
    , (sessionComponentId, sessionComponentVersion, True, BS.empty) ]

-- | A correctly shaped B2 envelope: metadata v1, the opaque lua-state
--   blob, and every REQUIRED Haskell component, with empty payloads.
b2Probe ∷ BS.ByteString
b2Probe = encodeAt currentEnvelopeVersion $
    [ (metadataComponentId, legacyMetadataComponentVersion, True, legacyV1Metadata)
    , (luaStateComponentId, luaStateComponentVersion, True
      , S.encode (HM.empty ∷ HM.HashMap Text Text)) ]
    ⧺ [ (rcId rc, rcVersion rc, True, BS.empty)
      | rc ← saveComponentRegistry, rcRequired rc ]

recognised ∷ LegacyDecodeResult a → Bool
recognised NotLegacyShaped = False
recognised _               = True

-- | Whether the PUBLIC full-session decode took the legacy route: a
--   legacy-shaped envelope that got as far as per-component work
--   (@ReachedComponents@), rather than failing at the envelope.
publicFullRecognised ∷ BS.ByteString → Bool
publicFullRecognised bytes = case decodeSessionEnvelopeClassified HS.empty HS.empty bytes of
    Right _ → True
    Left (GenerationIncompatible (ReachedComponents _) _) → True
    Left _ → False

publicMetadataRecognised ∷ BS.ByteString → Bool
publicMetadataRecognised bytes =
    either (const False) (const True) (decodeSaveEnvelopeMetadata HS.empty bytes)

routeFlags
    ∷ (BS.ByteString → Bool) → (BS.ByteString → Bool)
    → (BS.ByteString → Bool) → (BS.ByteString → Bool)
    → BS.ByteString → Value
routeFlags pf pm df dm bytes = object
    [ "publicFullSession" .= pf bytes
    , "publicMetadataOnly" .= pm bytes
    , "directFullSession" .= df bytes
    , "directMetadataOnly" .= dm bytes ]

legacyCarriers ∷ [CarrierIn]
legacyCarriers =
    [ CarrierIn "legacy:session" "legacy" sessionComponentVersion
        [sessionComponentVersion] True Nothing []
        (object [ "route" .= ("b1-session" ∷ Text) ])
    , CarrierIn "legacy:lua-state" "legacy" luaStateComponentVersion
        [luaStateComponentVersion] True Nothing []
        (object [ "route" .= ("b2-session" ∷ Text) ]) ]

-- | Route ownership is CONSERVATIVE. B1 reconstructs the whole
--   'SessionSnapshot' from the frozen v90 shape, which reaches live
--   types current carriers also use, so it is owned by metadata and by
--   every Haskell gameplay carrier; narrowing that would need the
--   historical shape analysis this guard excludes. B2 is owned by
--   metadata, @lua-state@ and every required Haskell carrier.
liveRoutes ∷ [RouteIn]
liveRoutes =
    [ RouteIn "b1-session"
        ("metadata" : "legacy:session" : haskellIds)
        (routeFlags publicFullRecognised publicMetadataRecognised
            (recognised . decodeLegacySessionEnvelope)
            (recognised . decodeLegacySessionMetadata) b1Probe)
    , RouteIn "b2-session"
        ("metadata" : "legacy:lua-state"
            : [ "haskell:" <> cidText (rcId rc) | rc ← saveComponentRegistry, rcRequired rc ])
        (routeFlags publicFullRecognised publicMetadataRecognised
            (recognised . decodeB2SessionEnvelope)
            (recognised . decodeB2SessionMetadata) b2Probe) ]
  where
    haskellIds = [ "haskell:" <> cidText (rcId rc) | rc ← saveComponentRegistry ]
    cidText (ComponentId t) = t

-- Lua ----------------------------------------------------------------

data LuaEvidence = LuaEvidence
    { leModules ∷ [Value]
    , leLoaded  ∷ [Text]
    , leProbe   ∷ Text
    }

-- | Load the production loader's script list with a RECORDING
--   @engine.loadScript@, run every recorded script's real @init@, and
--   read the evaluated @saveModules.registry@. Only engine services the
--   scripts call outside a boot are stubbed; a script that cannot be
--   loaded or initialised fails the run.
luaChunk ∷ Text
luaChunk = T.intercalate "\n"
    [ "local loaded = {}"
    , "local function proxy()"
    , "  return setmetatable({}, { __index = function(t, k)"
    , "    local f = function() return nil end; rawset(t, k, f); return f end })"
    , "end"
    , "engine = proxy()"
    , "engine.getBootProfile = function() return 'normal' end"
    , "engine.loadScript = function(path, tick) loaded[#loaded + 1] = path; return #loaded end"
    , "engine.gameTime = function() return 1000.0 end"
    , "engine.getUIScale = function() return 1.0 end"
    , "engine.getFramebufferSize = function() return 1280, 720 end"
    , "engine.getWindowSize = function() return 1280, 720 end"
    , "for _, n in ipairs({'UI','unit','world','building','camera','item','combat','injury','thought','input','scene'}) do _G[n] = proxy() end"
    , "local ok, err = pcall(function() require('scripts.init_loader').load() end)"
    , "if not ok then error('init_loader.load failed: ' .. tostring(err)) end"
    , "if #loaded == 0 then error('the production loader requested no scripts') end"
    , "local sm = require('scripts.lib.save_modules')"
    , "local owners = {}"
    , "for i, path in ipairs(loaded) do"
    , "  local before = {}"
    , "  for id in pairs(sm.registry) do before[id] = true end"
    , "  local lok, lerr = pcall(function()"
    , "    local chunk = assert(loadfile(path))"
    , "    local m = chunk()"
    , "    if type(m) == 'table' and type(m.init) == 'function' then m.init(i) end"
    , "  end)"
    , "  if not lok then error('script ' .. path .. ' failed under the stubs: ' .. tostring(lerr)) end"
    , "  for id in pairs(sm.registry) do if not before[id] then owners[id] = path end end"
    , "end"
    , "local function esc(s) return (tostring(s):gsub('[%c\"\\\\]', function(c) return string.format('\\\\u%04x', c:byte()) end)) end"
    , "local function arr(t) local o = {} for _, v in ipairs(t) do o[#o + 1] = tostring(v) end return '[' .. table.concat(o, ',') .. ']' end"
    , "local ids = {}"
    , "for id in pairs(sm.registry) do ids[#ids + 1] = id end"
    , "table.sort(ids)"
    , "local mods = {}"
    , "for _, id in ipairs(ids) do"
    , "  local r = sm.registry[id]"
    , "  local acc = {}"
    , "  local maxv = r.version"
    , "  for _, v in ipairs(r.inputVersions) do if v > maxv then maxv = v end end"
    , "  for v = 0, maxv + 3 do"
    , "    local res = sm.prepareLoad({ { id = id, version = v, payload = '' } }, nil, false)"
    , "    sm.abortPreparedLoad(nil)"
    , "    local unsupported = false"
    , "    for _, e in ipairs(res.errors or {}) do"
    , "      if e:find(\"unsupported save version: Lua module '\" .. id .. \"' found v\" .. v .. ' ', 1, true) then unsupported = true end"
    , "    end"
    , "    if not unsupported then acc[#acc + 1] = v end"
    , "  end"
    , "  mods[#mods + 1] = '{\"id\":\"' .. esc(id) .. '\",\"version\":' .. r.version .. ',\"inputVersions\":' .. arr(r.inputVersions)"
    , "    .. ',\"accepted\":' .. arr(acc) .. ',\"required\":' .. tostring(r.required) .. ',\"scope\":\"' .. esc(r.scope)"
    , "    .. '\",\"owner\":\"' .. esc(owners[id] or '') .. '\"}'"
    , "end"
    , "local codec = require('scripts.lib.data_codec')"
    , "local probe = codec.encode({ a = 1, b = { true, 'x', 2.5 }, c = 'y' })"
    , "local hexp = probe:gsub('.', function(c) return string.format('%02x', c:byte()) end)"
    , "local files = {}"
    , "for _, p in ipairs(loaded) do files[#files + 1] = '\"' .. esc(p) .. '\"' end"
    , "return '{\"modules\":[' .. table.concat(mods, ',') .. '],\"loaded\":[' .. table.concat(files, ',') .. '],\"codecProbeHex\":\"' .. hexp .. '\"}'"
    ]

-- | Run the chunk in a fresh interpreter; the failure text on error.
luaEvidence ∷ IO (Either Text LuaEvidence)
luaEvidence = do
    r ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 luaChunk)
        case status of
            Lua.OK → do
                out ← Lua.tostring (-1)
                pure (Right (maybe "" id out))
            _ → do
                err ← Lua.tostring (-1)
                pure (Left (maybe "<no message>" TE.decodeUtf8Lenient err))
    pure $ r >>= \bytes → case eitherDecodeStrict bytes of
        Left e → Left ("Lua evidence is not JSON: " <> T.pack e)
        Right (Object o) → Right LuaEvidence
            { leModules = case KM.lookup (K.fromText "modules") o of
                Just (Array vs) → V.toList vs
                _ → []
            , leLoaded = case KM.lookup (K.fromText "loaded") o of
                Just (Array vs) → [ t | String t ← V.toList vs ]
                _ → []
            , leProbe = case KM.lookup (K.fromText "codecProbeHex") o of
                Just (String t) → t
                _ → "" }
        Right _ → Left "Lua evidence is not an object"

luaCarriers ∷ LuaEvidence → [CarrierIn]
luaCarriers ev =
    [ CarrierIn
        { ciId = "lua:" <> field "id" m
        , ciClass = "lua-module"
        , ciVersion = num "version" m
        , ciAccepted = nums "accepted" m
        , ciRequired = boolean "required" m
        , ciRoot = Nothing
        , ciOwners = [ field "owner" m, "scripts/lib/data_codec.lua" ]
        , ciDetail = object [ "advertised" .= nums "inputVersions" m
                            , "scope" .= field "scope" m
                            , "contract" .= ( "Lua payloads are dynamic tables encoded by \
                                              \scripts/lib/data_codec.lua; no schema is \
                                              \declared, so a payload change is outside \
                                              \mechanical detection and must advance the \
                                              \version by hand." ∷ Text) ] }
    | m ← leModules ev ]
  where
    look k (Object o) = KM.lookup (K.fromText k) o
    look _ _ = Nothing
    field k m = case look k m of Just (String t) → t; _ → ""
    num k m = case look k m of Just (Number n) → round n; _ → 0
    nums k m = case look k m of
        Just (Array vs) → [ round n | Number n ← V.toList vs ]
        _ → []
    boolean k m = case look k m of Just (Bool b) → b; _ → False

-- Assembly -----------------------------------------------------------

metadataCarrier ∷ CarrierIn
metadataCarrier = CarrierIn
    { ciId = "metadata", ciClass = "metadata"
    , ciVersion = metadataComponentVersion
    , ciAccepted = probeMetadata, ciRequired = True
    , ciRoot = Just metadataWireRoot, ciOwners = []
    , ciDetail = object [ "advertised" .= L.sort metadataComponentInputVersions ] }

framingCarrier ∷ CarrierIn
framingCarrier = CarrierIn
    { ciId = "framing", ciClass = "framing"
    , ciVersion = currentEnvelopeVersion
    , ciAccepted = probeFraming, ciRequired = True
    , ciRoot = Just framingWireRoot, ciOwners = []
    , ciDetail = framingDetail }

liveCarriers ∷ LuaEvidence → [CarrierIn]
liveCarriers lua =
    map componentCarrier saveComponentRegistry
        ⧺ [metadataCarrier, framingCarrier] ⧺ legacyCarriers ⧺ luaCarriers lua

liveExtras ∷ LuaEvidence → Value
liveExtras lua = object
    [ "luaWire" .= object
        [ "owner" .= ("scripts/lib/data_codec.lua" ∷ Text)
        , "versioned" .= False
        , "probeHex" .= leProbe lua ]
    , "luaLoader" .= object [ "scriptsRequested" .= length (leLoaded lua) ] ]

-- | A carrier whose advertised acceptance disagrees with what its real
--   decode entry point accepts.
advertisedMismatch ∷ CarrierIn → [Text]
advertisedMismatch c =
    [ "advertised acceptance disagrees with the real decoder for "
      <> ciId c <> ": advertises " <> tshow adv <> ", decoder accepts "
      <> tshow (L.sort (ciAccepted c))
    | Object d ← [ciDetail c]
    , Just (Array xs) ← [KM.lookup (K.fromText "advertised") d]
    , let adv = L.sort [ round n ∷ Word32 | Number n ← V.toList xs ]
    , adv ≢ L.sort (ciAccepted c) ]

-- | Hand-written boundaries must be listed exactly: every reachable
--   'WKCustom' node is declared, and every declaration is reachable.
customProblems ∷ [CarrierIn] → [Text]
customProblems cs =
    [ "unlisted custom boundary reached: " <> k | k ← reached, k `notElem` declared ]
    ⧺ [ "declared custom boundary is unreachable: " <> k | k ← declared, k `notElem` reached ]
  where
    declared = L.sort (map fst customBoundaries)
    reached = L.sort (L.nub
        [ wnKey n | Just r ← map ciRoot cs, n ← Map.elems (wrNodes r)
        , isCustom (wnKind n) ])
    isCustom (WKCustom _) = True
    isCustom _ = False

-- | Cross-checks the record builder cannot make from one carrier alone.
liveProblems ∷ LuaEvidence → [Text]
liveProblems lua = concat
    [ concatMap advertisedMismatch (liveCarriers lua)
    , customProblems (liveCarriers lua)
    , [ "b2 ownership does not cover the compiled b2Ids: " <> tshow (HS.toList b2Ids)
      | HS.fromList (map (\(ComponentId t) → t) (HS.toList b2Ids))
          ≢ HS.fromList ("metadata" : "lua-state"
              : [ t | rc ← saveComponentRegistry, rcRequired rc
                , let ComponentId t = rcId rc ]) ]
    , [ "b1 ownership does not cover the compiled b1LegacyIds"
      | HS.map (\(ComponentId t) → t) b1LegacyIds ≢ HS.fromList ["metadata", "session"] ]
    , [ "the Lua loader recorded no registered module" | null (leModules lua) ]
    , [ "Lua module " <> m <> " has no owning script"
      | Object o ← leModules lua
      , Just (String m) ← [KM.lookup (K.fromText "id") o]
      , Just (String "") ← [KM.lookup (K.fromText "owner") o] ]
    ]
