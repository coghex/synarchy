{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.Lifecycle
    ( worldInitFn
    , worldGetIdentityFn
    , worldGetLanguageProvenanceFn
    , worldInitArenaFn
    , worldInitArenaDoneFn
    , worldOpenArenaFn
    , worldShowFn
    , worldHideFn
    , worldGetInitProgressFn
    , worldWaitForInitFn
    , worldDestroyFn
    , worldDestroyAllFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.Char (isDigit)
import Data.IORef (readIORef)
import Control.Concurrent (threadDelay)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.State (EngineEnv, luaQueue, activeWorldStateFrom)
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import Language.Generated.Types
    ( LanguageProvenance(..), GeneratorVersion(..), LangSeed(..)
    , GeneratorError(..), generatorErrorText, currentGeneratorVersion
    , supportedGeneratorVersions, langSeedText )
import World.Generate.Config
    (minimumWorldSize, normalizePlateCount, normalizeWorldSize)
import World.Plate (defaultPlatesFor)

-- | world.init(pageId, seed, worldSizeInChunks, plateCount
--             [, displayName[, gloss[, languageSeed[, languageVersion]]]])
--   The optional trailing arguments (#707) give the page a player-facing
--   identity: a display name plus an optional English gloss. They are
--   display TEXT (spaces/punctuation welcome, no save-name rules); each
--   is trimmed of leading/trailing whitespace and an omitted, nil, or
--   whitespace-only display name creates an unnamed page (discarding any
--   gloss). Read it back with world.getIdentity(pageId).
--
--   @languageSeed@ (#1101) declares that the supplied name/gloss were
--   RENDERED from a generated language, recording that language's #1092
--   provenance on the page — which is what lets the page's placed
--   locations be named in the same language ("Location.Naming"). It is
--   a DECIMAL STRING for the same reason 'world.getLanguageProvenance'
--   returns one: a language seed is an unsigned 64-bit value and Lua
--   carries neither a @Word64@ integer nor an exact double for the top
--   of that range. @languageVersion@ is the generator version, defaulting
--   to the current one.
--
--   Provenance is only ever attached to a name the caller states came
--   from that language, and it is never inferred: with no display name
--   there is no identity to attach it to, and a malformed seed or an
--   unconstructible version is refused with a warning, leaving an
--   ordinary custom-named page (#708 principle 7).
worldInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitFn env = do
    pageIdArg ← Lua.tostring 1
    seedArg   ← Lua.tointeger 2
    sizeArg   ← Lua.tointeger 3
    platesArg ← Lua.tointeger 4
    nameArg   ← Lua.tostring 5
    glossArg  ← Lua.tostring 6
    langArg   ← Lua.tostring 7
    langVerArg ← Lua.tointeger 8

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                seed   = maybe 42 fromIntegral seedArg
                mName  = TE.decodeUtf8Lenient ⊚ nameArg
                mGloss = TE.decodeUtf8Lenient ⊚ glossArg
            mProv ← case TE.decodeUtf8Lenient ⊚ langArg of
                Nothing  → pure Nothing
                Just raw → do
                    logger ← readIORef (ccLoggerRef (toCoreCapability env))
                    parseProvenance logger raw
                        (fromIntegral ⊚ langVerArg)
            let identity = case mProv of
                    Just prov → mkGeneratedWorldIdentity mName mGloss prov
                    Nothing   → mkWorldIdentity mName mGloss
                rawSize = maybe 64 fromIntegral sizeArg
            -- A provenance the caller DID supply, parsed fine, and that
            -- still went nowhere because there is no name for it to
            -- describe. Silently dropping it would leave the page's
            -- locations unnamed with nothing to explain why.
            when (isJust mProv ∧ isNothing identity) $ do
                logger ← readIORef (ccLoggerRef (toCoreCapability env))
                logWarn logger CatWorld
                    "world.init ignoring language provenance: no display \
                    \name was supplied, so the page has no identity to \
                    \record it on."
            let
                size = normalizeWorldSize rawSize
                -- Plate count scales with worldSize when caller
                -- doesn't supply one — fixes the "10 plates for any
                -- world" issue (audit #17). Explicit user values
                -- still honored after minimum-count normalization.
                rawPlates = maybe (defaultPlatesFor size) fromIntegral platesArg
                plates = normalizePlateCount rawPlates
            when (size ≢ rawSize ∨ plates ≢ rawPlates) $ do
                logger ← readIORef (ccLoggerRef (toCoreCapability env))
                logWarn logger CatWorld $
                    "world.init normalized worldgen inputs: worldSize "
                    <> T.pack (show rawSize) <> " → "
                    <> T.pack (show size) <> ", plateCount "
                    <> T.pack (show rawPlates) <> " → "
                    <> T.pack (show plates)
                    <> " (worldSize minimum/multiple "
                    <> T.pack (show minimumWorldSize)
                    <> ", plateCount min 1)."
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env))
                (WorldInit pageId seed size plates identity)
        Nothing → pure ()

    return 0

-- | Parse @world.init@'s optional language-provenance arguments (#1101).
--   'Nothing' — with a warning naming what was wrong — for a seed that
--   is not a plain unsigned decimal in 'Word64' range, or a generator
--   version this build cannot construct a profile for. Refusing is the
--   point: a page whose recorded language cannot be rebuilt would name
--   its locations in some OTHER language, which is worse than naming
--   them from their definition labels.
parseProvenance
    ∷ LoggerState → Text → Maybe Int → IO (Maybe LanguageProvenance)
parseProvenance logger raw mVer = case mSeed of
    Nothing → refuse
        "language seed must be an unsigned decimal integer below 2^64"
    Just s
        | ver `elem` supportedGeneratorVersions →
            pure $ Just LanguageProvenance
                { lpSeed = LangSeed (fromInteger s), lpVersion = ver }
        | otherwise → refuse
            (generatorErrorText (UnsupportedGeneratorVersion
                                    (generatorVersionInt ver)))
  where
    trimmed = T.strip raw
    -- Parsed as an 'Integer' and range-checked, never as a 'Word64':
    -- 'fromInteger' at that type wraps silently, so an out-of-range seed
    -- would otherwise become a DIFFERENT, perfectly valid language. The
    -- digit guard is what keeps 'Read' from accepting "0x10" / "-1".
    mSeed = case reads (T.unpack trimmed) ∷ [(Integer, String)] of
        [(n, "")] | not (T.null trimmed)
                  , T.all isDigit trimmed
                  , n ≤ toInteger (maxBound ∷ Word64) → Just n
        _ → Nothing
    ver = maybe currentGeneratorVersion GeneratorVersion mVer
    refuse why = do
        logWarn logger CatWorld $
            "world.init ignoring language provenance (" <> why
            <> "); the page keeps its custom name with no language."
        pure Nothing

-- | world.getIdentity(pageId) → { name, gloss? } | nil
--   Read-only query for a page's player-facing identity (#707). Returns
--   a table with the display name (and the gloss when one was stored)
--   for a named page; nil when the page does not exist or has no
--   identity (unnamed 4-argument world.init pages, arenas). There is
--   deliberately no setter — identity changes only by loading saved
--   state.
worldGetIdentityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetIdentityFn env = do
    pageIdArg ← Lua.tostring 1
    mIdentity ← Lua.liftIO $ case pageIdArg of
        Just pageIdBS → do
            mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            case lookup (WorldPageId (TE.decodeUtf8Lenient pageIdBS)) (wmWorlds mgr) of
                Just ws → readIORef (wsIdentityRef ws)
                Nothing → pure Nothing
        Nothing → pure Nothing
    case mIdentity of
        Just ident → do
            Lua.newtable
            Lua.pushstring (TE.encodeUtf8 (wiName ident))
            Lua.setfield (-2) "name"
            forM_ (wiGloss ident) $ \g → do
                Lua.pushstring (TE.encodeUtf8 g)
                Lua.setfield (-2) "gloss"
        Nothing → Lua.pushnil
    return 1

-- | world.getLanguageProvenance(pageId) → { seed, version } | nil
--   Read-only query for which generated language named a page, and
--   under which generator version (#1092) — enough to rebuild that
--   language's profile without reaching into save internals. Returns
--   nil for every page that genuinely has no language: a missing page,
--   an unnamed one, a CUSTOM-named one (a player-entered name has no
--   inferred meaning, #708 principle 7), and one restored from a save
--   written before provenance was recorded.
--
--   @seed@ is a DECIMAL STRING, not a number: a language seed is an
--   unsigned 64-bit value, and Lua's integer is signed 64-bit while
--   its number is a double, so either would silently mangle the top of
--   the range. @version@ is a small integer and is pushed as one.
worldGetLanguageProvenanceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetLanguageProvenanceFn env = do
    pageIdArg ← Lua.tostring 1
    mProv ← Lua.liftIO $ case pageIdArg of
        Just pageIdBS → do
            mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            pageLanguageProvenance mgr
                (WorldPageId (TE.decodeUtf8Lenient pageIdBS))
        Nothing → pure Nothing
    case mProv of
        Just prov → do
            Lua.newtable
            Lua.pushstring (TE.encodeUtf8 (langSeedText (lpSeed prov)))
            Lua.setfield (-2) "seed"
            Lua.pushinteger (fromIntegral (generatorVersionInt (lpVersion prov)))
            Lua.setfield (-2) "version"
        Nothing → Lua.pushnil
    return 1

-- | world.initArena(pageId) — create flat test arena, no geology
worldInitArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8Lenient bs)
            Nothing → WorldPageId "test_arena"    -- default when called with no args
    Lua.liftIO $ Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldInitArena pageId)
    return 0

-- | world.initArenaDone(pageId) — signal that all arena textures have been sent
worldInitArenaDoneFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaDoneFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8Lenient bs)
            Nothing → WorldPageId "test_arena"
    Lua.liftIO $ Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldInitArenaDone pageId)
    return 0

-- | world.openArena() — convenience function that broadcasts to Lua
worldOpenArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldOpenArenaFn env = do
    Lua.liftIO $ Q.writeQueue (luaQueue env) (LuaOpenArena)
    return 0

-- | world.show(pageId)
worldShowFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldShowFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldShow pageId)
        Nothing → pure ()

    return 0

-- | world.hide(pageId)
worldHideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHideFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldHide pageId)
        Nothing → pure ()

    return 0

-- | world.getInitProgress() → (phase, current, total, stage)
--   phase: 0=idle, 1=setup, 2=chunks, 3=done
--   current/total: numeric progress within current phase
--   stage: human-readable string ("idle", "setup", "chunks", "done")
--
--   Returns 4 values for backward compatibility: existing Lua scripts
--   use `local phase, current, total = world.getInitProgress()` and
--   the 4th value (stage) is simply ignored by those callers.
worldGetInitProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetInitProgressFn env = do
    mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
    case mWs of
        Just worldState → do
            phase ← Lua.liftIO $ readIORef (wsLoadPhaseRef worldState)
            case phase of
                LoadIdle → do
                    Lua.pushinteger 0
                    Lua.pushinteger 0
                    Lua.pushinteger 0
                    Lua.pushstring "idle"
                LoadPhase1 current total → do
                    Lua.pushinteger 1
                    Lua.pushinteger (fromIntegral current)
                    Lua.pushinteger (fromIntegral total)
                    Lua.pushstring "setup"
                LoadPhase2 remaining total → do
                    Lua.pushinteger 2
                    Lua.pushinteger (fromIntegral (total - remaining))
                    Lua.pushinteger (fromIntegral total)
                    Lua.pushstring "chunks"
                LoadDone → do
                    Lua.pushinteger 3
                    Lua.pushinteger 1
                    Lua.pushinteger 1
                    Lua.pushstring "done"
            return 4
        Nothing → do
            Lua.pushinteger 0
            Lua.pushinteger 0
            Lua.pushinteger 0
            Lua.pushstring "idle"
            return 4

-- | world.waitForInit(timeout_seconds) → table (same as getInitProgress)
--   Blocks until world generation is complete or timeout is reached.
--   Default timeout: 600 seconds (10 minutes).
--   Returns the final progress table.
worldWaitForInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldWaitForInitFn env = do
    timeoutArg ← Lua.tointeger 1
    let timeoutSec = case timeoutArg of
            Just t | t > 0 → fromIntegral t ∷ Int
            _              → 600
        maxIter = timeoutSec * 4  -- poll at 250ms intervals
    Lua.liftIO $ waitLoop maxIter
    worldGetInitProgressFn env
  where
    waitLoop 0 = return ()
    waitLoop n = do
        mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
        case mWs of
            Just ws → do
                phase ← readIORef (wsLoadPhaseRef ws)
                case phase of
                    LoadDone → return ()
                    _        → do
                        threadDelay 250000
                        waitLoop (n - 1)
            Nothing → do
                threadDelay 250000
                waitLoop (n - 1)

-- | world.destroy(pageId)
-- Removes the world from the world manager entirely, freeing its state.
worldDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDestroyFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldDestroy pageId)
        Nothing → pure ()

    return 0

-- | world.destroyAll() — tear down every world (Exit to Menu). Clears
--   wmWorlds/wmVisible (so no hidden world resolves as the implicit active
--   world behind the menu), sim-deactivates each, and resets the global
--   unit/building managers. (#58)
worldDestroyAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDestroyAllFn env = do
    Lua.liftIO $ Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) WorldDestroyAll
    return 0
