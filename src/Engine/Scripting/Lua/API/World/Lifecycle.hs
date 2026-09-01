{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.Lifecycle
    ( worldInitFn
    , worldCheckMapImagePlanFn
    , worldGetIdentityFn
    , worldGetLanguageProvenanceFn
    , worldSuggestNameFn
    , maxSuggestNameOrdinal
    , worldGeneratedNameCharactersFn
    , SuggestionStep(..)
    , suggestionStep
    , suggestionStepLabel
    , readCatalogueForSuggestions
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
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Exception (IOException, evaluate, try)
import Control.Concurrent (threadDelay)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.State (EngineEnv, luaQueue, activeWorldStateFrom)
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Engine.Scripting.Lua.Types
    (LuaMsg(..), LuaBackendState(..), LanguageCache(..))
import World.Types
import Language.Generated.Types
    ( LanguageProvenance(..), GeneratorVersion(..), LangSeed(..)
    , GeneratorError(..), generatorErrorText, currentGeneratorVersion
    , supportedGeneratorVersions, langSeedText )
import Language.Generated.Orthography (outputInventory)
import Language.Semantic.Types (Catalogue, NameExpr, catalogueErrorText)
import Language.Etymology.Source (decodeNameExpr, encodeNameExpr)
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Language.Suggest
    ( NameSuggester, NameSuggestion(..), mkNameSuggester, suggestNameAt
    , suggestErrorText, worldLanguageSeed )
import World.Generate.Config
    (minimumWorldSize, normalizePlateCount, normalizeWorldSize)
import World.Map.ImagePlan (mapImageRefusalText)
import Engine.Map.ImageAdmission (admitWorldZoomAtlas)
import World.Plate (defaultPlatesFor)

-- | world.init(pageId, seed, worldSizeInChunks, plateCount
--             [, displayName[, gloss[, languageSeed[, languageVersion
--             [, nameExpr]]]]])
--
--   Returns @true@ when the world was accepted and queued, or
--   @false, message@ when it was REFUSED (#2020). A refusal is decided
--   synchronously, before anything is enqueued: no @WorldInit@ command,
--   no page registration, no worker generation, and whatever world is
--   currently live is left exactly as it was. Existing callers that
--   ignore the return values are unaffected — Lua discards extra
--   results.
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
--   @nameExpr@ (#1104) is the SEMANTIC EXPRESSION the name was rendered
--   from, in 'Language.Etymology.Source.encodeNameExpr''s compact text
--   form — exactly what @world.suggestName@ hands back as @expr@. It is
--   what lets the world's own name be decomposed into roots and
--   meanings later, and it is stored rather than recovered because a
--   rendered name cannot be parsed back into morphemes (bound forms are
--   shortenings and boundary repair edits letters).
--
--   Provenance is only ever attached to a name the caller states came
--   from that language, and it is never inferred: with no display name
--   there is no identity to attach it to, and a malformed seed or an
--   unconstructible version is refused with a warning, leaving an
--   ordinary custom-named page (#708 principle 7). The expression
--   follows the same rule one level down — it is honoured ONLY on the
--   generated-name path, and a malformed one is refused with a warning,
--   leaving a generated name whose etymology is simply unavailable
--   rather than one explained by a guess.
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
    exprArg   ← Lua.tostring 9

    refusal ← case pageIdArg of
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
            mExpr ← case (mProv, TE.decodeUtf8Lenient ⊚ exprArg) of
                (Just _, Just raw) → do
                    logger ← readIORef (ccLoggerRef (toCoreCapability env))
                    parseNameExpr logger raw
                -- An expression with no provenance describes no
                -- language, so there is nothing it could be checked
                -- against or rendered through. Dropped silently: the
                -- custom-name path is where a caller ends up when it
                -- supplies no language, and warning there would fire on
                -- every ordinary player-named world.
                _                  → pure Nothing
            let identity = case mProv of
                    Just prov → mkGeneratedWorldIdentity mName mGloss prov mExpr
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
                    <> tshow rawSize <> " → "
                    <> tshow size <> ", plateCount "
                    <> tshow rawPlates <> " → "
                    <> tshow plates
                    <> " (worldSize minimum/multiple "
                    <> tshow minimumWorldSize
                    <> ", plateCount min 1)."
            -- #2020: admission is SYNCHRONOUS and happens here, before
            -- anything is enqueued. A refusal registers no page, starts
            -- no worker generation, and leaves whatever world is
            -- currently live completely untouched — the caller gets the
            -- diagnostic as a second return value and decides what to
            -- do. This is deliberately not a new asynchronous
            -- world-generation failure phase.
            admitted ← admitWorldZoomAtlas env size
            case admitted of
                Left refusal → do
                    logger ← readIORef (ccLoggerRef (toCoreCapability env))
                    let msg = mapImageRefusalText refusal
                    logWarn logger CatWorld $ "world.init refused: " <> msg
                    pure (Just msg)
                Right _plan → do
                    -- As with initArena: a re-init replaces the page's
                    -- state.
                    enqueueSelectionChange env
                        (WorldInit pageId seed size plates identity)
                    pure Nothing
        Nothing → pure $ Just "world.init requires a page id"

    case refusal of
        Nothing → do
            Lua.pushboolean True
            return 1
        Just msg → do
            Lua.pushboolean False
            Lua.pushstring (TE.encodeUtf8 msg)
            return 2

-- | @world.checkMapImagePlan(worldSize)@ — the SIDE-EFFECT-FREE half of
--   'worldInitFn''s admission (#2020).
--
--   Create World cannot use @world.init@'s return value as its
--   pre-check: @scripts\/create_world\/generation.lua@ destroys the
--   current world before it ever reaches @worldView.startGeneration()@,
--   and @scripts\/world_view.lua@ may DEFER the actual @world.init@ call
--   until textures finish loading — so by the time a refusal could be
--   returned, the world the player still has is already gone. This verb
--   answers the same question ahead of that, through the SAME planner
--   and the same ceiling, so the two cannot disagree about what is
--   admissible or about how the refusal reads.
--
--   Normalizes its argument exactly as @world.init@ does, so the size it
--   answers about is the size that would actually be generated.
--
--   Returns @true@, or @false, message@.
worldCheckMapImagePlanFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldCheckMapImagePlanFn env = do
    sizeArg ← Lua.tointeger 1
    result ← Lua.liftIO $ case sizeArg of
        Nothing → pure $ Just "world.checkMapImagePlan requires a world size"
        Just raw → do
            admitted ← admitWorldZoomAtlas env
                           (normalizeWorldSize (fromIntegral raw))
            pure $ either (Just . mapImageRefusalText) (const Nothing) admitted
    case result of
        Nothing → do
            Lua.pushboolean True
            return 1
        Just msg → do
            Lua.pushboolean False
            Lua.pushstring (TE.encodeUtf8 msg)
            return 2

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

-- | Parse @world.init@'s optional name-expression argument (#1104).
--   'Nothing' — with a warning — for anything that is not one of the
--   four expression shapes
--   'Language.Etymology.Source.encodeNameExpr' emits.
--
--   Refusing rather than approximating matches 'parseProvenance' one
--   level down: a page that records no expression simply reports its
--   etymology as unavailable, which is honest, whereas a guessed one
--   would attach a fabricated derivation to a real world name (#1104
--   requirement 7). The page keeps its name, its gloss, and its
--   language either way.
parseNameExpr ∷ LoggerState → Text → IO (Maybe NameExpr)
parseNameExpr logger raw = case decodeNameExpr (T.strip raw) of
    Just expr → pure (Just expr)
    Nothing   → do
        logWarn logger CatWorld $
            "world.init ignoring name expression " <> tshow raw
            <> " (not a recognized expression form); the page keeps its \
            \generated name with no etymology."
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

-- | world.suggestName(worldSeed [, ordinal])
--     → { name, gloss, expr, language = { seed, version } }
--     → nil, errorMessage
--
--   The producer side of the naming arc (#1106): suggestion number
--   @ordinal@ (0-based, defaulting to 0) of the language belonging to
--   @worldSeed@ — the NORMALIZED numeric world seed the Create World
--   screen is about to pass to @world.init@, so the world's own name and
--   its locations' names come out of the same language, and two
--   spellings of one seed identify one language.
--
--   Returns the native name AND its English gloss, both rendered from
--   one semantic expression, that EXPRESSION itself as @expr@ (#1104,
--   in 'Language.Etymology.Source.encodeNameExpr''s compact text form —
--   pass it straight back to @world.init@ so an accepted suggestion can
--   later be decomposed into roots and meanings), plus the #1092
--   provenance to record if the
--   player accepts the suggestion — in exactly the
--   @{ seed = \<decimal string\>, version = \<int\> }@ shape
--   'worldGetLanguageProvenanceFn' returns and @world.init@ accepts, for
--   the same reason: a language seed is unsigned 64-bit and neither of
--   Lua's numeric types carries the top of that range intact.
--
--   Rerolling means calling again with the NEXT ordinal. Consecutive
--   ordinals are guaranteed a different meaning in the SAME language
--   (see "Language.Suggest"), which is the behavior the dice button
--   exists to show.
--
--   **The supported ordinal range is 0 through 10,000 inclusive**
--   ('maxSuggestNameOrdinal'). An omitted, non-numeric, or negative
--   ordinal still normalizes to 0; one ABOVE the maximum is refused
--   outright, with @nil@ plus a message naming that maximum. The bound
--   exists because the reroll chain is a replay: "Language.Suggest"'s
--   @headIndexAt@ walks from ordinal zero one step at a time, which is
--   what makes each reroll provably differ from its predecessor (#1106
--   requirement 2) and also makes the cost Θ(ordinal). Since this runs
--   SYNCHRONOUSLY on the shared Lua thread — the one serving the UI
--   scripts in a graphical session (#1106 requirement 8) — a
--   caller-sized ordinal would monopolize it, so the domain is bounded
--   rather than the walk made sublinear (#1272). 10,000 is 250 times
--   the longest reroll sequence anything exercises and still completes
--   in integer-only replay well inside one interactive frame's budget.
--
--   A failure — an out-of-range ordinal, no catalogue on disk, a
--   malformed one, an unconstructible generator version — returns @nil@
--   plus a descriptive message and changes nothing. There is
--   deliberately no fallback name: the dummy word-list generator this
--   replaced produced text with no language behind it, and quietly
--   resurrecting that behavior is what #1106 requirement 7 forbids.
worldSuggestNameFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
worldSuggestNameFn backendState = do
    seedArg ← Lua.tointeger 1
    ordArg  ← Lua.tointeger 2
    let seed   = maybe 0 fromIntegral seedArg ∷ Word64
        rawOrd = maybe 0 id ordArg ∷ Lua.Integer
    -- Bound-check the RAW Lua integer, before any narrowing conversion
    -- and before 'resolveSuggestion' — which reads the catalogue and
    -- writes 'lbsLanguageCache'. A refused ordinal must do neither, so
    -- it costs nothing and leaves nothing behind.
    if rawOrd > fromIntegral maxSuggestNameOrdinal
      then do
        Lua.pushnil
        Lua.pushstring (TE.encodeUtf8 (ordinalOutOfRangeText rawOrd))
        return 2
      else do
        let ordinal = max 0 (fromIntegral rawOrd) ∷ Int
        result ← Lua.liftIO $ resolveSuggestion backendState seed ordinal
        case result of
            Right sug → do
                Lua.newtable
                Lua.pushstring (TE.encodeUtf8 (nsName sug))
                Lua.setfield (-2) "name"
                Lua.pushstring (TE.encodeUtf8 (nsGloss sug))
                Lua.setfield (-2) "gloss"
                Lua.pushstring (TE.encodeUtf8 (encodeNameExpr (nsExpr sug)))
                Lua.setfield (-2) "expr"
                Lua.newtable
                Lua.pushstring (TE.encodeUtf8 (langSeedText (nsSeed sug)))
                Lua.setfield (-2) "seed"
                Lua.pushinteger
                    (fromIntegral (generatorVersionInt (nsVersion sug)))
                Lua.setfield (-2) "version"
                Lua.setfield (-2) "language"
                return 1
            Left msg → do
                Lua.pushnil
                Lua.pushstring (TE.encodeUtf8 msg)
                return 2

-- | The largest reroll ordinal @world.suggestName@ accepts, inclusive.
--
--   Part of that function's public contract — see its documentation for
--   why the domain is bounded at all. The value is stated there too, so
--   move both together.
maxSuggestNameOrdinal ∷ Int
maxSuggestNameOrdinal = 10000

-- | An over-bound ordinal's rejection message. Names the offending
--   value AND the maximum, because a caller that guessed the domain
--   wrong needs to know where it actually ends.
ordinalOutOfRangeText ∷ Lua.Integer → Text
ordinalOutOfRangeText raw = T.concat
    [ "world.suggestName: reroll ordinal "
    , tshow (toInteger raw)
    , " is out of range; the maximum is "
    , tshow maxSuggestNameOrdinal
    , "." ]

-- | world.generatedNameCharacters() → string
--
--   Every character a generated name can ever contain
--   ('Language.Generated.Orthography.outputInventory'), as one UTF-8
--   string: ASCII letters in both cases, #1100's extended letters in
--   both cases, and the two marks a rendered name may carry (the
--   possessive apostrophe and a hyphen-joining language's separator).
--
--   Exists so a text field holding a world name can accept exactly what
--   the generator can produce. #1106 requirement 4 turns on a player
--   being able to TYPE over a suggestion, which they cannot do if the
--   field rejects the very letters it was just filled with — and a
--   hand-written character class in Lua would drift from the generator
--   the first time the repertoire moved. This is the generator's own
--   answer, so the two cannot disagree.
worldGeneratedNameCharactersFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
worldGeneratedNameCharactersFn = do
    Lua.pushstring (TE.encodeUtf8 (T.pack outputInventory))
    return 1

-- | What one suggestion request must do, given 'lbsLanguageCache' as it
--   stands. Split out from the IO around it because this is exactly
--   where a press can turn back into filesystem work: everything except
--   'StepReadCatalogue' must complete without touching the disk.
data SuggestionStep
    = StepReadCatalogue
      -- ^ Nothing cached yet — the one read a session ever needs.
    | StepFailed !Text
      -- ^ A cached catalogue failure, reported again as-is.
    | StepBuild !Catalogue
      -- ^ Catalogue cached, but the language is a different one.
    | StepReuse !NameSuggester
      -- ^ Cached suggester for exactly this language.

suggestionStep ∷ LanguageProvenance → Maybe LanguageCache → SuggestionStep
suggestionStep _ Nothing = StepReadCatalogue
suggestionStep prov (Just lc) = case lcCatalogue lc of
    Left msg  → StepFailed msg
    Right cat → case lcSuggester lc of
        Just (p, sgr) | p ≡ prov → StepReuse sgr
        _                        → StepBuild cat

-- | A step as report text. 'SuggestionStep' carries a 'NameSuggester'
--   and a 'Catalogue', neither worth an 'Eq' instance of its own, so
--   this is what a test compares.
suggestionStepLabel ∷ SuggestionStep → Text
suggestionStepLabel s = case s of
    StepReadCatalogue → "read"
    StepFailed _      → "failed"
    StepBuild _       → "build"
    StepReuse _       → "reuse"

-- | Resolve one suggestion, reusing 'lbsLanguageCache' wherever it
--   still applies (#1106 requirement 8: the dice button runs
--   synchronously on the UI's own thread, so a press must not re-read
--   and re-parse @data/language/concepts.yaml@ or re-derive 150 concept
--   roots).
--
--   Rerolling within one seed reuses both levels. Editing the seed is a
--   different language, so the suggester is rebuilt — but from the
--   CACHED catalogue, with no filesystem access at all. Exactly one
--   suggestion per session reads the file, whether that read SUCCEEDS
--   or FAILS: a failure is cached and reported from the cache
--   thereafter, so a broken installation costs one read rather than one
--   per press (see 'LanguageCache' for why that is sticky).
--
--   A profile-construction failure caches the catalogue it did resolve,
--   so an unconstructible generator version doesn't force a re-read
--   either.
resolveSuggestion
    ∷ LuaBackendState → Word64 → Int → IO (Either Text NameSuggestion)
resolveSuggestion backendState seed ordinal = do
    cached ← readIORef (lbsLanguageCache backendState)
    case suggestionStep prov cached of
        StepReuse sgr  → pure (render sgr)
        StepFailed msg → pure (Left msg)
        StepBuild cat  → build cat
        StepReadCatalogue → do
            eCat ← readCatalogueForSuggestions conceptCataloguePath
                                               conceptOrdinalPath
            case eCat of
                Left msg → do
                    writeIORef (lbsLanguageCache backendState)
                        (Just (LanguageCache (Left msg) Nothing))
                    pure (Left msg)
                Right cat → build cat
  where
    prov = LanguageProvenance
        { lpSeed    = worldLanguageSeed seed
        , lpVersion = currentGeneratorVersion
        }

    build ∷ Catalogue → IO (Either Text NameSuggestion)
    build cat = case mkNameSuggester cat prov of
        Left sErr → do
            writeIORef (lbsLanguageCache backendState)
                (Just (LanguageCache (Right cat) Nothing))
            pure (Left (suggestErrorText sErr))
        Right sgr → do
            writeIORef (lbsLanguageCache backendState)
                (Just (LanguageCache (Right cat) (Just (prov, sgr))))
            pure (render sgr)

    render sgr = case suggestNameAt sgr ordinal of
        Left sErr → Left (suggestErrorText sErr)
        Right sug → Right sug

-- | The catalogue read behind a suggestion, with BOTH of its failure
--   modes turned into one descriptive 'Left'.
--
--   'loadCatalogue' only returns 'Left' for a file it could PARSE and
--   reject; a missing or unreadable one throws out of 'BS.readFile'
--   instead. Letting that propagate would skip the cache write, so the
--   very case the cache exists for — a broken installation — would go
--   back to a filesystem read per dice press (#1106 requirement 8).
--
--   'evaluate' forces the decode inside the handler's scope, so the
--   result is a settled 'Either' rather than a thunk that could still
--   fault after the caller has cached it. Only 'IOException' is caught:
--   an async exception delivered to this thread is not a catalogue
--   problem and must not be recorded as one.
readCatalogueForSuggestions ∷ FilePath → FilePath → IO (Either Text Catalogue)
readCatalogueForSuggestions catPath ordPath = do
    eRead ← try (loadCatalogue catPath ordPath ⌦ evaluate)
    pure $ case eRead of
        Left (ioErr ∷ IOException) → Left (describe (tshow ioErr))
        Right (Left cErr)          → Left (describe (catalogueErrorText cErr))
        Right (Right cat)          → Right cat
  where
    describe why = "concept catalogue " <> T.pack catPath
        <> " could not be loaded, so no name can be suggested: " <> why

-- | world.initArena(pageId) — create flat test arena, no geology
worldInitArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8Lenient bs)
            Nothing → WorldPageId "test_arena"    -- default when called with no args
    -- A re-init REPLACES an existing page's state, so it counts as a
    -- selection change too (#1602).
    Lua.liftIO $ enqueueSelectionChange env (WorldInitArena pageId)
    return 0

-- | world.initArenaDone(pageId) — signal that all arena textures have been sent
worldInitArenaDoneFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaDoneFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8Lenient bs)
            Nothing → WorldPageId "test_arena"
    -- #1602: its handler PREPENDS the page to wmVisible, so this is a
    -- selection change like any other and must be visible to the
    -- synchronous binding check while it sits unapplied.
    Lua.liftIO $ enqueueSelectionChange env (WorldInitArenaDone pageId)
    return 0

-- | world.openArena() — convenience function that broadcasts to Lua
worldOpenArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldOpenArenaFn env = do
    Lua.liftIO $ Q.writeQueue (luaQueue env) (LuaOpenArena)
    return 0

-- | Enqueue a world command that will CHANGE PAGE SELECTION, marking the
--   request pending in the same step (#1602). Every such request must go
--   through here: a placement binding cannot be trusted while a
--   selection change is in flight, and the count is the only thing that
--   makes an unapplied command visible to the synchronous check
--   @building.canPlaceAt@ / @construction.designate@ run on this thread.
enqueueSelectionChange ∷ EngineEnv → WorldCommand → IO ()
enqueueSelectionChange env cmd = do
    atomicModifyIORef' (wsWorldManagerRef (toWorldSimCapability env)) $ \mgr →
        let (effective, projected) = selectionRequestEffect cmd mgr
        in (requestSelectionChange effective projected mgr, ())
    Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) cmd

-- | Will this request actually MOVE the selection, and what does the
--   projected visible list become (#1602)?
--
--   Only an EFFECTIVE request may invalidate a live placement binding:
--   showing a page that is already visible, or hiding one that is
--   already hidden, is ordinary traffic that changes nothing a placement
--   depends on, and refusing clicks for it would regress the
--   no-page-switch path.
--
--   Judged against the PROJECTED list, never the applied one. The two
--   differ exactly when it matters: @world.show B@ followed by
--   @world.hide B@ leaves the applied list untouched at the moment the
--   hide is requested, so reading it would call that hide a no-op — and
--   once the show alone had been applied the projection would look
--   settled again while a real change was still queued, which is the
--   window a placement would be accepted in and then dropped at the
--   commit.
--
--   The test is whether the visible HEAD moves, not whether the visible
--   SET does. 'wmVisible' is a list and several pages can be visible at
--   once, but a placement binding only ever names its head — that is
--   what 'resolveActiveWorld' answers with and what @world.pickTile@
--   hit-tests, and @building.canPlaceAt@ refuses outright when nothing
--   is visible.
--
--   Each case below mirrors ITS HANDLER's own precondition, against the
--   PROJECTED state rather than the applied one, so a request that the
--   handler will turn into a no-op is predicted as one:
--
--     * a @world.show@ of a page that is already the head, or that is
--       not registered at all (the handler refuses those) — but a
--       queued @world.init@ ahead of it makes the same show real, which
--       is why registration is projected too;
--     * a @world.hide@ or @world.destroy@ of a page that is hidden,
--       absent, or visible-but-not-head;
--     * a @world.init@ / @world.initArena@ replacing anything but the
--       head;
--     * a @world.destroyAll@ with nothing visible to lose.
--
--   Over-predicting only ever costs a click, and never a wrong commit —
--   but it costs one on the no-page-switch path, which is the whole
--   point of predicting rather than blanket-invalidating.
--
--   A load publish stays effective unconditionally: it replaces the
--   whole session, and it is never ordinary traffic during placement.
selectionRequestEffect
    ∷ WorldCommand → WorldManager → (Bool, ([WorldPageId], [WorldPageId]))
selectionRequestEffect cmd mgr = case cmd of
    -- handleWorldShowCommand refuses an unregistered page outright.
    WorldShow pid
        | pid `notElem` worlds → unchanged
        | otherwise            → visibility pid True
    -- handleWorldInitArenaDoneCommand has no such registration check.
    WorldInitArenaDone pid     → visibility pid True
    WorldHide pid              → visibility pid False
    WorldDestroy pid           → register (filter (≢ pid) worlds)
                                          (visibility pid False)
    WorldDestroyAll            → (isJust (selectionHead visible), ([], []))
    -- These REPLACE a page's state without touching the visible list,
    -- so they matter exactly when the page being replaced is the head.
    -- They also REGISTER it, which a later queued show depends on.
    WorldInit pid _ _ _ _      → registering pid
    WorldInitArena pid         → registering pid
    WorldLoadPublish{}         → (True, ([], []))
    _                          → unchanged
  where
    (worlds, visible) = projectedVisible mgr
    unchanged = (False, (worlds, visible))
    registering pid =
        ( selectionHead visible ≡ Just pid
        , (if pid `elem` worlds then worlds else pid : worlds, visible) )
    visibility pid shown =
        let after = projectSelectionVisible pid shown visible
        in ( selectionHead after ≢ selectionHead visible
           , (worlds, after) )
    register worlds' (effective, (_, visible')) =
        (effective, (worlds', visible'))

-- | world.show(pageId)
worldShowFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldShowFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            enqueueSelectionChange env (WorldShow pageId)
        Nothing → pure ()

    return 0

-- | world.hide(pageId)
worldHideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHideFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            enqueueSelectionChange env (WorldHide pageId)
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
            enqueueSelectionChange env (WorldDestroy pageId)
        Nothing → pure ()

    return 0

-- | world.destroyAll() — tear down every world (Exit to Menu). Clears
--   wmWorlds/wmVisible (so no hidden world resolves as the implicit active
--   world behind the menu), sim-deactivates each, and resets the global
--   unit/building managers. (#58)
worldDestroyAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDestroyAllFn env = do
    Lua.liftIO $ enqueueSelectionChange env WorldDestroyAll
    return 0
