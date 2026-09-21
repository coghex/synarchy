{-# LANGUAGE Strict #-}
-- | Lua surface for the faction-tag catalogue (#2506, FTS-2 of the
--   faction-tag arc #2496): the @engine.loadFactionYaml@ populator, and
--   nothing else.
--
--   __One verb, deliberately.__ Requirement 9 permits a loader binding
--   for the startup queue and no faction QUERY verb: reading tags,
--   relations or profiles from Lua is FTS-6's, and shipping a query here
--   would give script authors an API to depend on before the live model
--   that should answer it exists.
--
--   The catalogue registry is this module's to WRITE, so it arrives
--   through 'ContentRegistriesCapability' exactly as its loot-table and
--   loot-profile siblings do. The logger comes through
--   'CoreCapability'. This module never touches an 'EngineEnv'.
module Engine.Scripting.Lua.API.Factions
    ( loadFactionYamlFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Log (LogCategory(..), logDebug, logError)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Scripting.Lua.API.YamlResult
    (YamlRefusal(..), pushYamlRefusal, pushYamlResult)
import Engine.Asset.YamlFactions
    ( admitFactionYamlDoc, catalogueIntegrityRefusal
    , loadFactionYamlOutcome, refusalDetail, refusalReason
    , scanFactionTagVocabulary )
import Unit.Faction.Catalogue (extendFactionCatalogue, withoutSource)

-- | @engine.loadFactionYaml(path)@ — parse one @data\/factions\/@ file
--   and register its tag declarations and base relations.
--
--   Answers the number of DECLARATIONS registered (tags plus relations),
--   so a file that declares only relations still reports what it
--   contributed rather than reading as empty.
--
--   __The #2203 outcome contract, plus #2241's refusal.__ A bare call
--   answers one number; a truthy SECOND argument opts in to
--   @(count, parsed)@:
--
--     * a decode failure answers @(0, false)@ —
--       @scripts\/startup_loader.lua@ turns that into the terminal
--       startup failure it turns every family's parse failure into;
--     * a successful registration answers @(count, true)@;
--     * a SEMANTIC refusal (requirement 2's rules) answers
--       @(0, true, detail, reason)@ and registers NOTHING. The file
--       decoded, so it is not a parse failure and must not be reported
--       as one; it is terminal all the same, because continuing would
--       reach the main menu with a catalogue the author believes is
--       loaded and every unit file checked against a table that is
--       missing rows.
--
--   __Validated against the live registry, then written in one step.__
--   The read and the write are not atomic, and do not need to be: the
--   startup queue is the only caller and it runs one file at a time on
--   the Lua thread. What DOES matter is that nothing is written until
--   the whole document is admitted, which is why 'admitFactionYamlDoc'
--   answers the validated declarations rather than a yes\/no.
--
--   __Repeatable on one path.__ This verb stays publicly callable like
--   every other @engine.load*Yaml@, so it judges the file against the
--   catalogue MINUS whatever that same path contributed before
--   ('withoutSource'). Re-reading one file therefore replaces its own
--   declarations; two different files colliding is still a refusal. The
--   write is then gated on the COMPLETE proposed catalogue
--   ('catalogueIntegrityRefusal'), because a replacement document can
--   be faultless in itself and still leave another file's relation
--   naming a tag nothing declares any more. On that refusal nothing is
--   written, so the previously registered catalogue survives intact.
--
--   __Order-independent.__ Relation endpoints resolve against the whole
--   directory's declared vocabulary ('scanFactionTagVocabulary'), not
--   against what happens to be registered, so a relations-only file is
--   admitted whether it is enumerated before or after the file
--   declaring its tags.
loadFactionYamlFn ∷ CoreCapability → ContentRegistriesCapability
                  → Lua.LuaE Lua.Exception Lua.NumResults
loadFactionYamlFn core regs = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            outcome ← Lua.liftIO $ do
                logger ← getLoggerFor core
                mDoc ← loadFactionYamlOutcome logger filePath
                case mDoc of
                    Nothing → do
                        logDebug logger CatAsset $
                            "loadFactionYaml: loaded 0 faction declarations \
                            \from " <> T.pack filePath
                        return (Right (False, 0 ∷ Int))
                    Just doc → do
                        registered ← readIORef (crFactionCatalogueRef regs)
                        -- The directory's whole declared vocabulary,
                        -- staged BEFORE admission so a relation naming a
                        -- sibling's tag is admitted whether that sibling
                        -- has loaded yet or not.
                        vocab ← scanFactionTagVocabulary filePath
                        -- Judge this file against the REST of the
                        -- directory: re-reading one path replaces what
                        -- that path said rather than colliding with it,
                        -- which is what keeps this verb as repeatable
                        -- as its nine siblings.
                        let cat = withoutSource filePath registered
                            admitted = do
                                (decls, entries) ← admitFactionYamlDoc
                                                       vocab cat doc
                                let proposed = extendFactionCatalogue
                                                   filePath decls entries cat
                                -- The write is gated on the COMPLETE
                                -- proposed catalogue, not just on this
                                -- document: a replacement that drops a
                                -- tag another file's relation names is
                                -- a faultless document and a broken
                                -- registry.
                                maybe (Right (decls, entries, proposed)) Left
                                      (catalogueIntegrityRefusal vocab proposed)
                        case admitted of
                            Left refusal → do
                                logError logger CatAsset $
                                    "loadFactionYaml: refused "
                                    <> T.pack filePath <> " entirely: "
                                    <> refusalReason refusal <> " '"
                                    <> refusalDetail refusal <> "'"
                                return (Left (YamlRefusal
                                    (Just (refusalReason refusal))
                                    (refusalDetail refusal)))
                            Right (decls, entries, proposed) → do
                                -- Nothing was written on the refusal
                                -- branch, so a refused reload leaves the
                                -- previously registered catalogue —
                                -- `registered`, not `cat` — exactly as
                                -- it was.
                                writeIORef (crFactionCatalogueRef regs) proposed
                                let n = length decls + length entries
                                logDebug logger CatAsset $
                                    "loadFactionYaml: loaded " <> tshow n
                                    <> " faction declarations ["
                                    <> tshow (length decls) <> " tags, "
                                    <> tshow (length entries)
                                    <> " relations] from " <> T.pack filePath
                                return (Right (True, n))
            case outcome of
                Left refusal        → pushYamlRefusal refusal
                Right (parsed, cnt) → pushYamlResult parsed cnt
