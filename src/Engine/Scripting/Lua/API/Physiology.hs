{-# LANGUAGE Strict #-}
-- | Lua surface for the physiology system registry (#2828, S2 of epic
--   #2827): the @engine.loadPhysiologyYaml@ populator and the
--   non-mutating @engine.getPhysiologyRegistry@ accessor, and nothing
--   else. The per-unit read verbs live in
--   "Engine.Scripting.Lua.API.Units.Physiology".
--
--   The registry is this module's to WRITE, so it arrives through
--   'ContentRegistriesCapability' exactly as its faction-catalogue
--   sibling does. The logger comes through 'CoreCapability'. This module
--   never touches an 'EngineEnv'.
module Engine.Scripting.Lua.API.Physiology
    ( loadPhysiologyYamlFn
    , getPhysiologyRegistryFn
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
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
import Engine.Asset.YamlPhysiology
    ( PhysiologyRefusal(..), admitPhysiologyDoc, loadPhysiologyYamlOutcome )
import Unit.Physiology.Types

-- | @engine.loadPhysiologyYaml(path)@ — parse one @data\/physiology\/@
--   file and register its systems, constants and profiles.
--
--   Answers the number of SYSTEMS registered. Same #2203 outcome
--   contract as every @engine.load*Yaml@: a bare call answers one number;
--   a truthy second argument opts in to @(count, parsed)@, and a
--   SEMANTIC refusal answers @(0, true, detail, reason)@ and registers
--   NOTHING — the file decoded, so it is not a parse failure, but it is
--   terminal at startup because continuing would reach the main menu
--   with unit definitions about to be checked against a registry that is
--   not there.
--
--   __One registry.__ The registry is one document: system ids and keys
--   are a single namespace, so a second file is not merged into the
--   first. Re-reading the SAME path replaces the registry; a DIFFERENT
--   path while one is registered is refused. A refused call leaves the
--   previously registered registry intact.
loadPhysiologyYamlFn ∷ CoreCapability → ContentRegistriesCapability
                     → Lua.LuaE Lua.Exception Lua.NumResults
loadPhysiologyYamlFn core regs = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            outcome ← Lua.liftIO $ do
                logger ← getLoggerFor core
                mDoc ← loadPhysiologyYamlOutcome logger filePath
                case mDoc of
                    Nothing → do
                        logDebug logger CatAsset $
                            "loadPhysiologyYaml: loaded 0 physiology systems \
                            \from " <> T.pack filePath
                        return (Right (False, 0 ∷ Int))
                    Just doc → do
                        registered ← readIORef (crPhysiologyRegistryRef regs)
                        let refusal r = do
                                logError logger CatAsset $
                                    "loadPhysiologyYaml: refused "
                                    <> T.pack filePath <> " entirely: "
                                    <> refusalReason r <> " '"
                                    <> refusalDetail r <> "'"
                                return (Left (YamlRefusal
                                    (Just (refusalReason r)) (refusalDetail r)))
                        case (prSource registered, admitPhysiologyDoc filePath doc) of
                            (_, Left r) → refusal r
                            (Just other, Right _)
                                | other ≢ filePath →
                                    refusal PhysiologyRefusal
                                        { refusalReason =
                                            "duplicate physiology registry"
                                        , refusalDetail = T.pack other }
                            (_, Right reg) → do
                                writeIORef (crPhysiologyRegistryRef regs) reg
                                let n = length (prSystems reg)
                                logDebug logger CatAsset $
                                    "loadPhysiologyYaml: loaded " <> tshow n
                                    <> " physiology systems ["
                                    <> tshow (length (prProfiles reg))
                                    <> " profiles] from " <> T.pack filePath
                                return (Right (True, n))
            case outcome of
                Left refusal        → pushYamlRefusal refusal
                Right (parsed, cnt) → pushYamlResult parsed cnt

-- | @engine.getPhysiologyRegistry()@ — the loaded registry as a table,
--   or @nil@ when none is loaded. Strictly a READ: the table is a copy,
--   and no mutating entry point exists.
--
--   @{ source, constants = {NAME = number}, systemOrder = {id, ..},
--   systems = {id = {id, requires, owns, derived, meters, provides,
--   contributes, effects}}, profiles = {id = {system = "nominal" |
--   "simulated"}} }@. Healthy rules and substitutes are described by
--   @kind@ (@"const"@ \/ @"from"@, @"value"@ \/ @"none"@ \/
--   @"virtual_meal"@ \/ @"tissue_recovery"@), with every constant already
--   resolved to a number (@value@, @scale@) beside the names it came from
--   (@ref@, @scaleRefs@), so a Lua module reads @SALT_PER_KG@ from here
--   rather than carrying its own copy.
getPhysiologyRegistryFn ∷ ContentRegistriesCapability
                        → Lua.LuaE Lua.Exception Lua.NumResults
getPhysiologyRegistryFn regs = do
    reg ← Lua.liftIO (readIORef (crPhysiologyRegistryRef regs))
    if not (physiologyRegistryLoaded reg)
        then Lua.pushnil
        else pushRegistry reg
    return 1

pushRegistry ∷ PhysiologyRegistry → Lua.LuaE Lua.Exception ()
pushRegistry reg = do
    Lua.newtable
    forM_ (prSource reg) $ \p → field "source" (Lua.pushstring (TE.encodeUtf8 (T.pack p)))
    field "constants" $ do
        Lua.newtable
        forM_ (Map.toList (prConstants reg)) $ \(n, v) →
            field n (pushNum v)
    field "systemOrder" $ pushArray (map psId (prSystems reg)) pushText
    field "systems" $ do
        Lua.newtable
        forM_ (prSystems reg) $ \s → field (psId s) (pushSystem reg s)
    field "profiles" $ do
        Lua.newtable
        forM_ (prProfiles reg) $ \p → field (ppId p) $ do
            Lua.newtable
            forM_ (Map.toList (ppModes p)) $ \(sid, m) →
                field sid (pushText (physioModeText m))

pushSystem ∷ PhysiologyRegistry → PhysiologySystem → Lua.LuaE Lua.Exception ()
pushSystem reg s = do
    Lua.newtable
    field "id" (pushText (psId s))
    field "requires" (pushArray (psRequires s) pushText)
    field "owns" $ pushArray (psOwns s) $ \o → do
        Lua.newtable
        field "key" (pushText (okKey o))
        field "healthy" (pushHealthy reg (okHealthy o))
    field "derived" $ pushArray (psDerived s) $ \d → do
        Lua.newtable
        field "key" (pushText (dkKey d))
        field "rule" (pushText (derivedRuleText (dkRule d)))
        field "healthy" (pushNum (dkHealthy d))
    field "meters" (pushArray (psMeters s) pushText)
    field "provides" (pushEntries reg (psProvides s))
    field "contributes" (pushEntries reg (psContributes s))
    field "effects" (pushEntries reg (psEffects s))

pushHealthy ∷ PhysiologyRegistry → HealthyRule → Lua.LuaE Lua.Exception ()
pushHealthy reg rule = do
    Lua.newtable
    case rule of
        HealthyConst q → do
            field "kind" (pushText "const")
            pushQuantity "value" "ref" q
        HealthyFrom k qs → do
            field "kind" (pushText "from")
            field "from" (pushText k)
            field "scale" (pushNum (product (map resolved qs)))
            field "scaleRefs" (pushArray qs (pushText ∘ quantityRef))
  where
    resolved q = fromMaybe 0 (resolveQuantity' q)
    resolveQuantity' (QLit f)   = Just f
    resolveQuantity' (QConst n) = Map.lookup n (prConstants reg)
    pushQuantity valueKey refKey q = do
        field valueKey (pushNum (resolved q))
        case q of
            QConst n → field refKey (pushText n)
            QLit _   → pure ()

pushEntries ∷ PhysiologyRegistry → [InterfaceEntry]
            → Lua.LuaE Lua.Exception ()
pushEntries reg es = pushArray es $ \e → do
    Lua.newtable
    field "name" (pushText (ieName e))
    field "substitute" $ do
        Lua.newtable
        case ieSubstitute e of
            SubValue q → do
                field "kind" (pushText "value")
                field "value" (pushNum (fromMaybe 0 (resolve q)))
                case q of
                    QConst n → field "ref" (pushText n)
                    QLit _   → pure ()
            SubNone           → field "kind" (pushText "none")
            SubVirtualMeal    → field "kind" (pushText "virtual_meal")
            SubTissueRecovery → field "kind" (pushText "tissue_recovery")
  where
    resolve (QLit f)   = Just f
    resolve (QConst n) = Map.lookup n (prConstants reg)

quantityRef ∷ Quantity → Text
quantityRef (QConst n) = n
quantityRef (QLit f)   = tshow f

-- * Table helpers

-- | Set @name@ on the table on top of the stack to whatever the action
--   pushes.
field ∷ Text → Lua.LuaE Lua.Exception () → Lua.LuaE Lua.Exception ()
field name push = do
    push
    Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 name))

pushText ∷ Text → Lua.LuaE Lua.Exception ()
pushText = Lua.pushstring ∘ TE.encodeUtf8

pushNum ∷ Float → Lua.LuaE Lua.Exception ()
pushNum = Lua.pushnumber ∘ Lua.Number ∘ realToFrac

-- | A 1-based Lua array.
pushArray ∷ [α] → (α → Lua.LuaE Lua.Exception ()) → Lua.LuaE Lua.Exception ()
pushArray xs push = do
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] xs) $ \(i, x) → do
        push x
        Lua.rawseti (-2) (fromIntegral i)
