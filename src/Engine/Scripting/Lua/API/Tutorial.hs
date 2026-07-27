{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the tutorial definition tree (#957, phase 1 of the
--   tutorial epic #956): one loader verb and one READ-ONLY query.
--
--   Narrowed to the @content-registries@ capability (#890's convention,
--   epic #537): the tutorial registry is reached only through
--   'ContentRegistriesCapability' and the logger only through
--   'CoreCapability', so this module never touches an 'EngineEnv'.
--
--   The query is the surface requirement 6 names: it hands Lua the
--   ALREADY-VALIDATED tree in its deterministic display order, so the
--   later tutorial runtime evaluates the declared keys without
--   duplicating, mutating, or re-inferring YAML structure. Nothing here
--   writes tutorial state — this slice has none.
module Engine.Scripting.Lua.API.Tutorial
  ( loadTutorialYamlFn
  , getTutorialTreeFn
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
  (ContentRegistriesCapability(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Asset.YamlTutorials (loadTutorialYaml)
import Tutorial.Types

-- | engine.loadTutorialYaml(path) — parses, validates, and publishes
--   one tutorial tree file, returning 1 on success and 0 on failure.
--
--   A file holds one WHOLE tree, so this is all-or-nothing: on any
--   parse or validation failure nothing is published (the registry
--   keeps whatever it already had, which at boot is the explicit empty
--   state) and 'loadTutorialYaml' has already logged an actionable
--   error naming the file and the offending objective. Boot never
--   aborts on a bad tutorial file — the game simply comes up without
--   onboarding, loudly.
loadTutorialYamlFn ∷ CoreCapability → ContentRegistriesCapability
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadTutorialYamlFn core regs = do
  pathArg ← Lua.tostring 1
  case pathArg of
    Nothing → do
      Lua.pushnumber 0
      return 1
    Just pathBS → do
      let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
      count ← Lua.liftIO $ do
        logger ← getLoggerFor core
        mTree ← loadTutorialYaml logger filePath
        case mTree of
          Nothing → return (0 ∷ Int)
          Just tree → do
            writeIORef (crTutorialRegistryRef regs)
                       (singleTutorialRegistry tree)
            logInfo logger CatAsset $
              "loadTutorialYaml: loaded tutorial tree '" <> ttId tree
              <> "' from " <> T.pack filePath
            return 1
      Lua.pushnumber (Lua.Number (fromIntegral count))
      return 1

-- | engine.getTutorialTree() → the active tutorial tree, or nil when
--   none is loaded (no tutorial file, or one that failed validation —
--   a partial tree is never published, so nil is unambiguous).
--
--   Shape:
--
--   > { id = "first_session", root = <node> }
--
--   where each @\<node\>@ is
--
--   > { id, kind, label, tooltip, evaluator, order,
--   >   children      = { <node>, … },
--   >   subobjectives = { <node>, … } }
--
--   Both relationship arrays are ALWAYS present (empty when unused), so
--   a consumer never has to distinguish nil from empty. Their order is
--   the validated display order — @order@ within a sibling group, ties
--   broken by id — and at most one of the two is ever non-empty.
getTutorialTreeFn ∷ ContentRegistriesCapability
                  → Lua.LuaE Lua.Exception Lua.NumResults
getTutorialTreeFn regs = do
  reg ← Lua.liftIO $ readIORef (crTutorialRegistryRef regs)
  case activeTutorialTree reg of
    Nothing → Lua.pushnil >> return 1
    Just tree → do
      Lua.newtable
      Lua.pushstring (TE.encodeUtf8 (ttId tree))
      Lua.setfield (-2) "id"
      pushNode (ttRoot tree)
      Lua.setfield (-2) "root"
      return 1

-- | Push one node table onto the stack.
pushNode ∷ TutorialNode → Lua.LuaE Lua.Exception ()
pushNode node = do
  let obj = tnObjective node
  Lua.newtable
  Lua.pushstring (TE.encodeUtf8 (toId obj))
  Lua.setfield (-2) "id"
  Lua.pushstring (TE.encodeUtf8 (objectiveKindText (toKind obj)))
  Lua.setfield (-2) "kind"
  Lua.pushstring (TE.encodeUtf8 (toLabel obj))
  Lua.setfield (-2) "label"
  Lua.pushstring (TE.encodeUtf8 (toTooltip obj))
  Lua.setfield (-2) "tooltip"
  Lua.pushstring (TE.encodeUtf8 (toEvaluator obj))
  Lua.setfield (-2) "evaluator"
  Lua.pushinteger (fromIntegral (toOrder obj))
  Lua.setfield (-2) "order"
  pushNodeArray (tnChildren node)
  Lua.setfield (-2) "children"
  pushNodeArray (tnSubobjectives node)
  Lua.setfield (-2) "subobjectives"

-- | Push an ordered array of node tables.
pushNodeArray ∷ [TutorialNode] → Lua.LuaE Lua.Exception ()
pushNodeArray nodes = do
  Lua.newtable
  forM_ (zip [1..] nodes) $ \(i, n) → do
    pushNode n
    Lua.rawseti (-2) i
