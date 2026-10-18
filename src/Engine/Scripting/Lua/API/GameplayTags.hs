-- | The @gameplayTags@ Lua namespace (#2706, SCN-06 of epic #2698): the
--   script-facing surface over the live gameplay-tag owners.
--
--   Nothing here holds tag state. Every verb decodes its arguments
--   ("Engine.Scripting.Lua.API.GameplayTags.Args"), resolves the target
--   against the live page, and calls the owner's own operation:
--
--     [units, buildings] one atomic modify of the manager
--       ('Gameplay.Tags.Live.mutateUnitTags', 'mutateBuildingTags');
--     [locations] one atomic modify of the page's params
--       ('mutateLocationTags');
--     [physical items] the session index's guarded write
--       ('mutateItemTags');
--     [terrain cells, plants and plots] the world thread
--       ('Gameplay.Tags.Request.requestPageTagEdit').
--
--   The first three complete inside the call. The last QUEUES the edit
--   on the world thread, which revalidates the page's incarnation when
--   it applies it, and the verb waits for that answer: so for every
--   category a verb that returns @true@ has applied its edit, and a
--   subsequent read or ordinary save sees it. A page destroyed or
--   re-initialised in between, or a load that publishes first, refuses
--   the queued edit and leaves the replacement untouched.
--
--   Reads and queries capture ONE 'Gameplay.Tags.Live.LiveTagView' (the
--   prerequisites' consistency boundary) and evaluate the whole
--   expression natively. Returned tables are fresh values: editing one
--   changes nothing in the engine.
--
--   Every verb answers a malformed request or an unavailable target
--   with @nil, message@ and never raises, so a refusal is distinguishable
--   from an empty list, a zero or @false@.
module Engine.Scripting.Lua.API.GameplayTags
    ( -- * The owners a verb reaches
      TagApi(..)
      -- * The list-marshalling seam
    , TagMarshal(..)
    , productionTagMarshal
      -- * Verbs
    , tagAddFn
    , tagRemoveFn
    , tagReplaceFn
    , tagClearFn
    , tagTagsOfFn
    , tagHasFn
    , tagQueryFn
    , tagCountFn
    , tagExistsFn
    ) where

import UPrelude
import Data.Int (Int64)
import Data.IORef (IORef, readIORef)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Building.Types (BuildingId(..), BuildingManager)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Engine.Core.State (resolveActiveWorld)
import Engine.Scripting.Lua.API.GameplayTags.Args
import Gameplay.Tags.Live
import Gameplay.Tags.Query (QueryResult, TagQuery, targetSetsToList, targetSetsSize)
import Gameplay.Tags.Request (requestPageTagEditAt)
import Gameplay.Tags.Types
import Location.Instance (LocationInstanceId(..))
import Unit.Types.Manager (UnitId(..), UnitManager)
import Engine.Save.Barrier (SaveBarrier, SavePhase(..), SaveStatus(..), readSaveStatus)
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration, canonicalChunkCoord)
import World.Flora.Identity (floraInstanceIdToLua)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..), pageWrapWorldSize)

-- * The owners a verb reaches

-- | The owner handles and the save barrier, narrowed from the engine's
--   capabilities by the registrar.
data TagApi = TagApi
    { taUnits       ∷ IORef UnitManager
    , taBuildings   ∷ IORef BuildingManager
    , taWorld       ∷ WorldSimCapability
    , taSaveBarrier ∷ SaveBarrier
    , taAfterResolve ∷ IO ()
      -- ^ Runs between an edit's target resolution and its application:
      --   the test seam for a page replaced in exactly that window.
      --   Production passes @pure ()@.
    }

-- * The list-marshalling seam

-- | How a LIST-mode query pushes its targets: once per target, onto a
--   table the verb already created. @count@ and @exists@ are built from
--   the same query path and never call it, which the Hspec group
--   observes by substituting a counting seam.
newtype TagMarshal = TagMarshal
    { tmPushTarget ∷ TagTarget → Lua.LuaE Lua.Exception () }

productionTagMarshal ∷ TagMarshal
productionTagMarshal = TagMarshal pushTarget

-- | One target as a fresh table (see the reference for the encodings).
--   Page-scoped targets always carry their page explicitly, so a
--   returned target round-trips whatever page is active when it is
--   reused.
pushTarget ∷ TagTarget → Lua.LuaE Lua.Exception ()
pushTarget target = do
    Lua.newtable
    case target of
        TargetUnit (UnitId n) → kind "unit" *> int "id" (fromIntegral n)
        TargetBuilding (BuildingId n) → kind "building" *> int "id" (fromIntegral n)
        TargetItem i → kind "item" *> int "id" (fromIntegral (itemTargetId i))
        TargetLocation (LocationTarget p (LocationInstanceId n)) →
            kind "location" *> page p *> int "id" (fromIntegral n)
        TargetPlant pt → case (plantTargetId pt, plantTargetPlotTile pt) of
            (Just fid, _) → kind "plant" *> page (plantTargetPage pt)
                              *> int "id" (floraInstanceIdToLua fid)
            (_, Just (x, y)) → kind "plot" *> page (plantTargetPage pt)
                                 *> int "x" (fromIntegral x) *> int "y" (fromIntegral y)
            _ → kind "plant" *> page (plantTargetPage pt)
        TargetTile t → do
            kind "tile" *> page (tileTargetPage t)
            int "x" (fromIntegral (tileTargetX t))
            int "y" (fromIntegral (tileTargetY t))
            int "z" (fromIntegral (tileTargetZ t))
  where
    kind k = str "kind" k
    page (WorldPageId p) = str "page" p
    str ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
    str f v = Lua.pushstring (TE.encodeUtf8 v) *> Lua.setfield (-2) f
    int ∷ Lua.Name → Int64 → Lua.LuaE Lua.Exception ()
    int f v = Lua.pushinteger (Lua.Integer v) *> Lua.setfield (-2) f

-- * Running a verb

-- | Check the argument count, decode, act, and answer. The stack is
--   restored to the arguments after decoding whichever way it ends, and
--   every refusal is @nil, "gameplayTags.<verb>: <reason>"@.
verb ∷ Text → (Int, Int) → Dec α → (α → IO (Either Text r))
     → (r → Lua.LuaE Lua.Exception Lua.NumResults) → Lua.LuaE Lua.Exception Lua.NumResults
verb name (lo, hi) decode act answer = do
    top ← Lua.gettop
    let n = fromIntegral (Lua.fromStackIndex top)
    if n < lo ∨ n > hi
        then refusal ("expected " <> arity <> ", got " <> tshow n)
        else do
            decoded ← runDec decode
            Lua.settop top
            case decoded of
                Left err → refusal err
                Right a → Lua.liftIO (act a) ≫= either refusal answer
  where
    arity | lo ≡ hi   = tshow lo <> " argument" <> (if lo ≡ 1 then "" else "s")
          | otherwise = tshow lo <> " or " <> tshow hi <> " arguments"
    refusal err = do
        Lua.pushnil
        Lua.pushstring (TE.encodeUtf8 ("gameplayTags." <> name <> ": " <> err))
        pure 2

pushTrue ∷ () → Lua.LuaE Lua.Exception Lua.NumResults
pushTrue () = Lua.pushboolean True *> pure 1

-- * Resolving a target

-- | The page a request names, or the active page ('resolveActiveWorld',
--   the rule every page-defaulting world verb uses).
resolvePage ∷ WorldManager → Maybe WorldPageId → Either Text (WorldPageId, WorldState)
resolvePage wm = \case
    Nothing → maybe (Left "no active world page") Right (resolveActiveWorld wm)
    Just p  → maybe (Left ("no live page '" <> unWorldPageId p <> "'")) (Right . (p,))
                    (lookup p (wmWorlds wm))

-- | A resolved target, and for a page-owned one (cell, plant, plot) the
--   page and the incarnation it was resolved against.
data Resolved = Resolved !TagTarget !(Maybe (WorldPageId, ChunkGeneration))

resolvedTarget ∷ Resolved → TagTarget
resolvedTarget (Resolved t _) = t

-- | The typed target a reference names on the live page: coordinates
--   canonicalised exactly as each owner keys them (cells through the
--   page's arena-aware chunk canonicalisation, plots through the plot
--   map's wrap), so every seam alias names the one target.
--
--   A page-owned target's incarnation is read FIRST, before anything is
--   derived from the page, and travels with the target to the world
--   thread. A page replaced at any later point therefore refuses the
--   edit: the canonicalisation can never be applied to a page it was not
--   computed against.
resolveTarget ∷ TagApi → TargetRef → IO (Either Text Resolved)
resolveTarget api = \case
    RefUnit n → plain (TargetUnit (UnitId n))
    RefBuilding n → plain (TargetBuilding (BuildingId n))
    RefItem n → pure (maybe (Left "an item id must be positive") (Right . (`Resolved` Nothing) . TargetItem)
                            (mkItemTarget n))
    RefLocation mp lid → onPage' mp $ \p _ →
        plain (TargetLocation (LocationTarget p (LocationInstanceId lid)))
    RefPlant mp fid → pageOwned mp $ \p _ →
        pure (maybe (Left "not a flora occurrence id") (Right . TargetPlant) (mkPlantTarget p fid))
    RefPlot mp x y → pageOwned mp $ \p ws → do
        size ← pageWrapWorldSize ws
        pure (Right (TargetPlant (mkPlotTarget size p x y)))
    RefTile mp x y z → pageOwned mp $ \p ws → readIORef (wsGenParamsRef ws) ≫= \case
        Nothing → pure (Left ("page '" <> unWorldPageId p <> "' has no terrain"))
        Just params → pure (Right (TargetTile (mkTileTarget (canonicalChunkCoord params) p x y z)))
  where
    plain t = pure (Right (Resolved t Nothing))
    onPage' mp k = do
        wm ← readIORef (wsWorldManagerRef (taWorld api))
        either (pure . Left) (uncurry k) (resolvePage wm mp)
    pageOwned mp k = onPage' mp $ \p ws → do
        epoch ← pageIncarnation ws
        fmap (`Resolved` Just (p, epoch)) <$> k p ws

noTarget ∷ Text
noTarget = "no such live target (it does not exist, or is not loaded)"

-- * Mutation

-- | Refused while a save or load is capturing the session: from the
--   moment the transaction begins until its snapshot is captured. Lua
--   drives every save and load publish, and their Lua callbacks run
--   inside that window while the world thread is parked, so a tile or
--   plant edit waiting on the world thread would deadlock the
--   transaction, and any other edit would race the snapshot. Encoding,
--   after capture, is not refused. Lua is the only thread that begins a
--   transaction, so none can begin between this check and the edit.
captureRefusal ∷ Text
captureRefusal = "a save or load is capturing the session; edit again once it completes"

capturing ∷ SaveBarrier → IO Bool
capturing barrier = flip fmap (readSaveStatus barrier) $ \case
    Just s → ssOutcome s ≡ Nothing ∧ ssPhase s ∉ [SaveEncoding, SaveCaptureComplete, SaveFailed]
    Nothing → False

-- | Apply one edit through the target's own owner.
applyEdit ∷ TagApi → (TargetRef, TagEdit) → IO (Either Text ())
applyEdit api (ref, edit) = capturing (taSaveBarrier api) ≫= \case
  True → pure (Left captureRefusal)
  False → resolveTarget api ref ≫= \case
    Left err → pure (Left err)
    Right (Resolved target page) → do
        taAfterResolve api
        result ← case (target, page) of
            (TargetUnit u, _)     → mutateUnitTags (taUnits api) u edit
            (TargetBuilding b, _) → mutateBuildingTags (taBuildings api) b edit
            (TargetLocation l, _) → mutateLocationTags wmRef l edit
            (TargetItem i, _)     → mutateItemTags (taUnits api) (taBuildings api) wmRef Nothing i edit
            (_, Just (p, epoch))  → requestPageTagEditAt (taWorld api) p epoch target edit
            (_, Nothing)          → pure TagEditNoTarget
        pure $ case result of
            TagEditApplied  → Right ()
            TagEditNoTarget → Left noTarget
  where
    wmRef = wsWorldManagerRef (taWorld api)

editVerb ∷ Text → (Set.Set GameplayTag → TagEdit) → TagApi → Lua.LuaE Lua.Exception Lua.NumResults
editVerb name mk api = verb name (2, 2)
    ((,) <$> readTargetRef 1 <*> (mk ⊚ readTagSet 2)) (applyEdit api) pushTrue

-- | @gameplayTags.add(target, tags) → true | nil, err@
tagAddFn ∷ TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagAddFn = editVerb "add" AddTagsEdit

-- | @gameplayTags.remove(target, tags) → true | nil, err@
tagRemoveFn ∷ TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagRemoveFn = editVerb "remove" RemoveTagsEdit

-- | @gameplayTags.replace(target, tags) → true | nil, err@ — an empty
--   list clears.
tagReplaceFn ∷ TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagReplaceFn = editVerb "replace" ReplaceTagsEdit

-- | @gameplayTags.clear(target) → true | nil, err@
tagClearFn ∷ TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagClearFn api = verb "clear" (1, 1) ((, ClearTagsEdit) ⊚ readTargetRef 1) (applyEdit api) pushTrue

-- * Inspection

capture ∷ TagApi → IO LiveTagView
capture api = captureLiveTagView (taUnits api) (taBuildings api) (wsWorldManagerRef (taWorld api))

-- | A live target's tags in one view; 'Nothing' when it is not live.
targetTagsIn ∷ LiveTagView → TagTarget → Maybe (Set.Set GameplayTag)
targetTagsIn v = \case
    TargetUnit u     → unitTagsIn (ltvUnits v) u
    TargetBuilding b → buildingTagsIn (ltvBuildings v) b
    TargetLocation (LocationTarget p l) → lookup p (ltvLocations v) ≫= (`locationTagsIn` l)
    TargetItem i     → itemTagsIn v i
    TargetPlant p    → plantTagsIn v p
    TargetTile t     → tileTagsIn v t

inspect ∷ TagApi → TargetRef → IO (Either Text (Set.Set GameplayTag))
inspect api ref = resolveTarget api ref ≫= \case
    Left err → pure (Left err)
    Right (resolvedTarget → target) → do
        v ← capture api
        pure (maybe (Left noTarget) Right (targetTagsIn v target))

-- | @gameplayTags.tagsOf(target) → { tag, … } | nil, err@ — sorted by
--   code point; an untagged live target answers @{}@.
tagTagsOfFn ∷ TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagTagsOfFn api = verb "tagsOf" (1, 1) (readTargetRef 1) (inspect api) $ \ts → do
    Lua.newtable
    forM_ (zip [1 ..] (Set.toAscList ts)) $ \(i, t) → do
        Lua.pushstring (TE.encodeUtf8 (gameplayTagText t))
        Lua.rawseti (-2) i
    pure 1

-- | @gameplayTags.has(target, tag) → boolean | nil, err@
tagHasFn ∷ TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagHasFn api = verb "has" (2, 2) ((,) <$> readTargetRef 1 <*> readTag 2)
    (\(ref, t) → fmap (Set.member t) <$> inspect api ref)
    (\b → Lua.pushboolean b *> pure 1)

-- * Queries

-- | Evaluate one expression over the selected categories of one page,
--   against one captured view: the page is resolved in the SAME view the
--   query runs over.
evaluate ∷ TagApi → (TagQuery, QueryOptions) → IO (Either Text QueryResult)
evaluate api (q, opts) = do
    v ← capture api
    pure $ case resolvePage (ltvManager v) (qoPage opts) of
        Left err → Left err
        Right (page, _) → Right (runLiveTagQueryIn (qoCategories opts) v (onPage v page) q)

-- | The one query path every mode shares: decode @(expr [, options])@,
--   evaluate, and hand the native result to the mode's answer.
queryVerb ∷ Text → (QueryResult → Lua.LuaE Lua.Exception Lua.NumResults)
          → TagApi → Lua.LuaE Lua.Exception Lua.NumResults
queryVerb name answer api = verb name (1, 2)
    ((,) <$> readExpression 1 <*> readQueryOptions 2) (evaluate api) answer

-- | @gameplayTags.query(expr [, options]) → { target, … } | nil, err@ —
--   every match, complete and duplicate-free, in the documented order.
tagQueryFn ∷ TagMarshal → TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagQueryFn marshal = queryVerb "query" $ \r → do
    Lua.newtable
    forM_ (zip [1 ..] (targetSetsToList r)) $ \(i, t) → do
        tmPushTarget marshal t
        Lua.rawseti (-2) i
    pure 1

-- | @gameplayTags.count(expr [, options]) → integer | nil, err@ — the
--   native result's size; no target is pushed and no list is built.
tagCountFn ∷ TagMarshal → TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagCountFn _ = queryVerb "count" $ \r → do
    Lua.pushinteger (fromIntegral (targetSetsSize r))
    pure 1

-- | @gameplayTags.exists(expr [, options]) → boolean | nil, err@
tagExistsFn ∷ TagMarshal → TagApi → Lua.LuaE Lua.Exception Lua.NumResults
tagExistsFn _ = queryVerb "exists" $ \r → do
    Lua.pushboolean (targetSetsSize r > 0)
    pure 1
