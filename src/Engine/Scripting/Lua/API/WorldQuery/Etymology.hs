{-# LANGUAGE Strict #-}
-- | Name-etymology queries (#1104): @world.getEtymology@ and the
--   minimal @world.getRiverAt@ selection resolution it needs.
--
--   /One decomposition path./ World, location, and river are three thin
--   ADAPTERS over one shared call: each resolves its own stored name,
--   gloss, and optional 'EtymologySource', and hands all three to
--   'Language.Etymology.decomposeName'. Nothing entity-specific reaches
--   past that boundary, so the three can never explain a name
--   differently (#1104 requirement 3).
--
--   /Read-only./ Every lookup here is an 'readIORef' on state the world
--   thread owns; nothing is written, queued, or re-rendered. A stored
--   name, gloss, source, and provenance are byte-identical before and
--   after a query because the query never has a handle it could write
--   through (requirement 2).
--
--   /Recurrence eligibility is computed, never remembered./ There is no
--   session history and no \"seen names\" log (requirement 8). Each call
--   re-derives the eligible set from the ACTIVE page as it stands: the
--   current world name, every location at or beyond
--   'LifecycleDiscovered', and — only when a river is the thing being
--   inspected — that one river. An undiscovered location, another river,
--   and every inactive page are absent by construction rather than by a
--   filter that could be relaxed.
--
--   /The target page and the recurrence page are separate scopes/
--   (#1265). An explicit @pageId@ selects the TARGET — its stored name,
--   gloss, source, and the language its decomposition is validated
--   against all come from the page that was named. Recurrence candidates
--   never do: they come exclusively from 'resolveActiveWorld''s page,
--   whichever page the target came from, which is what makes the
--   paragraph above true of the PUBLIC query and not merely of its
--   omitted-argument case. The two scopes meeting means identity has to
--   carry the page ('eePage'): every page's world entry is
--   @(\"world\", Nothing)@ and location ids are page-local, so
--   self-exclusion that compared kind and id alone would silently drop
--   the active page's own world name — or an unrelated same-numbered
--   location — from an inactive-page target's recurrence.
module Engine.Scripting.Lua.API.WorldQuery.Etymology
    ( worldGetEtymologyFn
    , worldGetRiverAtFn
      -- * Pure helpers (tested directly)
    , EtyEntity(..)
    , eligibleEntities
    , recurrenceFor
    , riverAtTile
    ) where

import UPrelude
import Data.IORef (modifyIORef', readIORef)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified HsLua as Lua
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Engine.Core.State (resolveActiveWorld)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LanguageCache(..))
import Language.Etymology
import Language.Etymology.Source (EtymologySource)
import Language.Semantic.Types (Catalogue, ConceptId(..), catalogueErrorText)
import Language.Generated.Types
    ( LanguageProvenance(..), generatorVersionInt, langSeedText )
import Language.Semantic.Catalogue (conceptCataloguePath, loadCatalogue)
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), instancesToList
    , isDiscoveredLifecycle )
import World.Geology.Hash (wrappedDeltaUV)
import World.River.Identity (timelineRivers)
import World.River.Naming (RiverName(..), lookupRiverName)
-- Re-exports 'GeoFeatureId', 'WorldIdentity', 'WorldPageId', the
-- world-manager/state records, and the river geometry this reads.
import World.Types
import Control.Exception (IOException, evaluate, try)

-- * Eligible entities ------------------------------------------------

-- | One thing on the active page whose name can participate in
--   recurrence, reduced to exactly what a recurrence entry may expose:
--   its KIND and its already-visible stored name (#1104 requirement 9's
--   tightened wording — no coordinates, instance ids, lifecycle state,
--   or river geometry travels with it).
--
--   The identity fields are here only so an entity can recognize
--   ITSELF and drop out of its own recurrence list; they are never
--   pushed to Lua.
--
--   That identity is PAGE-QUALIFIED (#1265). Neither of the other two
--   fields is unique on its own across pages: every page's world entry
--   is @(\"world\", Nothing)@, and a 'LocationInstanceId' is allocated
--   per page, so two pages routinely hold locations numbered alike.
--   Since an explicit @pageId@ can put the target on a different page
--   from the candidates, dropping the page here would make an
--   inactive-page target silently censor the active page's own world
--   name, and an equal-numbered active location, from its recurrence.
data EtyEntity = EtyEntity
    { eePage   ∷ !WorldPageId     -- ^ the page this entity lives on
    , eeKind   ∷ !Text            -- ^ @world@ \/ @location@ \/ @river@
    , eeRef    ∷ !(Maybe Int)     -- ^ its own id within that kind
    , eeName   ∷ !Text
    , eeGloss  ∷ !(Maybe Text)
    , eeSource ∷ !(Maybe EtymologySource)
    } deriving (Show, Eq)

-- | The eligible set for one query (#1104 requirement 8), for the page
--   the caller is enumerating — which is always the ACTIVE one for a
--   real query (#1265).
--
--   @mRiver@ is the river currently being inspected, and is 'Nothing'
--   for a world or location target — which is what makes \"no river
--   participates at all\" the behavior for those two, rather than
--   \"every named river on the page\". It is 'Nothing' for a river
--   target on ANOTHER page too: the inspected river is not on this page,
--   and re-resolving its numeric id here would admit a different river
--   that merely shares a 'GeoFeatureId'.
eligibleEntities
    ∷ WorldPageId
    → Maybe WorldIdentity
    → [LocationInstance]
    → Maybe (GeoFeatureId, RiverName)
    → [EtyEntity]
eligibleEntities pid mIdent instances mRiver =
    worldEntry ⧺ locationEntries ⧺ riverEntry
  where
    worldEntry =
        [ EtyEntity pid "world" Nothing (wiName i) (wiGloss i) (wiEtymology i)
        | Just i ← [mIdent] ]

    -- 'isDiscoveredLifecycle' rather than a fresh comparison: it is the
    -- single predicate the zoom-map icons and the discovered query field
    -- already share, so this cannot drift from what the rest of the game
    -- calls discovered.
    locationEntries =
        [ EtyEntity pid "location" (Just (unLocationInstanceId (liId li)))
                    (liDisplayName li) (liGloss li) (liEtymology li)
        | li ← instances
        , isDiscoveredLifecycle (liLifecycle li)
        ]

    riverEntry =
        [ EtyEntity pid "river" (Just fid) (rvnDisplayName rn) (rvnGloss rn)
                    (rvnEtymology rn)
        | Just (GeoFeatureId fid, rn) ← [mRiver] ]

-- | The recurrence links for one decomposition: per morpheme, in
--   surface order, the OTHER eligible entities whose own validated
--   decomposition contains the same morpheme identity.
--
--   Only names with a successfully validated etymology participate
--   (requirement 8's last line): an entity whose decomposition comes
--   back unavailable is simply absent, never matched on its raw text.
--   The inspected entity itself is excluded — it is the thing being
--   explained, not a place the morpheme also turns up.
--
--   \"Itself\" is decided by the full page-qualified identity (#1265):
--   an entity on ANOTHER page is a different thing however its kind and
--   id read.
recurrenceFor
    ∷ Catalogue → EtyEntity → [EtyEntity] → Etymology
    → [(MorphemeIdentity, [EtyEntity])]
recurrenceFor cat self eligible ety =
    [ (mi, [ e | (e, other) ← decomposed, etymologyMentions mi other ])
    | mi ← etymologyIdentities ety ]
  where
    decomposed =
        [ (e, o)
        | e ← eligible
        , not (isSelf e)
        , EtyAvailable o ← [ decomposeName cat (eeName e) (eeGloss e)
                                          (eeSource e) ]
        ]
    isSelf e = eePage e ≡ eePage self ∧ eeKind e ≡ eeKind self
               ∧ eeRef e ≡ eeRef self

-- * River selection ---------------------------------------------------

-- | Which river's visible channel covers a tile, if any (#1104
--   requirement 10's \"selecting a visible river segment, resolved
--   through #1102's stable river identity\").
--
--   Deliberately minimal, and deliberately the CHANNEL rather than the
--   valley: the channel is where the water a player can see actually
--   is, so a click on a river bank does not silently select the river
--   two valleys over.
--
--   Geometry is read exactly the way
--   'World.Hydrology.River.Carving.carveFromSegment' reads it, including
--   'wrappedDeltaUV'. That is not a stylistic nicety: the world wraps on
--   the u axis, so a river crossing the seam has segment endpoints whose
--   RAW coordinate difference from a nearby tile is a whole world wide.
--   Measuring with raw deltas would put every seam-crossing river
--   impossibly far from its own water — resolving to no river, or to
--   whichever unrelated river happened to be nearer in unwrapped space —
--   and the etymology entry point for those rivers would simply not
--   exist. Sharing the carve's own delta function is what keeps this
--   answer and the terrain it is asking about in the same coordinate
--   space.
--
--   The nearest qualifying river wins, and a river with no resolvable
--   'GeoFeatureId' is skipped rather than reported without one: an
--   unidentified river cannot be looked up, and a wrong id would attach
--   another river's name to it.
riverAtTile ∷ Int → GeoTimeline → Int → Int → Maybe GeoFeatureId
riverAtTile worldSize timeline gx gy =
    fmap snd (listToMaybe (sortOn fst hits))
  where
    hits =
        [ (d, fid)
        | (Just fid, river) ← timelineRivers timeline
        , Just d ← [ closestChannelDistance river ]
        ]

    closestChannelDistance river = case dists of
        [] → Nothing
        ds → Just (minimum ds)
      where
        dists = [ d
                | seg ← V.toList (rpSegments river)
                , Just d ← [segmentDistance seg]
                , d ≤ max 1.0 (fromIntegral (rsWidth seg) / 2.0) ]

    -- Perpendicular distance from the segment AXIS, and only for a
    -- point that actually lies beside the segment: past either end the
    -- next segment — or the next river — owns the water, so an
    -- unbounded axis distance would let a short tributary claim tiles
    -- far downstream of itself.
    segmentDistance seg
        | segLen < 0.001          = Nothing
        | alongT < 0 ∨ alongT > 1 = Nothing
        | otherwise               = Just (abs (px * ny - py * nx))
      where
        GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        -- Both deltas are taken FROM the segment start, wrapped, exactly
        -- as the carve takes them.
        (pxi, pyi) = wrappedDeltaUV worldSize gx gy sx sy
        (fxi, fyi) = wrappedDeltaUV worldSize ex ey sx sy
        px = fromIntegral pxi ∷ Double
        py = fromIntegral pyi ∷ Double
        fdx = fromIntegral fxi ∷ Double
        fdy = fromIntegral fyi ∷ Double
        segLen = sqrt (fdx * fdx + fdy * fdy)
        nx = fdx / segLen
        ny = fdy / segLen
        alongT = (px * nx + py * ny) / segLen

-- * The Lua entry points ----------------------------------------------

-- | world.getEtymology(kind [, id] [, pageId]) → table
--
--   @kind@ is @\"world\"@, @\"location\"@, or @\"river\"@. A location
--   takes its 'LocationInstanceId'; a river takes the @id@
--   @world.getRivers@ / @world.getRiverAt@ reports. A world target has
--   no id of its own, so an explicit page for one is the THIRD
--   positional argument with a @nil@ second:
--   @world.getEtymology(\"world\", nil, pageId)@.
--
--   /Which page (#1265)./ @pageId@ names the TARGET only; recurrence has
--   its own scope and is never widened by it:
--
--   * Omitted — target and recurrence are both the canonical active page
--     ('resolveActiveWorld'). The whole query is one page, as before.
--   * A live page that is not active — the target is resolved there, and
--     its stored name, gloss, source, decomposition and page-language
--     validation are all that page's. Recurrence candidates still come
--     only from the active page, so an inactive page's names can never
--     appear as recurrence. Its own world entry and its equal-numbered
--     locations stay eligible, because self-exclusion is page-qualified;
--     no river participates at all, since the inspected river is not on
--     the active page.
--   * A page that does not exist (and likewise an entity that does not)
--     — @available = false@ with @reason = \"no_entity\"@, unchanged.
--   * No visible page at all (a mid-transition window) — recurrence
--     follows 'resolveActiveWorld' exactly, including its head-of-
--     @wmWorlds@ fallback, and substitutes nothing when that resolves to
--     'Nothing'. A target selected by an explicit page keeps its full
--     result with recurrence simply empty; a missing ingredient on the
--     RECURRENCE page never downgrades a valid target.
--
--   A successful result is
--   @{ available = true, name, gloss?, language = { seed, version },
--      form, morphemes = {...}, tokens = {...}, recurrence = {...} }@.
--   Concatenating @tokens@' @text@ reproduces @name@ exactly.
--
--   An unsuccessful one is @{ available = false, reason, reasonText }@,
--   and the caller keeps showing the stored name (#1104 requirement 7).
--   @reason@ is a stable lowercase key; @reasonText@ is the player-facing
--   sentence.
worldGetEtymologyFn
    ∷ WorldSimCapability → LuaBackendState
    → Lua.LuaE Lua.Exception Lua.NumResults
worldGetEtymologyFn wsc backendState = do
    kindArg ← Lua.tostring 1
    idArg   ← Lua.tointeger 2
    pageArg ← Lua.tostring 3
    let kind = maybe "" TE.decodeUtf8Lenient kindArg
        mPid = WorldPageId ∘ TE.decodeUtf8Lenient <$> pageArg
        mId  = fromIntegral <$> idArg
    result ← Lua.liftIO $ resolveEtymology wsc backendState kind mId mPid
    pushEtymologyResult result
    return 1

-- | world.getRiverAt(gx, gy [, pageId]) → { id, name?, gloss? } | nil
--
--   The one selection resolution #1104 adds: which visible river covers
--   a tile. Deliberately singular — it answers for the SELECTED tile
--   only and exposes nothing about any other river, so it is not a
--   global river list (#1104 requirement 10's explicit exclusion).
--
--   @name@ and @gloss@ are absent for a river the page never named,
--   mirroring @world.getRivers@' own optional-field convention.
worldGetRiverAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetRiverAtFn wsc = do
    gxArg   ← Lua.tointeger 1
    gyArg   ← Lua.tointeger 2
    pageArg ← Lua.tostring 3
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            let mPid = WorldPageId ∘ TE.decodeUtf8Lenient <$> pageArg
            mParams ← Lua.liftIO $ genParamsFor wsc mPid
            case mParams ⌦ \params →
                    (,) params <$> riverAtTile (wgpWorldSize params)
                                               (wgpGeoTimeline params)
                                               (fromIntegral gx)
                                               (fromIntegral gy) of
                Nothing → Lua.pushnil
                Just (params, fid@(GeoFeatureId raw)) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral raw)
                    Lua.setfield (Lua.nth 2) "id"
                    forM_ (lookupRiverName fid (wgpRiverNames params)) $ \rn → do
                        Lua.pushstring (TE.encodeUtf8 (rvnDisplayName rn))
                        Lua.setfield (Lua.nth 2) "name"
                        forM_ (rvnGloss rn) $ \g → do
                            Lua.pushstring (TE.encodeUtf8 g)
                            Lua.setfield (Lua.nth 2) "gloss"
        _ → Lua.pushnil
    return 1

-- * Resolution --------------------------------------------------------

-- | What one query resolved to: either a decomposition plus the
--   recurrence links around it, or the honest reason there is none.
data ResolvedEtymology
    = ResolvedOk !Etymology ![(MorphemeIdentity, [EtyEntity])]
    | ResolvedNone !EtyUnavailable !(Maybe (Text, Maybe Text))
      -- ^ the reason, plus the entity's own stored name and gloss when
      --   there IS an entity. #1104 requirement 7 is explicit that the
      --   UI keeps showing the stored name beside the explanation, so an
      --   unavailable result that dropped the name would force the panel
      --   either to hide it or to fetch it a second way — and a second
      --   way is a second answer waiting to disagree.
    | ResolvedNoEntity
      -- ^ the page or the entity itself does not exist — distinct from
      --   \"exists but cannot be explained\", and reported as an ordinary
      --   unavailable result rather than an error

resolveEtymology
    ∷ WorldSimCapability → LuaBackendState → Text → Maybe Int
    → Maybe WorldPageId → IO ResolvedEtymology
resolveEtymology wsc backendState kind mId mPid = do
    -- ONE read of the manager, so the target page and the active page
    -- are decided against the same registry rather than against two
    -- snapshots a concurrent page switch could have moved between.
    mgr ← readIORef (wsWorldManagerRef wsc)
    case pageFor mgr mPid of
        Nothing → pure ResolvedNoEntity
        Just (targetPid, ws) → do
            mIdent  ← readIORef (wsIdentityRef ws)
            mParams ← readIORef (wsGenParamsRef ws)
            case mParams of
                Nothing → pure ResolvedNoEntity
                Just params → do
                    let instances = instancesToList (wgpLocationInstances params)
                        mRiver = do
                            rid ← mId
                            let fid = GeoFeatureId rid
                            rn ← lookupRiverName fid (wgpRiverNames params)
                            pure (fid, rn)
                        mSelf = selfEntity targetPid kind mId mIdent instances
                                           params
                    case mSelf of
                        Nothing → pure ResolvedNoEntity
                        Just self → do
                            eligible ← recurrenceCandidates mgr kind targetPid
                                                            mRiver
                            eCat ← resolveCatalogue backendState
                            let stored = Just (eeName self, eeGloss self)
                            pure $ case eCat of
                                Left msg → ResolvedNone
                                    (EtyReconstructionFailed msg) stored
                                -- The PAGE's own language is passed in,
                                -- so a source belonging to a different
                                -- one is refused before it can produce a
                                -- validated-looking explanation in the
                                -- wrong lexicon.
                                Right cat → case decomposeEntityName cat
                                                    (wiLanguage =≪ mIdent)
                                                    (eeName self)
                                                    (eeGloss self)
                                                    (eeSource self) of
                                    -- Sharpen the bare "no source" into
                                    -- WHY there is none: #1104
                                    -- requirement 7 lists a custom name,
                                    -- an absent source, and an absent
                                    -- provenance as three separate
                                    -- honest answers, and only the
                                    -- adapter can tell them apart.
                                    EtyUnavailable EtyNoSource →
                                        ResolvedNone (absentReason kind mIdent)
                                                     stored
                                    EtyUnavailable u → ResolvedNone u stored
                                    EtyAvailable ety → ResolvedOk ety
                                        (recurrenceFor cat self eligible ety)

-- | The names a decomposition may be linked against, sourced
--   EXCLUSIVELY from the canonical active page (#1265) — whichever page
--   the target itself came from.
--
--   This is the one place the module's active-page contract is enforced
--   for the public query, and it is enforced by never READING another
--   page rather than by filtering one out afterwards.
--
--   Three absences all mean \"no candidates\", never \"no result\": no
--   page resolves ('resolveActiveWorld' returning 'Nothing' during a
--   transition — nothing may be substituted for it), the active page has
--   no gen params, or it has neither an identity nor a discovered
--   location. A target the caller legitimately selected keeps its own
--   result in every one of them.
recurrenceCandidates
    ∷ WorldManager → Text → WorldPageId → Maybe (GeoFeatureId, RiverName)
    → IO [EtyEntity]
recurrenceCandidates mgr kind targetPid mRiver = case resolveActiveWorld mgr of
    Nothing → pure []
    Just (activePid, ws) → do
        mIdent  ← readIORef (wsIdentityRef ws)
        mParams ← readIORef (wsGenParamsRef ws)
        pure $ case mParams of
            Nothing → []
            Just params → eligibleEntities activePid mIdent
                (instancesToList (wgpLocationInstances params))
                -- Only a RIVER target admits a river into the eligible
                -- set (requirement 8) — and only when the inspected
                -- river is genuinely ON this page. Resolving the same
                -- numeric id against another page's 'wgpRiverNames'
                -- would admit a DIFFERENT river, which is precisely the
                -- inspected-river-only rule this query promises.
                (if kind ≡ "river" ∧ activePid ≡ targetPid
                     then mRiver else Nothing)

-- | Which of requirement 7's absences applies when a name has no
--   etymology source.
--
--   A world page with no #1092 provenance was named by the PLAYER —
--   'World.Page.Types.mkWorldIdentity' is the only path that produces
--   one, and it never records a language — so "custom" is a fact here,
--   not a guess. A location or river on such a page has no language
--   behind it either, but its name is a definition label or a
--   fallback rather than a player's choice, so it reports the page's
--   missing language instead of claiming the player picked it.
--
--   With provenance present, the name really was generated and its
--   expression simply predates #1104.
absentReason ∷ Text → Maybe WorldIdentity → EtyUnavailable
absentReason kind mIdent = case wiLanguage =≪ mIdent of
    Just _  → EtyNoSource
    Nothing
        | kind ≡ "world" → EtyCustomName
        | otherwise      → EtyNoProvenance

-- | The entity a query targets, as the same reduced record recurrence
--   uses — so the target and the things it is compared against are
--   resolved by ONE piece of code and cannot disagree about what an
--   entity's stored name is.
selfEntity
    ∷ WorldPageId → Text → Maybe Int → Maybe WorldIdentity
    → [LocationInstance] → WorldGenParams → Maybe EtyEntity
selfEntity pid kind mId mIdent instances params = case kind of
    "world" → do
        i ← mIdent
        pure (EtyEntity pid "world" Nothing (wiName i) (wiGloss i)
                        (wiEtymology i))
    "location" → do
        lid ← mId
        li  ← listToMaybe [ l | l ← instances
                          , unLocationInstanceId (liId l) ≡ lid ]
        pure (EtyEntity pid "location" (Just lid) (liDisplayName li)
                        (liGloss li) (liEtymology li))
    "river" → do
        rid ← mId
        rn  ← lookupRiverName (GeoFeatureId rid) (wgpRiverNames params)
        pure (EtyEntity pid "river" (Just rid) (rvnDisplayName rn)
                        (rvnGloss rn) (rvnEtymology rn))
    _ → Nothing

-- | The page an optional @pageId@ argument selects, with its id — the
--   one resolution both entry points here share. An omitted argument is
--   the canonical active page; an explicit one is looked up directly,
--   active or not.
pageFor
    ∷ WorldManager → Maybe WorldPageId → Maybe (WorldPageId, WorldState)
pageFor mgr Nothing    = resolveActiveWorld mgr
pageFor mgr (Just pid) = (,) pid <$> lookup pid (wmWorlds mgr)

worldStateFor
    ∷ WorldSimCapability → Maybe WorldPageId → IO (Maybe WorldState)
worldStateFor wsc mPid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    pure (snd <$> pageFor mgr mPid)

genParamsFor
    ∷ WorldSimCapability → Maybe WorldPageId → IO (Maybe WorldGenParams)
genParamsFor wsc mPid = do
    mState ← worldStateFor wsc mPid
    case mState of
        Nothing → pure Nothing
        Just ws → readIORef (wsGenParamsRef ws)

-- | The concept catalogue, reusing @world.suggestName@'s own cache
--   ('lbsLanguageCache') so an etymology query costs no filesystem work
--   in a session that has already read it — and fills that same cache
--   for the dice button in a session that has not. Its failure is
--   reported, never substituted with an empty catalogue: every concept
--   would then look invalid and every name would report a fabricated
--   reason.
resolveCatalogue ∷ LuaBackendState → IO (Either Text Catalogue)
resolveCatalogue backendState = do
    cached ← readIORef (lbsLanguageCache backendState)
    case lcCatalogue <$> cached of
        Just done → pure done
        Nothing   → do
            eRead ← readCatalogueForEtymology
            atomicWrite eRead
            pure eRead
  where
    atomicWrite eRead = modifyIORef' (lbsLanguageCache backendState) $ \cur →
        case cur of
            Just lc → Just lc
            Nothing → Just LanguageCache
                { lcCatalogue = eRead, lcSuggester = Nothing }

-- | Read the catalogue with BOTH of its failure modes turned into one
--   descriptive 'Left' — 'loadCatalogue' throws for a missing or
--   unreadable file and returns 'Left' for one it could parse and
--   reject. Mirrors @world.suggestName@'s own reader for exactly the
--   same reason: a broken installation must not turn every query into
--   filesystem work.
readCatalogueForEtymology ∷ IO (Either Text Catalogue)
readCatalogueForEtymology = do
    eRead ← try (loadCatalogue conceptCataloguePath ⌦ evaluate)
    pure $ case eRead of
        Left (ioErr ∷ IOException) → Left (describe (T.pack (show ioErr)))
        Right (Left cErr)          → Left (describe (catalogueErrorText cErr))
        Right (Right cat)          → Right cat
  where
    describe why = "the concept catalogue could not be read (" <> why <> ")"

-- * Pushing -----------------------------------------------------------

pushEtymologyResult ∷ ResolvedEtymology → Lua.LuaE Lua.Exception ()
pushEtymologyResult res = case res of
    ResolvedNoEntity → pushUnavailable "no_entity"
        "there is no such name on this world" Nothing
    ResolvedNone u stored → pushUnavailable (etyUnavailableReason u)
                                            (etyUnavailableText u) stored
    ResolvedOk ety links → do
        Lua.newtable
        Lua.pushboolean True
        Lua.setfield (Lua.nth 2) "available"
        Lua.pushstring (TE.encodeUtf8 (etyName ety))
        Lua.setfield (Lua.nth 2) "name"
        forM_ (etyGloss ety) $ \g → do
            Lua.pushstring (TE.encodeUtf8 g)
            Lua.setfield (Lua.nth 2) "gloss"
        Lua.pushstring (TE.encodeUtf8 (etyFormText (etyForm ety)))
        Lua.setfield (Lua.nth 2) "form"
        pushLanguage (etyLanguage ety)
        Lua.setfield (Lua.nth 2) "language"
        pushList (etyMorphemes ety) pushMorpheme
        Lua.setfield (Lua.nth 2) "morphemes"
        pushList (etyTokens ety) pushToken
        Lua.setfield (Lua.nth 2) "tokens"
        pushList links pushRecurrence
        Lua.setfield (Lua.nth 2) "recurrence"

-- | An unavailable result. Carries the entity's own stored name and
--   gloss whenever there IS an entity, so the panel can keep showing the
--   name it could not explain (#1104 requirement 7) without going and
--   asking for it a second, independently-resolvable way.
pushUnavailable
    ∷ Text → Text → Maybe (Text, Maybe Text) → Lua.LuaE Lua.Exception ()
pushUnavailable reason why stored = do
    Lua.newtable
    Lua.pushboolean False
    Lua.setfield (Lua.nth 2) "available"
    Lua.pushstring (TE.encodeUtf8 reason)
    Lua.setfield (Lua.nth 2) "reason"
    Lua.pushstring (TE.encodeUtf8 why)
    Lua.setfield (Lua.nth 2) "reasonText"
    forM_ stored $ \(nm, gl) → do
        Lua.pushstring (TE.encodeUtf8 nm)
        Lua.setfield (Lua.nth 2) "name"
        forM_ gl $ \g → do
            Lua.pushstring (TE.encodeUtf8 g)
            Lua.setfield (Lua.nth 2) "gloss"

-- | The language a decomposition belongs to, in exactly the
--   @{ seed = \<decimal string\>, version = \<int\> }@ shape
--   @world.getLanguageProvenance@ reports and @world.init@ accepts — a
--   language seed is unsigned 64-bit, and neither of Lua's numeric
--   types carries the top of that range intact.
pushLanguage ∷ LanguageProvenance → Lua.LuaE Lua.Exception ()
pushLanguage prov = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 (langSeedText (lpSeed prov)))
    Lua.setfield (Lua.nth 2) "seed"
    Lua.pushinteger (fromIntegral (generatorVersionInt (lpVersion prov)))
    Lua.setfield (Lua.nth 2) "version"

pushMorpheme ∷ EtyMorpheme → Lua.LuaE Lua.Exception ()
pushMorpheme m = do
    Lua.newtable
    setText "id" (morphemeIdentityText (emIdentity m))
    setText "concept" (conceptIdText (emConcept m))
    setText "role" (etyRoleText (emRole m))
    setText "surface" (emSurface m)
    setText "free" (emFree m)
    Lua.pushboolean (emBound m)
    Lua.setfield (Lua.nth 2) "bound"
    setText "lemma" (emLemma m)
    forM_ (emMark m) $ \mk → setText "mark" (etyMarkText mk)
    forM_ (emMarkSurface m) $ \ms → setText "markSurface" ms

pushToken ∷ EtyToken → Lua.LuaE Lua.Exception ()
pushToken t = do
    Lua.newtable
    setText "kind" (etyTokenKindText t)
    setText "text" (etyTokenText t)
    case t of
        TokenMorpheme cid _ → setText "concept" (conceptIdText cid)
        TokenMark mk _      → setText "mark" (etyMarkText mk)
        _                   → pure ()

pushRecurrence
    ∷ (MorphemeIdentity, [EtyEntity]) → Lua.LuaE Lua.Exception ()
pushRecurrence (mi, entries) = do
    Lua.newtable
    setText "morpheme" (morphemeIdentityText mi)
    setText "concept" (conceptIdText (miConcept mi))
    pushList entries $ \e → do
        Lua.newtable
        setText "kind" (eeKind e)
        setText "name" (eeName e)
    Lua.setfield (Lua.nth 2) "entries"

setText ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
setText key val = do
    Lua.pushstring (TE.encodeUtf8 val)
    Lua.setfield (Lua.nth 2) key

pushList ∷ [a] → (a → Lua.LuaE Lua.Exception ()) → Lua.LuaE Lua.Exception ()
pushList xs push = do
    Lua.newtable
    forM_ (zip [1 ..] xs) $ \(i, x) → do
        push x
        Lua.rawseti (Lua.nth 2) i
