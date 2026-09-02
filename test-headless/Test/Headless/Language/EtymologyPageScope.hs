{-# LANGUAGE OverloadedStrings #-}
-- | @world.getEtymology@'s TARGET page and its RECURRENCE page are
--   separate scopes (#1265).
--
--   The module contract and CLAUDE.md both promise that recurrence is
--   computed from the ACTIVE page, with every inactive page absent by
--   construction. The public query broke that promise for its optional
--   third argument: an explicit @pageId@ was resolved straight out of
--   @wmWorlds@ and then used for BOTH the target and the candidate set,
--   so naming a live inactive page surfaced that page's discovered
--   locations as recurrence.
--
--   Driven through the REAL registered Lua function against real
--   manager refs — same bare-Lua-backend technique as
--   'Test.Headless.Item.GroundPageOwnership', and for the same reason:
--   the defect lives in the query's page resolution, which the pure
--   'eligibleEntities' \/ 'recurrenceFor' examples in
--   'Test.Headless.Language.Etymology' structurally cannot see. Those
--   keep covering the WITHIN-page eligibility rules this issue leaves
--   untouched; this covers the boundary between two pages.
--
--   /Why the fixture looks the way it does./ An empty recurrence list
--   would satisfy a leak assertion vacuously, so every case here pins
--   BOTH sides: the inactive page's uniquely named candidate is absent
--   AND the expected active-page candidates are present. That needs the
--   two pages to record the SAME 'LanguageProvenance' — a morpheme
--   identity is @(provenance, concept)@ and matching is by identity
--   alone, so two languages never share one — and every name here is a
--   real 'renderNative' rendering of a @Modifier ASH _@ expression: they
--   all share the ASH morpheme, and their distinct HEAD concepts give
--   each a distinct stored name, which is the only handle a recurrence
--   entry exposes.
--
--   Pages are in-memory 'emptyWorldState's carrying hand-built
--   identities, location instances and river names, so two live worlds
--   cost no worldgen. That is also why 'Spec.hs' wraps this module in
--   'Test.Headless.Harness.withHeadlessEngineNoWorld' rather than the
--   world-thread-starting 'withHeadlessEngine' (#1362) — see
--   'requireLiveEngine' below.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match \"Language etymology (page scope)\"'@.
module Test.Headless.Language.EtymologyPageScope (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Language.Etymology.Source (EtymologySource(..))
import Language.Generated.Profile (generateProfile)
import Language.Generated.Render (renderNative)
import Language.Generated.Root (assignLanguageRoots)
import Language.Generated.Types
    ( LangSeed(..), LanguageProvenance(..), Profile, currentGeneratorVersion )
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Language.Semantic.English (renderGloss)
import Language.Semantic.Types
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationInstances(..)
    , LocationLifecycle(..), emptyLocationInstances )
import World.Base (GeoFeatureId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldIdentity(..), WorldPageId(..))
import World.River.Naming (RiverName(..), RiverNames(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Fixture identities ------------------------------------------------

-- | The ACTIVE page. Everything named on it is an expected recurrence
--   candidate; nothing an inactive-page target does may hide one.
pageActive ∷ WorldPageId
pageActive = WorldPageId "ety_active"

-- | A live page that is NOT active. Its names are the leak: they may
--   never appear as recurrence, however the query reaches it.
pageOther ∷ WorldPageId
pageOther = WorldPageId "ety_inactive"

-- | Visible but backed by no 'WorldState' — how 'resolveActiveWorld' is
--   made to resolve to 'Nothing', which is exactly the mid-transition
--   window it documents.
pageGhost ∷ WorldPageId
pageGhost = WorldPageId "ety_ghost"

-- | The ONE numeric location id both pages use, so \"same id, different
--   page\" is the fixture's default rather than something it contrives.
--   A page-blind self-exclusion silently drops the active page's #5
--   whenever the inactive page's #5 is the target.
sharedLocId ∷ Int
sharedLocId = 5

-- | Likewise for rivers: one 'GeoFeatureId' named on both pages. Feature
--   ids restart per timeline, so this too is the natural collision.
sharedRiverId ∷ Int
sharedRiverId = 3

-- * Languages ---------------------------------------------------------

-- | The language BOTH pages record in the main scene. Recurrence matches
--   on morpheme identity — @(provenance, concept)@ — so a fixture whose
--   two pages spoke different languages could not tell a leak from an
--   ordinary non-match.
provMain ∷ LanguageProvenance
provMain = LanguageProvenance (LangSeed 0x5EED0000000000A1)
                              currentGeneratorVersion

-- | A second language, for the one scene that must show the TARGET
--   page's own language validating its decomposition.
provOther ∷ LanguageProvenance
provOther = LanguageProvenance (LangSeed 0x0FF1CE0000000B2C)
                               currentGeneratorVersion

-- * Names -------------------------------------------------------------

-- | Every fixture name shares the ASH modifier and differs in its head,
--   so all of them are linked to each other by one morpheme while each
--   stays individually identifiable by its stored text.
ashOf ∷ Text → NameExpr
ashOf headConcept = Modifier (ConceptId "ASH") (ConceptId headConcept)

activeWorldE, activeLocE, activeRiverE ∷ NameExpr
activeWorldE = ashOf "FORD"
activeLocE   = ashOf "KEEP"
activeRiverE = ashOf "RIVER"

otherWorldE, otherLocE, otherRiverE ∷ NameExpr
otherWorldE = ashOf "LAND"
otherLocE   = ashOf "HOLLOW"
otherRiverE = ashOf "VALE"

-- * The scene ---------------------------------------------------------

-- | Which language each page records, whether the active page has gen
--   params at all, and what is visible — the three axes the cases here
--   vary.
data SceneOpts = SceneOpts
    { soActiveProv   ∷ LanguageProvenance
    , soOtherProv    ∷ LanguageProvenance
    , soActiveParams ∷ Bool
      -- ^ 'False' strips the ACTIVE page's 'wsGenParamsRef': the missing
      --   recurrence ingredient that must not downgrade a valid target.
    , soVisible      ∷ [WorldPageId]
    }

defaultScene ∷ SceneOpts
defaultScene = SceneOpts
    { soActiveProv   = provMain
    , soOtherProv    = provMain
    , soActiveParams = True
    , soVisible      = [pageActive]
    }

-- | Install the two live pages. @pageActive@ is deliberately the HEAD of
--   'wmWorlds' as well as the visible page, because 'resolveActiveWorld'
--   falls back to that head when nothing is visible: the empty-@wmVisible@
--   case below asserts the fallback lands on the intended active page,
--   which is only a statement about the fix if the head IS that page.
installScene ∷ EngineEnv → Catalogue → SceneOpts → IO ()
installScene env cat opts = do
    wsA ← emptyWorldState
    wsO ← emptyWorldState
    writeIORef (wsIdentityRef wsA)
        (Just (identityFor cat (soActiveProv opts) activeWorldE))
    writeIORef (wsIdentityRef wsO)
        (Just (identityFor cat (soOtherProv opts) otherWorldE))
    writeIORef (wsGenParamsRef wsA) $ if soActiveParams opts
        then Just (paramsFor cat (soActiveProv opts) activeLocE activeRiverE)
        else Nothing
    writeIORef (wsGenParamsRef wsO) $
        Just (paramsFor cat (soOtherProv opts) otherLocE otherRiverE)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageActive, wsA), (pageOther, wsO)]
        , wmVisible = soVisible opts }

-- | A page whose ONE discovered location and ONE named river carry real
--   generated names and real etymology sources, so both are genuine
--   recurrence candidates rather than entities that drop out for want of
--   a validated decomposition.
paramsFor
    ∷ Catalogue → LanguageProvenance → NameExpr → NameExpr → WorldGenParams
paramsFor cat prov locE rivE = defaultWorldGenParams
    { wgpLocationInstances = emptyLocationInstances
        { lisNextId = sharedLocId + 1
        , lisById   = HM.singleton (LocationInstanceId sharedLocId)
                                   (instanceFor cat prov locE)
        }
    , wgpRiverNames = RiverNames
        (HM.singleton (GeoFeatureId sharedRiverId)
                      (riverNameFor cat prov rivE))
    }

identityFor ∷ Catalogue → LanguageProvenance → NameExpr → WorldIdentity
identityFor cat prov expr = WorldIdentity
    { wiName      = nameOf cat prov expr
    , wiGloss     = glossOf cat expr
    , wiLanguage  = Just prov
    , wiEtymology = Just (sourceFor prov expr)
    }

instanceFor
    ∷ Catalogue → LanguageProvenance → NameExpr → LocationInstance
instanceFor cat prov expr = LocationInstance
    { liId              = LocationInstanceId sharedLocId
    , liDefId           = "ruin_small"
    , liChunk           = ChunkCoord 0 0
    , liAnchor          = (0, 0)
    , liBounds          = AbsBounds 0 0 1 1
    , liDisplayName     = nameOf cat prov expr
    , liGloss           = glossOf cat expr
    , liEtymology       = Just (sourceFor prov expr)
    , liLifecycle       = LifecycleDiscovered
    , liContentsSpawned = False
    , liEncounter       = Nothing
    , liSignificant     = []
    , liClearEventEmitted = False
    }

riverNameFor ∷ Catalogue → LanguageProvenance → NameExpr → RiverName
riverNameFor cat prov expr = RiverName
    { rvnDisplayName = nameOf cat prov expr
    , rvnGloss       = glossOf cat expr
    , rvnEtymology   = Just (sourceFor prov expr)
    }

sourceFor ∷ LanguageProvenance → NameExpr → EtymologySource
sourceFor prov expr = EtymologySource { esExpr = expr, esLanguage = prov }

-- * Rendering ---------------------------------------------------------

-- The names are RENDERED by the shipping renderer rather than written
-- down, for the same reason the pure spec renders its own: a hand-picked
-- string would stop matching the moment the generator's phonology moved,
-- and the query's answer is the renderer's output.

profileFor ∷ LanguageProvenance → Profile
profileFor prov = case generateProfile (lpVersion prov) (lpSeed prov) of
    Right p → p
    Left e  → error ("test setup: profile: " <> show e)

nameOf ∷ Catalogue → LanguageProvenance → NameExpr → Text
nameOf cat prov expr =
    either (\e → error ("test setup: native: " <> show e)) id
           (renderNative profile roots expr)
  where
    profile = profileFor prov
    roots   = assignLanguageRoots profile (catOrdinals cat) (conceptIds cat)

glossOf ∷ Catalogue → NameExpr → Maybe Text
glossOf cat expr =
    either (\e → error ("test setup: gloss: " <> show e)) Just
           (renderGloss cat expr)

loadRealCatalogue ∷ IO Catalogue
loadRealCatalogue = do
    loaded ← loadCatalogue conceptCataloguePath conceptOrdinalPath
    case loaded of
        Right cat → pure cat
        Left err  → error ("test setup: catalogue: " <> show err)

-- * Lua plumbing ------------------------------------------------------

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | Boot a bare Lua backend, install a scene, and run one probe. Each
--   example gets its OWN backend and its own pages: the query caches the
--   concept catalogue in the backend state, and a case that strips the
--   active page's gen params must not leave that page behind for the
--   next one.
runProbe ∷ EngineEnv → SceneOpts → Text → IO (Catalogue, Text)
runProbe env opts call = do
    requireLiveEngine env
    cat ← loadRealCatalogue
    ls  ← newBareLuaBackend env
    installScene env cat opts
    out ← evalDebug ls (etymologyProbe call)
    requireLiveEngine env
    pure (cat, unquote out)

-- | Every example must run against a LIVE engine (#1362).
--
--   This spec used to be wrapped in the world-thread-starting
--   'Test.Headless.Harness.withHeadlessEngine'. Its pages are in-memory
--   'emptyWorldState's, but the visible one carries 'paramsFor', built
--   on 'defaultWorldGenParams' — seed 42, worldSize 128, and an EMPTY
--   'wgpPlates'. The worker's chunk loading picked that up and died in
--   'twoNearestPlates'; 'World.Thread' catches the exception, logs it,
--   writes 'CleaningUp' and stops WITHOUT rethrowing, so the whole
--   spec ran green against a dead worker. 'Spec.hs' now wraps this
--   module in 'withHeadlessEngineNoWorld', which starts no worker at
--   all.
--
--   'CleaningUp' is the state that crash writes, and nothing in this
--   spec's own path ever sets it, so checking it on both sides of every
--   probe is what keeps that false-green from returning silently: a
--   reintroduced worker that dies again fails these examples instead of
--   passing them. It adds no example of its own, so the gate's count is
--   unchanged.
requireLiveEngine ∷ EngineEnv → IO ()
requireLiveEngine env = do
    lifecycle ← readIORef (lifecycleRef env)
    when (lifecycle ≡ CleaningUp) $ expectationFailure
        "engine lifecycle is CleaningUp: a background worker died \
        \mid-suite, so this example is not testing a live engine (#1362)"

-- | The debug console renders a returned STRING the way it would print
--   it, quotes included. Strip the one balanced pair so the assertions
--   below compare the probe's own line rather than its console rendering.
unquote ∷ Text → Text
unquote t = fromMaybe t (T.stripSuffix "\"" =≪ T.stripPrefix "\"" t)

-- | Just the recurrence half of a probe line. The TARGET's own stored
--   name is on the other side of the @|@, and an inactive-page target
--   legitimately reports its own name there — so an absence assertion
--   that scanned the whole line would find the target and fail for the
--   wrong reason.
recurrencePart ∷ Text → Text
recurrencePart = T.drop 1 ∘ T.dropWhile (≢ '|')

-- | Run one @world.getEtymology@ call and reduce its result to a single
--   comparable line: the target's stored name, then every recurrence
--   entry as @kind:name@, deduplicated and sorted.
--
--   Deduplicated because a candidate sharing two morphemes with the
--   target appears under each; sorted so the assertion states a SET.
--   Carrying the @kind@ is what lets a river-target case assert that no
--   river entry exists AT ALL rather than merely that one particular
--   river is missing.
etymologyProbe ∷ Text → Text
etymologyProbe call = T.intercalate "\n"
    [ "local r = " <> call
    , "if type(r) ~= 'table' then return 'not-a-table' end"
    , "if not r.available then"
    , "  return 'unavailable:' .. tostring(r.reason)"
    , "end"
    , "local seen, names = {}, {}"
    , "for _, m in ipairs(r.recurrence or {}) do"
    , "  for _, e in ipairs(m.entries or {}) do"
    , "    local k = e.kind .. ':' .. e.name"
    , "    if not seen[k] then"
    , "      seen[k] = true"
    , "      names[#names + 1] = k"
    , "    end"
    , "  end"
    , "end"
    , "table.sort(names)"
    , "return r.name .. '|' .. table.concat(names, ',')"
    ]

-- | The line 'etymologyProbe' produces for a given target name and
--   expected candidate set — built the same way, so an assertion reads
--   as the set it pins.
expectedLine ∷ Text → [Text] → Text
expectedLine target entries =
    target <> "|" <> T.intercalate "," (sort entries)

-- | A recurrence entry as the probe spells it.
worldEntry, locationEntry ∷ Text → Text
worldEntry    n = "world:" <> n
locationEntry n = "location:" <> n

-- | The Lua call for each of the three target forms against an explicit
--   page.
worldCall, locationCall, riverCall ∷ Text → Text
worldCall pid = "world.getEtymology(\"world\", nil, \"" <> pid <> "\")"
locationCall pid = "world.getEtymology(\"location\", "
    <> T.pack (show sharedLocId) <> ", \"" <> pid <> "\")"
riverCall pid = "world.getEtymology(\"river\", "
    <> T.pack (show sharedRiverId) <> ", \"" <> pid <> "\")"

otherPage ∷ Text
otherPage = case pageOther of WorldPageId t → t

-- * The spec ----------------------------------------------------------

spec ∷ SpecWith EngineEnv
spec = describe "target page and recurrence page are separate scopes" $ do

    describe "an explicit INACTIVE page selects the target only" $ do

        it "a world target keeps the named page's own name and draws \
           \recurrence from the active page -- including that page's own \
           \world name, which a page-blind self-exclusion would drop" $
            \env → do
                (cat, r) ← runProbe env defaultScene (worldCall otherPage)
                r `shouldBe` expectedLine (nameOf cat provMain otherWorldE)
                    [ worldEntry (nameOf cat provMain activeWorldE)
                    , locationEntry (nameOf cat provMain activeLocE) ]

        it "a LOCATION target sharing its numeric id with an active-page \
           \location does not self-exclude that active candidate" $
            \env → do
                (cat, r) ← runProbe env defaultScene (locationCall otherPage)
                r `shouldBe` expectedLine (nameOf cat provMain otherLocE)
                    [ worldEntry (nameOf cat provMain activeWorldE)
                    , locationEntry (nameOf cat provMain activeLocE) ]

        it "a RIVER target admits NO river at all -- the inspected river \
           \is not on the active page, and the same-numbered river there \
           \is a different river" $ \env → do
            (cat, r) ← runProbe env defaultScene (riverCall otherPage)
            r `shouldBe` expectedLine (nameOf cat provMain otherRiverE)
                [ worldEntry (nameOf cat provMain activeWorldE)
                , locationEntry (nameOf cat provMain activeLocE) ]

        forM_ ([ ("world", worldCall), ("location", locationCall)
               , ("river", riverCall) ] ∷ [(String, Text → Text)]) $
            \(form, call) →
              it ("names neither the inactive page's location nor its \
                  \river for a " <> form <> " target") $ \env → do
                  (cat, r) ← runProbe env defaultScene (call otherPage)
                  let links = recurrencePart r
                  forM_ [ nameOf cat provMain otherLocE
                        , nameOf cat provMain otherRiverE ] $ \leak →
                      (leak, T.isInfixOf leak links) `shouldBe` (leak, False)

    describe "an OMITTED page is unchanged" $ do

        it "resolves target and recurrence alike on the active page" $
            \env → do
                (cat, r) ← runProbe env defaultScene
                               "world.getEtymology(\"world\")"
                r `shouldBe` expectedLine (nameOf cat provMain activeWorldE)
                    [ locationEntry (nameOf cat provMain activeLocE) ]

        it "still resolves a river target on the active page, the \
           \inspected river excluding itself as before" $ \env → do
            (cat, r) ← runProbe env defaultScene
                           ("world.getEtymology(\"river\", "
                            <> T.pack (show sharedRiverId) <> ")")
            r `shouldBe` expectedLine (nameOf cat provMain activeRiverE)
                [ worldEntry (nameOf cat provMain activeWorldE)
                , locationEntry (nameOf cat provMain activeLocE) ]

    describe "pages that do not resolve" $ do

        it "reports no_entity for an explicit page that does not exist" $
            \env → do
                (_, r) ← runProbe env defaultScene (worldCall "nope")
                r `shouldBe` "unavailable:no_entity"

        it "falls back to wmWorlds' HEAD when nothing is visible, and \
           \that head is the active page -- so the inactive page stays \
           \excluded" $ \env → do
            (cat, r) ← runProbe env defaultScene { soVisible = [] }
                                (worldCall otherPage)
            r `shouldBe` expectedLine (nameOf cat provMain otherWorldE)
                [ worldEntry (nameOf cat provMain activeWorldE)
                , locationEntry (nameOf cat provMain activeLocE) ]

        it "substitutes NO page when active resolution fails outright, \
           \keeping the explicitly selected target with empty \
           \recurrence" $ \env → do
            (cat, r) ← runProbe env defaultScene { soVisible = [pageGhost] }
                                (worldCall otherPage)
            r `shouldBe` expectedLine (nameOf cat provMain otherWorldE) []

        it "an active page with no gen params yields empty recurrence \
           \rather than downgrading a valid target" $ \env → do
            (cat, r) ← runProbe env defaultScene { soActiveParams = False }
                                (worldCall otherPage)
            r `shouldBe` expectedLine (nameOf cat provMain otherWorldE) []

    describe "decomposition validates against the TARGET page's language" $

        it "an inactive target in a different language than the active \
           \page still decomposes, with recurrence legitimately empty" $
            \env → do
                (cat, r) ← runProbe env
                               defaultScene { soOtherProv = provOther }
                               (worldCall otherPage)
                -- Validating against the ACTIVE page's language instead
                -- would refuse the source as foreign and report
                -- available = false.
                r `shouldBe` expectedLine (nameOf cat provOther otherWorldE) []
