{-# LANGUAGE Strict #-}
-- | The @data\/physiology\/*.yaml@ authoring boundary (#2828, S2 of the
--   physiology stat-modes epic #2827): the system registry file, its
--   decoder and the semantic rules that refuse an unusable one.
--
--   __Two stages, the split the faction catalogue
--   ("Engine.Asset.YamlFactions") uses.__ 'loadPhysiologyYamlOutcome'
--   DECODES: shape, types and key spelling, answering 'Nothing' for a file
--   that is not a registry at all, which @scripts\/startup_loader.lua@
--   turns into its terminal parse failure (#2203). 'admitPhysiologyDoc'
--   then applies the SEMANTIC rules and a violation refuses the whole
--   file, naming the offending id, so the engine never registers a
--   registry nothing can use. A refused file leaves whatever was
--   registered untouched.
--
--   __Why incomplete declarations are semantic, not parse, failures.__ A
--   missing @healthy:@ on an owned key or a missing @substitute:@ on an
--   interface entry decodes fine and is refused by name
--   (@system.key@), because an Aeson error could only reach for a JSON
--   path like @$.systems[2].owns[0]@ — an index nobody can map back to an
--   entry without counting.
module Engine.Asset.YamlPhysiology
    ( -- * The authored shapes
      PhysiologyYamlDoc(..)
    , PhysiologyYamlSystem(..)
    , PhysiologyYamlOwned(..)
    , PhysiologyYamlHealthy(..)
    , PhysiologyYamlDerived(..)
    , PhysiologyYamlDerivedHealthy(..)
    , PhysiologyYamlEntry(..)
    , PhysiologyYamlSubstitute(..)
      -- * Loading
    , loadPhysiologyYamlOutcome
      -- * Post-decode validation
    , PhysiologyRefusal(..)
    , admitPhysiologyDoc
    ) where

import UPrelude
import Data.List (nub, (\\))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))
import Unit.Physiology (classifyKey, KeyRole(..), registryKeys)
import Unit.Physiology.Types

-- * The authored shapes

-- | One scalar that is a number or the NAME of a constant.
newtype YamlQuantity = YamlQuantity Quantity

instance FromJSON YamlQuantity where
    parseJSON v = case v of
        Aeson.Number n → pure (YamlQuantity (QLit (realToFrac n)))
        Aeson.String t → pure (YamlQuantity (QConst t))
        _ → fail "expected a number or the name of a constant"

-- | How a healthy rule was written.
data PhysiologyYamlHealthy
    = YamlHealthyConst !Quantity
    | YamlHealthyFrom !Text ![Quantity]
    | YamlHealthyRule !Text
      -- ^ @{ rule: max_stamina }@ (#2834): a NAMED rule
    deriving (Show, Eq)

instance FromJSON PhysiologyYamlHealthy where
    parseJSON = withObject "healthy rule" $ \o → do
        onlyKeys ["const", "from", "scale", "rule"] o
        mConst ← o .:? "const"
        mFrom  ← o .:? "from"
        mRule  ← o .:? "rule"
        case (mConst, mFrom, mRule) of
            (Just (YamlQuantity q), Nothing, Nothing) → do
                forbid "scale" o "a `const` rule"
                pure (YamlHealthyConst q)
            (Nothing, Just k, Nothing) → do
                scales ← scaleList o
                pure (YamlHealthyFrom k scales)
            (Nothing, Nothing, Just r) → do
                forbid "scale" o "a `rule` rule"
                pure (YamlHealthyRule r)
            _ → fail "a healthy rule is exactly one of `const: <value>`, \
                     \`from: <stat>` (with an optional `scale:`) or \
                     \`rule: <name>`"

-- | @scale:@ is one factor or a list of factors, multiplied together.
scaleList ∷ Aeson.Object → Aeson.Parser [Quantity]
scaleList o = case KM.lookup (Key.fromText "scale") o of
    Nothing → pure []
    Just v@(Aeson.Array _) → map unwrap <$> parseJSON v
    Just v → (\q → [unwrap q]) <$> parseJSON v
  where
    unwrap (YamlQuantity q) = q

data PhysiologyYamlOwned = PhysiologyYamlOwned
    { pyoKey     ∷ !Text
    , pyoHealthy ∷ !(Maybe PhysiologyYamlHealthy)
    , pyoField   ∷ !(Maybe Text)
      -- ^ @field: uiBlood@ (#2837): the dedicated unit field storing the key
    } deriving (Show, Eq)

instance FromJSON PhysiologyYamlOwned where
    parseJSON = withObject "owned key" $ \o → do
        onlyKeys ["key", "healthy", "field"] o
        PhysiologyYamlOwned ⊚ o .: "key" ⊛ o .:? "healthy" ⊛ o .:? "field"

-- | How a derived key's Nominal answer was written: a number, or the word
--   @live@ (#2836, S9: the rule itself, evaluated with the system's inputs
--   at their healthy readings and every other input live).
data PhysiologyYamlDerivedHealthy
    = YamlDerivedNumber !Float
    | YamlDerivedWord !Text
    deriving (Show, Eq)

instance FromJSON PhysiologyYamlDerivedHealthy where
    parseJSON v = case v of
        Aeson.Number n → pure (YamlDerivedNumber (realToFrac n))
        Aeson.String t → pure (YamlDerivedWord t)
        _ → fail "a derived key's healthy value is a number or `live`"

data PhysiologyYamlDerived = PhysiologyYamlDerived
    { pydKey     ∷ !Text
    , pydRule    ∷ !Text
    , pydHealthy ∷ !(Maybe PhysiologyYamlDerivedHealthy)
    } deriving (Show, Eq)

instance FromJSON PhysiologyYamlDerived where
    parseJSON = withObject "derived key" $ \o → do
        onlyKeys ["key", "rule", "healthy"] o
        PhysiologyYamlDerived ⊚ o .: "key" ⊛ o .: "rule" ⊛ o .:? "healthy"

-- | A substitute as written: @{ value: <q> }@ or a bare word.
data PhysiologyYamlSubstitute
    = YamlSubValue !Quantity
    | YamlSubWord !Text
    deriving (Show, Eq)

instance FromJSON PhysiologyYamlSubstitute where
    parseJSON v = case v of
        Aeson.String t → pure (YamlSubWord t)
        Aeson.Object o → do
            onlyKeys ["value"] o
            YamlQuantity q ← o .: "value"
            pure (YamlSubValue q)
        _ → fail "a substitute is `{ value: <q> }` or one of `none`, \
                 \`virtual_meal`, `tissue_recovery`, `live`"

-- | One @provides@ \/ @contributes@ \/ @effects@ entry.
data PhysiologyYamlEntry = PhysiologyYamlEntry
    { pyeName       ∷ !Text
    , pyeSubstitute ∷ !(Maybe PhysiologyYamlSubstitute)
    } deriving (Show, Eq)

instance FromJSON PhysiologyYamlEntry where
    parseJSON = withObject "interface entry" $ \o → do
        onlyKeys ["name", "substitute"] o
        PhysiologyYamlEntry ⊚ o .: "name" ⊛ o .:? "substitute"

data PhysiologyYamlSystem = PhysiologyYamlSystem
    { pysId          ∷ !Text
    , pysRequires    ∷ ![Text]
    , pysOwns        ∷ ![PhysiologyYamlOwned]
    , pysDerived     ∷ ![PhysiologyYamlDerived]
    , pysMeters      ∷ ![Text]
    , pysProvides    ∷ ![PhysiologyYamlEntry]
    , pysContributes ∷ ![PhysiologyYamlEntry]
    , pysEffects     ∷ ![PhysiologyYamlEntry]
    , pysRenamedFrom ∷ ![Text]
    } deriving (Show, Eq)

instance FromJSON PhysiologyYamlSystem where
    parseJSON = withObject "physiology system" $ \o → do
        onlyKeys [ "id", "requires", "owns", "derived", "meters"
                 , "provides", "contributes", "effects", "renamed_from" ] o
        PhysiologyYamlSystem
            ⊚ o .:  "id"
            ⊛ o .:? "requires"    .!= []
            ⊛ o .:? "owns"        .!= []
            ⊛ o .:? "derived"     .!= []
            ⊛ o .:? "meters"      .!= []
            ⊛ o .:? "provides"    .!= []
            ⊛ o .:? "contributes" .!= []
            ⊛ o .:? "effects"     .!= []
            ⊛ o .:? "renamed_from" .!= []

-- | One registry file.
data PhysiologyYamlDoc = PhysiologyYamlDoc
    { pydConstants ∷ !(Map.Map Text Float)
    , pydSystems   ∷ ![PhysiologyYamlSystem]
    , pydProfiles  ∷ !(Map.Map Text (Map.Map Text Text))
      -- ^ profile id → system id → mode, as written.
    , pydRetired   ∷ ![Text]
      -- ^ ids of systems that no longer exist (#2829).
    } deriving (Show, Eq)

instance FromJSON PhysiologyYamlDoc where
    parseJSON = withObject "physiology registry" $ \o → do
        onlyKeys ["constants", "systems", "profiles", "retired"] o
        PhysiologyYamlDoc
            ⊚ o .:? "constants" .!= Map.empty
            ⊛ o .:? "systems"   .!= []
            ⊛ o .:? "profiles"  .!= Map.empty
            ⊛ o .:? "retired"   .!= []

-- | Reject any key outside @allowed@, naming it: a typo'd key silently
--   ignored is how a declaration ends up incomplete without a diagnostic.
onlyKeys ∷ [Text] → Aeson.Object → Aeson.Parser ()
onlyKeys allowed o =
    case [ Key.toText k | k ← KM.keys o, Key.toText k `notElem` allowed ] of
        []      → pure ()
        (k : _) → fail ("unknown key `" ⧺ T.unpack k ⧺ "`")

forbid ∷ Text → Aeson.Object → String → Aeson.Parser ()
forbid k o what = when (KM.member (Key.fromText k) o) $
    fail ("`" ⧺ T.unpack k ⧺ "` does not apply to " ⧺ what)

-- * Loading

-- | Decode one registry file. 'Nothing' is a DECODE failure and nothing
--   else; every semantic rule lives in 'admitPhysiologyDoc'.
loadPhysiologyYamlOutcome ∷ LoggerState → FilePath
                          → IO (Maybe PhysiologyYamlDoc)
loadPhysiologyYamlOutcome logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse physiology YAML "
                <> T.pack path <> ": " <> tshow err
            return Nothing
        Right doc → do
            logDebug logger CatAsset $ "Parsed physiology registry "
                <> T.pack path <> ": " <> tshow (length (pydSystems doc))
                <> " system(s), " <> tshow (Map.size (pydProfiles doc))
                <> " profile(s)"
            return (Just doc)

-- * Post-decode validation

-- | Why a decoded registry was refused entire: the short reason phrase a
--   terminal startup line leads with, and the offending id.
data PhysiologyRefusal = PhysiologyRefusal
    { refusalReason ∷ !Text
    , refusalDetail ∷ !Text
    } deriving (Show, Eq)

refuse ∷ Text → Text → Either PhysiologyRefusal a
refuse reason detail = Left (PhysiologyRefusal reason detail)

-- | The registry a document means, or its first refusal, in document
--   order. Rules, each naming the offender:
--
--     * the registry declares at least one system, with a non-blank id,
--       and ids are unique;
--     * every owned, derived and meter name is unique across the WHOLE
--       registry (a key has one owner), as are interface entry names
--       within their section;
--     * a system requires only known capabilities and owns something;
--     * every owned key has a healthy rule, every derived key a known
--       rule and a healthy value, every interface entry a recognized
--       substitute;
--     * every constant a rule or substitute names is declared, every
--       number written anywhere (constants, literal healthy values and
--       scale factors, literal substitutes, derived healthy values) is
--       finite, and a rule's @from:@ input is not itself a
--       registry key (a healthy rule reads UNOWNED inputs);
--     * the @salt_concentration@ rule has the @salt@ and @hydration@
--       keys it reads and a numeric healthy value; the @circulation@ rule
--       (#2836) is @live@ and has the @heart_rate@ owned key it reads from
--       its own system and the @core_temp@ owned key some system owns;
--     * a @live@ substitute names a live derived key of its own system;
--     * every profile names known systems with valid modes and covers
--       every system (a profile is a fidelity tier for ALL of them).
admitPhysiologyDoc ∷ FilePath → PhysiologyYamlDoc
                   → Either PhysiologyRefusal PhysiologyRegistry
admitPhysiologyDoc path doc = do
    when (null (pydSystems doc)) $
        refuse "empty physiology registry" (T.pack path)
    forM_ (Map.toList (pydConstants doc)) $ \(n, v) →
        when (isNaN v ∨ isInfinite v) $
            refuse "non-finite physiology constant" n
    systems ← traverse (admitSystem constants) (pydSystems doc)
    checkUnique "duplicate physiology system" (map psId systems)
    checkUnique "duplicate physiology key" $ concat
        [ map okKey (psOwns s) ⧺ map dkKey (psDerived s) ⧺ psMeters s
        | s ← systems ]
    -- Retired and renamed ids (#2829) are RECOGNITION only: a stored
    -- override naming one is not an undeclared system. Each must be a
    -- real, unambiguous id that is not also an active system, or the
    -- distinction the load boundary draws would be meaningless.
    let activeIds = map psId systems
        renamedIds = concatMap psRenamedFrom systems
    forM_ (pydRetired doc ⧺ renamedIds) $ \old →
        when (T.null (T.strip old) ∨ T.any (≡ ' ') old) $
            refuse "malformed retired physiology system id" old
    checkUnique "duplicate retired physiology system"
                (pydRetired doc ⧺ renamedIds)
    forM_ (pydRetired doc ⧺ renamedIds) $ \old →
        when (old `elem` activeIds) $
            refuse "retired physiology system is still active" old
    let partial = PhysiologyRegistry
            { prSource = Just path, prConstants = constants
            , prSystems = systems, prProfiles = []
            , prRetired = pydRetired doc }
        keys = registryKeys partial
    forM_ systems $ \s → do
        forM_ (psOwns s) $ \o → case okHealthy o of
            HealthyFrom k _ | k `elem` keys →
                refuse "physiology healthy rule reads a registry key"
                       (psId s <> "." <> okKey o <> " <- " <> k)
            _ → pure ()
        forM_ (psDerived s) $ \d → case dkRule d of
            DerivedSaltConcentration → do
                requireOwned partial (psId s <> "." <> dkKey d) "salt"
                requireOwned partial (psId s <> "." <> dkKey d) "hydration"
                when (dkHealthy d ≡ DerivedLive) $
                    refuse "physiology derived rule cannot be live"
                           (psId s <> "." <> dkKey d)
            DerivedBloodMax      → bloodDerived s d
            DerivedBloodFraction → bloodDerived s d
            DerivedCirculation → do
                requireOwned partial (psId s <> "." <> dkKey d) "core_temp"
                unless (any ((≡ "heart_rate") ∘ okKey) (psOwns s)) $
                    refuse "unresolved physiology derived rule input"
                           (psId s <> "." <> dkKey d
                            <> " needs owned key heart_rate in its own system")
                unless (Map.member "RESTING_HR" constants) $
                    refuse "unresolved physiology derived rule input"
                           (psId s <> "." <> dkKey d
                            <> " needs the constant RESTING_HR")
                -- SD-21: a Nominal circulation is the live rule, never a
                -- constant that would erase blood loss, sepsis, fitness and
                -- cold.
                when (dkHealthy d ≢ DerivedLive) $
                    refuse "physiology circulation must be live"
                           (psId s <> "." <> dkKey d)
        forM_ (psProvides s) $ \e → when (ieSubstitute e ≡ SubLive) $
            unless (any (\d → dkKey d ≡ ieName e ∧ dkHealthy d ≡ DerivedLive)
                        (psDerived s)) $
                refuse "live physiology substitute without a live derived key"
                       (psId s <> ".provides." <> ieName e)
    profiles ← traverse (admitProfile systems) (Map.toList (pydProfiles doc))
    pure partial { prProfiles = profiles }
  where
    constants = pydConstants doc

    checkUnique reason names = case names \\ nub names of
        (n : _) → refuse reason n
        []      → pure ()

    -- #2837: the blood derived keys read the dedicated field of an owned key
    -- of their OWN system, and are live (a Nominal read is the mode-aware
    -- view, never a constant).
    bloodDerived s d = do
        unless (any (isJust ∘ okField) (psOwns s)) $
            refuse "unresolved physiology derived rule input"
                   (psId s <> "." <> dkKey d
                    <> " needs a field-backed owned key in its own system")
        when (dkHealthy d ≢ DerivedLive) $
            refuse "physiology blood derived key must be live"
                   (psId s <> "." <> dkKey d)

    requireOwned reg who k = case classifyKey reg k of
        Just (_, RoleOwned _) → pure ()
        _ → refuse "unresolved physiology derived rule input"
                   (who <> " needs owned key " <> k)

admitSystem ∷ Map.Map Text Float → PhysiologyYamlSystem
            → Either PhysiologyRefusal PhysiologySystem
admitSystem constants s = do
    when (T.null (T.strip sid) ∨ T.any (≡ ' ') sid) $
        refuse "malformed physiology system id" sid
    forM_ (pysRequires s) $ \c →
        when (c `notElem` knownCapabilities) $
            refuse "unknown physiology capability" (sid <> ": " <> c)
    when (null (pysOwns s) ∧ null (pysMeters s)) $
        refuse "incomplete physiology system" (sid <> " owns nothing")
    owns ← traverse admitOwned (pysOwns s)
    derived ← traverse admitDerived (pysDerived s)
    provides ← traverse (admitEntry "provides") (pysProvides s)
    contributes ← traverse (admitEntry "contributes") (pysContributes s)
    effects ← traverse (admitEntry "effects") (pysEffects s)
    pure PhysiologySystem
        { psId = sid, psRequires = pysRequires s, psOwns = owns
        , psDerived = derived, psMeters = pysMeters s
        , psProvides = provides, psContributes = contributes
        , psEffects = effects, psRenamedFrom = pysRenamedFrom s }
  where
    sid = pysId s

    ownedLabel o = sid <> "." <> pyoKey o

    admitOwned o = case pyoHealthy o of
        Nothing → refuse "owned physiology key without a healthy rule"
                         (sid <> "." <> pyoKey o)
        Just h  → do
            rule ← case h of
                YamlHealthyConst q → HealthyConst <$> declared (ownedLabel o) q
                YamlHealthyFrom k qs →
                    HealthyFrom k <$> traverse (declared (ownedLabel o)) qs
                YamlHealthyRule "max_stamina" → Right HealthyMaxStamina
                YamlHealthyRule "blood_max" → Right HealthyBloodMax
                YamlHealthyRule r → refuse "unknown physiology healthy rule"
                                           (ownedLabel o <> ": " <> r)
            field ← case pyoField o of
                Nothing → Right Nothing
                Just "uiBlood" → Right (Just "uiBlood")
                Just f → refuse "unknown physiology storage field"
                                (ownedLabel o <> ": " <> f)
            pure (OwnedKey (pyoKey o) rule field)

    admitDerived d = do
        rule ← maybe (refuse "unknown physiology derived rule"
                             (sid <> "." <> pydKey d <> ": " <> pydRule d))
                     Right (parseDerivedRule (pydRule d))
        written ← maybe (refuse "derived physiology key without a healthy value"
                                (sid <> "." <> pydKey d))
                        Right (pydHealthy d)
        healthy ← case written of
            YamlDerivedWord "live" → Right DerivedLive
            YamlDerivedWord w → refuse "unknown physiology derived healthy value"
                                       (sid <> "." <> pydKey d <> ": " <> w)
            YamlDerivedNumber v
                | isNaN v ∨ isInfinite v →
                    refuse "non-finite physiology value"
                           (sid <> "." <> pydKey d <> ": " <> tshow v)
                | otherwise → Right (DerivedFixed v)
        pure (DerivedKey (pydKey d) rule healthy)

    admitEntry section e = do
        let who = sid <> "." <> section <> "." <> pyeName e
        sub ← maybe (refuse "physiology interface entry without a substitute"
                            who)
                    Right (pyeSubstitute e)
        substitute ← case sub of
            YamlSubValue q → SubValue <$> declared who q
            YamlSubWord w → case w of
                "none"            → Right SubNone
                "virtual_meal"    → Right SubVirtualMeal
                "tissue_recovery" → Right SubTissueRecovery
                "live"            → Right SubLive
                "wound_flow"      → Right SubWoundFlow
                _ → refuse "unknown physiology substitute" (who <> ": " <> w)
        pure (InterfaceEntry (pyeName e) substitute)

    declared who q = case q of
        QLit f
            | isNaN f ∨ isInfinite f →
                refuse "non-finite physiology value"
                       (who <> ": " <> tshow f)
            | otherwise → Right q
        QConst n
            | Map.member n constants → Right q
            | otherwise → refuse "unknown physiology constant"
                                 (who <> ": " <> n)

admitProfile ∷ [PhysiologySystem] → (Text, Map.Map Text Text)
             → Either PhysiologyRefusal PhysiologyProfile
admitProfile systems (pid, entries) = do
    modes ← traverse one (Map.toList entries)
    forM_ systems $ \s →
        when (psId s `notElem` map fst modes) $
            refuse "incomplete physiology profile" (pid <> ": " <> psId s)
    pure (PhysiologyProfile pid (Map.fromList modes))
  where
    one (sid, modeText)
        | sid `notElem` map psId systems =
            refuse "unknown physiology system in profile" (pid <> ": " <> sid)
        | otherwise = case parsePhysioMode modeText of
            Nothing → refuse "invalid physiology mode"
                             (pid <> ": " <> sid <> ": " <> modeText)
            Just m  → Right (sid, m)
