{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlItems
    ( ItemYamlDef(..)
    , ItemYamlWeight(..)
    , ItemYamlContainer(..)
    , ItemYamlStorage(..)
    , ItemYamlContent(..)
    , ItemYamlFood(..)
    , ItemYamlRollSpec(..)
    , ItemYamlQualityTier(..)
    , ItemYamlWeapon(..)
    , ItemYamlArmor(..)
    , ItemYamlBuff(..)
    , ItemYamlFile(..)
    , loadItemYaml
    , loadItemYamlOutcome
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlListOutcome)

-- | Parse a REQUIRED authored physical quantity that must be a finite,
--   strictly positive number, diagnosing every rejection BY DEFINITION
--   NAME (#1233 requirement 2).
--
--   Naming the definition is the whole reason this exists rather than a
--   bare @v .: key@ plus a check. 'Engine.Asset.YamlList.loadYamlList'
--   supplies the failing FILE path in its warning, but an ordinary Aeson
--   field error only reaches for a JSON path like @$.items[7].bulk@ —
--   an index nobody can map back to a definition in a 14-entry file
--   without counting. So every fault below (absent, wrong type, zero,
--   negative, non-finite) is raised as ONE uniformly-shaped message
--   carrying @defName@, and the two halves together name the file AND
--   the definition.
--
--   Taking the whole 'Aeson.Value' rather than decoding to 'Float'
--   first is also deliberate: YAML's @.nan@/@.inf@ resolve to STRINGS
--   (the yaml package's scalar resolver only recognizes ordinary
--   numeric syntax), so decoding first would surface those as a type
--   error naming neither the definition nor what was actually wrong.
--   The finiteness check still has to run AFTER narrowing, because a
--   perfectly ordinary @1.0e+100@ is a valid 'Scientific' that becomes
--   'Infinity' in the engine's 32-bit 'Float' field.
requirePositiveQuantity
    ∷ Text        -- ^ the definition's @name@, for the diagnostic
    → Text        -- ^ what the quantity is, e.g. @"bulk"@
    → Text        -- ^ its unit, e.g. @"litres"@
    → Aeson.Object
    → Text        -- ^ the YAML key to read
    → Aeson.Parser Float
requirePositiveQuantity defName label unit v key = do
    mval ← v .:? Key.fromText key
    case mval of
        Nothing  → bad "is required and has no default"
        Just val → case val of
            Aeson.Number s →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then bad ("must be finite, got " <> tshow val)
                     else if f ≤ 0
                       then bad ("must be strictly positive, got " <> tshow f)
                       else pure f
            _ → bad ("must be a number of " <> unit <> ", got " <> tshow val)
  where
    bad why = fail ∘ T.unpack $
        "item definition '" <> defName <> "': " <> label <> " (key '"
        <> key <> "', " <> unit <> ") " <> why

-- | Optional container block. Items without this can't hold a fluid.
data ItemYamlContainer = ItemYamlContainer
    { iycCapacity    ∷ !Float
    , iycHolds       ∷ !Text
    , iycFillWeight  ∷ !Float   -- kg per fill unit (1.0 = litres/water)
    , iycDefaultFill ∷ !Float   -- fill when the spawn site gives none
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContainer where
    parseJSON = withObject "ItemYamlContainer" $ \v → ItemYamlContainer
        ⊚ v .:  "capacity"
        ⊛ v .:? "holds" .!= "water"
        ⊛ v .:? "fill_weight" .!= 1.0
        ⊛ v .:? "default_fill" .!= 0.0

-- | Optional @storage:@ block — portable ITEM-storage capacity (#1233).
--   Entirely separate from 'ItemYamlContainer' above: that one is a
--   homogeneous fluid/pill FILL, this one is an inventory of nested
--   items. A definition may carry both; neither inherits the other's
--   defaults or validation (@docs\/portable_loot_containers.md@ D-12).
--
--   BOTH capacities are required whenever the block is present, and
--   neither has a default — an omitted @weight_capacity@ is a
--   half-authored container, not "unlimited", and silently defaulting
--   either one would invent a physical limit the designer never chose.
--   They are also independent of each other and of the item's own
--   external @bulk@; see 'parseItemYamlStorage'.
data ItemYamlStorage = ItemYamlStorage
    { iysWeightCapacity ∷ !Float   -- ^ kg of contents (storage.weight_capacity)
    , iysBulkCapacity   ∷ !Float   -- ^ litres of contents (storage.bulk_capacity)
    } deriving (Show, Eq, Generic)

-- | Parse a @storage:@ block, threading the OWNING definition's name
--   through so a bad capacity is diagnosed the same way a bad top-level
--   @bulk@ is. There is deliberately no 'FromJSON' instance: the name is
--   not reachable from inside one, and a nameless
--   @Error in $.items[7].storage.bulk_capacity@ is the diagnostic
--   requirement 2 exists to rule out.
--   The object check is spelled out rather than delegated to
--   'withObject' for the same reason: @storage: 23@ would otherwise fail
--   with aeson's own "expected Object, but encountered Number", which
--   names neither the definition nor the block.
parseItemYamlStorage ∷ Text → Aeson.Value → Aeson.Parser ItemYamlStorage
parseItemYamlStorage defName val = case val of
    Aeson.Object v → ItemYamlStorage
        ⊚ requirePositiveQuantity defName "storage weight capacity"
              "kilograms" v "weight_capacity"
        ⊛ requirePositiveQuantity defName "storage bulk capacity"
              "litres" v "bulk_capacity"
    _ → fail ∘ T.unpack $
        "item definition '" <> defName <> "': storage must be a block \
        \authoring weight_capacity (kilograms) and bulk_capacity \
        \(litres), got " <> tshow val

-- | One entry in an item-container's default contents (first-aid kit /
--   toolbox): which item, how many, an optional fill for fillable
--   contents (a pill bottle's count, a fluid bottle's litres), and —
--   since #1418 — an optional NESTED @contents:@ list of the same shape.
--
--   The historical flat form decodes completely unchanged:
--   @- { item: bandage, count: 2, fill: 1 }@ is still exactly that entry
--   with no nested contents authored.
data ItemYamlContent = ItemYamlContent
    { iycoItem     ∷ !Text
    , iycoCount    ∷ !Int
    , iycoFill     ∷ !(Maybe Float)
    , iycoContents ∷ !(Maybe [ItemYamlContent])
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContent where
    parseJSON = withObject "ItemYamlContent" $ \v → ItemYamlContent
        ⊚ v .:  "item"
        ⊛ v .:? "count" .!= 1
        ⊛ v .:? "fill"
        -- NO @.!= []@ here, deliberately, and it is load-bearing: the
        -- three authoring states this field exists to express are
        -- omitted (delegate to the child definition's own defaults),
        -- @contents: []@ (materialise that child EMPTY) and a non-empty
        -- list (replace the child's defaults). Defaulting to @[]@ the way
        -- the DEFINITION-level key below does would collapse the first
        -- two into one and silently defeat the rule (#1418).
        --
        -- @contents: null@ resolves to the OMITTED case: aeson's '.:?'
        -- reads an explicit null as absent. That is deliberate, not
        -- incidental — an authored null says "nothing to say here", and
        -- saying nothing means the child definition decides.
        ⊛ v .:? "contents"

-- | Optional food block. Items without this can't be eaten. The
--   calories live under a `nutrition:` sub-object so future diet work
--   (protein / fat / carbohydrate / micronutrients) can add sibling
--   keys without restructuring the schema or bumping the save version.
--
--   'Item.Types.ItemFood' documents the two shapes as MUTUALLY
--   EXCLUSIVE, and since #1716 'parseItemYamlFood' below is what makes
--   that true: exactly one of the two fields is strictly positive in
--   any value this decoder produces, and the other is exactly zero.
data ItemYamlFood = ItemYamlFood
    { iyfCalories      ∷ !Float  -- ^ kcal per item (food.nutrition.calories);
                                 --   0 ⇒ not discrete food
    , iyfCaloriesPerKg ∷ !Float  -- ^ kcal per kg of fill for BULK food
                                 --   (food.nutrition.calories_per_kg);
                                 --   0 ⇒ not bulk food
    } deriving (Show, Eq, Generic)

-- | Parse ONE optional @nutrition:@ value. An absent key is the
--   documented "not this mode" spelling and reads as zero; a PRESENT
--   key must be a finite, NONNEGATIVE number once narrowed to the
--   engine's 32-bit 'Float'.
--
--   Every test runs AFTER narrowing, and that is the whole subtlety
--   (the same one 'requirePositiveQuantity' records). YAML's @.nan@ /
--   @.inf@ resolve to STRINGS — the yaml package's scalar resolver only
--   recognizes ordinary numeric syntax — so they arrive here as the
--   non-number case, while a perfectly ordinary @1.0e+100@ is a valid
--   'Scientific' that becomes 'Infinity' only in the @!Float@ field it
--   lands in. Positivity narrows first for the mirror-image reason: a
--   @1.0e-60@ is a strictly positive 'Scientific' that underflows to
--   exactly zero in a 'Float', and letting it "select" a mode would
--   author food the runtime cannot see.
--
--   Negative is rejected rather than clamped because the discrete feed
--   path (@Engine.Scripting.Lua.API.Units.Survival@) has an upper clamp
--   and no lower one: a negative @calories@ consumes the item and makes
--   the eater HUNGRIER, and returns a negative credit that is still
--   truthy in Lua. Requirement 9 of #1716 puts the fix here, at the
--   authoring boundary, and not as defensive clamping in that consumer.
--
--   @calories: null@ resolves to the ABSENT case, exactly as @.:?@
--   reads it everywhere else in this decoder. It is not silently
--   accepted: a definition whose only nutrition key is null selects no
--   mode, and the mode check below rejects it naming both keys and both
--   effective values.
parseNutritionValue
    ∷ Text          -- ^ the definition's @name@, for the diagnostic
    → Text          -- ^ this value's unit, e.g. @"kcal per item"@
    → Aeson.Object  -- ^ the @nutrition:@ block
    → Text          -- ^ the YAML key to read
    → Aeson.Parser Float
parseNutritionValue defName unit nut key = do
    mval ← nut .:? Key.fromText key
    case mval of
        Nothing  → pure 0.0
        Just val → case val of
            Aeson.Number s →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then bad ("must be finite, got " <> tshow val)
                     else if f < 0
                       then bad ("must not be negative, got " <> tshow f)
                       else pure f
            _ → bad ("must be a number of " <> unit <> ", got " <> tshow val)
  where
    bad why = fail ∘ T.unpack $
        "item definition '" <> defName <> "': food nutrition (key '"
        <> key <> "', " <> unit <> ") " <> why

-- | Parse a @food:@ block against the OWNING definition (#1716), which
--   is why there is deliberately no 'FromJSON' instance — exactly as
--   'parseItemYamlStorage' has none. Two things reachable only from out
--   there are load-bearing: the definition's @name@, so every rejection
--   is findable without reading engine source (requirement 5), and
--   whether it authors a @container:@ block, which decides whether bulk
--   nutrition can ever be eaten.
--
--   'Item.Types.ItemFood' documents two mutually exclusive shapes and
--   nothing enforced them, so four out-of-contract shapes reached
--   @unit.feed@. Each is rejected here:
--
--     * __Neither mode positive.__ The discrete branch removes the item
--       from the inventory and then credits @0@, returning a Lua number
--       @0@ — TRUTHY, so @if not unit.feed(…)@ reads a wasted item as a
--       successful meal.
--     * __Negative discrete calories.__ Same branch, no lower clamp:
--       the item is eaten and the eater ends up hungrier.
--     * __Both modes positive.__ @Survival.hs@ guards the bulk branch
--       on @ifCaloriesPerKg > 0@ and wins by branch order, so the
--       authored discrete value is silently unreachable.
--     * __Bulk nutrition with no @container:@.__ The bulk branch draws
--       from @iiCurrentFill@, which is 0 for every non-container, so
--       such a food always fails to feed — a silent permanent no-op.
--
--   The container test is on the DECODED @container:@ field, so
--   @container: null@ counts as no container and a bulk food authoring
--   it is rejected: @.:?@ reads an explicit null as absent, and this
--   check must agree with the field the runtime will actually see.
parseItemYamlFood
    ∷ Text   -- ^ the definition's @name@
    → Bool   -- ^ does the definition author a @container:@ block?
    → Aeson.Value
    → Aeson.Parser ItemYamlFood
parseItemYamlFood defName hasContainer val = case val of
    Aeson.Object v → do
        nut ← case KM.lookup "nutrition" v of
            Just (Aeson.Object n) → pure n
            Just other → fail ∘ T.unpack $
                "item definition '" <> defName <> "': food nutrition must \
                \be a block authoring exactly one of calories (kcal per \
                \item) or calories_per_kg (kcal per kilogram of fill), \
                \got " <> tshow other
            Nothing → fail ∘ T.unpack $
                "item definition '" <> defName <> "': food authors no \
                \nutrition: block — a food: block must author a \
                \nutrition: block with exactly one of calories (kcal per \
                \item) or calories_per_kg (kcal per kilogram of fill) \
                \strictly positive"
        cal   ← parseNutritionValue defName "kcal per item" nut "calories"
        perKg ← parseNutritionValue defName "kcal per kilogram of fill"
                    nut "calories_per_kg"
        case (cal > 0, perKg > 0) of
            (True, True) → fail ∘ T.unpack $
                "item definition '" <> defName <> "': food nutrition \
                \selects BOTH modes — key 'calories' (kcal per item) = "
                <> tshow cal <> " and key 'calories_per_kg' (kcal per \
                \kilogram of fill) = " <> tshow perKg <> " are mutually \
                \exclusive, and the runtime honours only \
                \calories_per_kg, so the authored calories would never \
                \be credited. Author exactly one."
            (False, False) → fail ∘ T.unpack $
                "item definition '" <> defName <> "': food nutrition \
                \selects NO mode — key 'calories' (kcal per item) = "
                <> tshow cal <> " and key 'calories_per_kg' (kcal per \
                \kilogram of fill) = " <> tshow perKg <> ", so eating \
                \this would consume the item and credit nothing. \
                \Exactly one must be strictly positive."
            (False, True)
                | not hasContainer → fail ∘ T.unpack $
                    "item definition '" <> defName <> "': food nutrition \
                    \selects BULK food (key 'calories_per_kg', kcal per \
                    \kilogram of fill) = " <> tshow perKg <> ", but the \
                    \definition authors no container: block. Bulk food \
                    \is drawn from the item's own fill, which is 0 for \
                    \a non-container, so it could never be eaten. Add a \
                    \container: block, or author calories (kcal per \
                    \item) instead."
            _ → pure (ItemYamlFood cal perKg)
    _ → fail ∘ T.unpack $
        "item definition '" <> defName <> "': food must be a block \
        \authoring a nutrition: block with exactly one of calories \
        \(kcal per item) or calories_per_kg (kcal per kilogram of \
        \fill), got " <> tshow val

-- | (min, max) range for a rolled spec — used by both quality and
--   condition. Interpreted as a normal distribution clamped to the
--   range. Reads as `{ min: 50, max: 75 }` in YAML.
data ItemYamlRollSpec = ItemYamlRollSpec
    { iyrsMin ∷ !Float
    , iyrsMax ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlRollSpec where
    parseJSON = withObject "ItemYamlRollSpec" $ \v → ItemYamlRollSpec
        ⊚ v .: "min"
        ⊛ v .: "max"

-- | One override band in an item's `quality_tiers:` list (#345) — a
--   (threshold, label) pair; see 'Item.Types.QualityTier'. Reads as
--   `{ min: 90, label: excellent }` in YAML.
data ItemYamlQualityTier = ItemYamlQualityTier
    { iyqtMin   ∷ !Float
    , iyqtLabel ∷ !Text
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlQualityTier where
    parseJSON = withObject "ItemYamlQualityTier" $ \v → ItemYamlQualityTier
        ⊚ v .: "min"
        ⊛ v .: "label"

-- | Parse and VALIDATE an item definition's optional @quality_tiers:@
--   override (#1739), against the OWNING definition — which is why
--   there is no 'FromJSON' instance doing this and why the instance
--   above stays exactly the bare field decoder it always was. Two
--   things reachable only from out here are load-bearing: the
--   definition's @name@, so every rejection is findable without
--   counting entries in the file (#1233 requirement 2), and whether the
--   definition authors a @quality:@ spec, which decides whether the
--   table can ever take effect at all.
--
--   'Item.Types.qualityTierLabel' replaces 'Item.Types.defaultQualityTiers'
--   WHOLESALE for any non-empty override — the default's 0-floor band
--   is never supplied as a fallback — so an override that cannot label
--   every quality resolves to 'Nothing' over the rest of the range.
--   'Nothing' means "omit the field" at all four reader sites, so a
--   malformed table is indistinguishable from an item that has no tiers
--   at all: the suffix simply disappears, with no error anywhere. Five
--   authoring faults produced exactly that symptom and each is rejected
--   here:
--
--     * __No zero floor.__ @[{min: 80, label: masterwork}]@ labels only
--       the top of the range; quality 50 resolves to nothing.
--     * __A non-finite @min@.__ Two genuinely different spellings, both
--       diagnosed here BY DEFINITION NAME. YAML's @.nan@ / @.inf@
--       resolve to STRINGS (the yaml package's scalar resolver only
--       recognizes ordinary numeric syntax), so they never reach a
--       number at all and are rejected as the non-numeric @min@ they
--       are; a perfectly ordinary @1.0e+100@ is a valid 'Scientific'
--       that becomes 'Infinity' only once narrowed to @iyqtMin@'s
--       32-bit 'Float', and poisons both the resolver's @sortBy@ and
--       its @find@. That is why finiteness is tested AFTER narrowing,
--       exactly as 'requirePositiveQuantity' and 'parseNutritionValue'
--       record — and why this parser decodes a band's fields itself
--       rather than delegating to the 'FromJSON' instance above, whose
--       bare type error would name neither the definition nor the
--       offending value.
--     * __A @min@ outside 0..100.__ 'Item.Types.QualityTier' calls
--       @qtMin@ the inclusive lower bound (0..100) and nothing checked it.
--     * __Duplicate minima.__ @sortBy@ is stable, so two bands sharing a
--       @min@ resolve by AUTHOR ORDER, contradicting the documented
--       highest-band-wins rule that exists to make author order
--       irrelevant. Identity is tested on the NARROWED 'Float' too:
--       @50@ and @50.0000001@ are distinct YAML numbers that collapse to
--       one 'Float' and would tie just as arbitrarily.
--     * __An empty label.__ @label: \"\"@ parsed, resolved to @Just \"\"@,
--       and @scripts\/ui\/quality_tier.lua@ then suppresses the suffix on
--       @it.qualityTier ~= \"\"@ — a second silent path to the same
--       missing suffix. Whitespace-only is the same thing, so the test
--       is on the TRIMMED label.
--
--   An absent key, an explicit @null@, and an explicit @[]@ all mean
--   "no override" and stay exactly as permissive as they have always
--   been — including on a definition with no @quality:@ spec, since
--   there is then no table whose effect could be lost. @null@ is read
--   as absent here rather than rejected the way @storage:@ and @food:@
--   are: those two are CAPABILITY blocks, where silently reading a
--   written key as absent quietly drops a capability, whereas an empty
--   quality-tier override selects the very same default table the key's
--   absence does.
parseItemYamlQualityTiers
    ∷ Text          -- ^ the definition's @name@, for the diagnostic
    → Bool          -- ^ does the definition author a @quality:@ spec?
    → Aeson.Object
    → Aeson.Parser [ItemYamlQualityTier]
parseItemYamlQualityTiers defName hasQuality v =
    case KM.lookup "quality_tiers" v of
        Nothing         → pure []
        Just Aeson.Null → pure []
        Just val        → do
            raws ← case val of
                Aeson.Array arr → pure (V.toList arr)
                _ → bad ("quality tier table (key 'quality_tiers') must \
                         \be a list of { min, label } bands, got "
                         <> tshow val)
            case raws of
                [] → pure []
                _  → do
                    let total = length raws
                    bands ← traverse (parseBand total)
                                     (zip [1 ..] raws)
                    unless hasQuality $ bad
                        ("quality tier table authors " <> tshow total
                         <> " bands, but the definition has no quality: \
                            \spec. Every reader resolves a tier only for an \
                            \item that rolls a quality, so this table could \
                            \never take effect. Author a quality: \
                            \{ min, max } block, or delete the \
                            \quality_tiers: list.")
                    let mins = map iyqtMin bands
                    case firstDuplicate mins of
                        Just m → bad
                            ("quality tier bands share the min " <> tshow m
                             <> " — two bands at one threshold resolve by \
                                \AUTHOR ORDER, and tier resolution is \
                                \defined to pick the highest min a quality \
                                \clears, so the winning label would be \
                                \arbitrary. Give every band a distinct min.")
                        Nothing → pure ()
                    unless (any (≡ 0) mins) $ bad
                        ("quality tier table authors no band with min 0, so \
                         \every quality below its lowest band ("
                         <> tshow (minimum mins) <> ") resolves to NO label \
                            \at all. A non-empty override replaces the \
                            \default table wholesale, so it must carry its \
                            \own floor: author a band with min: 0.")
                    pure bands
  where
    -- Polymorphic in its result so the same helper can abort a
    -- @Parser [Aeson.Value]@, a @Parser Float@ and a @Parser ()@ alike.
    bad ∷ ∀ α. Text → Aeson.Parser α
    bad why = fail ∘ T.unpack $
        "item definition '" <> defName <> "': " <> why

    -- Decoded field by field rather than through the 'FromJSON'
    -- instance above, so that a band's SHAPE faults are diagnosed the
    -- same way its CONTENT faults are. Delegating would surface
    -- @min: .nan@ as a bare aeson type error naming neither the
    -- definition nor the offending value — and @.nan@ / @.inf@ are
    -- precisely the spellings that arrive here as STRINGS, so they are
    -- the ones that need it most. The instance itself stays exactly the
    -- bare decoder it always was; it is a public part of this module's
    -- schema surface and nothing about it changed.
    parseBand ∷ Int → (Int, Aeson.Value) → Aeson.Parser ItemYamlQualityTier
    parseBand total (i, raw) = case raw of
        Aeson.Object o → do
            m ← bandMin o
            l ← bandLabel m o
            pure (ItemYamlQualityTier m l)
        _ → badBand ("must be a { min, label } block, got " <> tshow raw)
      where
        badBand ∷ ∀ α. Text → Aeson.Parser α
        badBand why = bad $
            "quality tier band " <> tshow i <> " of " <> tshow total
            <> " (key 'quality_tiers') " <> why

        -- Finiteness and range are tested AFTER narrowing to the
        -- engine's 32-bit 'Float', exactly as 'requirePositiveQuantity'
        -- and 'parseNutritionValue' record. The two hazards are
        -- genuinely different and both are named here: @.nan@ / @.inf@
        -- never reach a number at all, while an ordinary @1.0e+100@
        -- reaches one and becomes 'Infinity' only on the way in.
        bandMin o = case KM.lookup "min" o of
            Nothing → badBand
                "authors no min — every band needs an inclusive lower \
                \bound, a quality percentage in 0..100"
            Just val@(Aeson.Number s) →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then badBand ("must author a FINITE min, got "
                         <> tshow val <> " which narrows to " <> tshow f
                         <> " — an ordinary YAML number that large \
                            \overflows the engine's 32-bit float, and a \
                            \non-finite threshold poisons tier \
                            \resolution's own sort and search")
                     else if f < 0 ∨ f > 100
                       then badBand ("must author a min within 0..100 \
                            \inclusive — it is a quality percentage — got "
                            <> tshow f <> " instead")
                       else pure f
            Just other → badBand ("must author a NUMERIC min, got "
                <> tshow other <> " — YAML's scalar resolver reads the \
                \not-a-number and infinity spellings as STRINGS rather \
                \than numbers, so neither is a way to author an \
                \unbounded band; a min is a quality percentage in \
                \0..100")

        -- The min is threaded in only so a blank label — which prints
        -- as nothing and so cannot identify its own band — is still
        -- reported against a findable threshold.
        bandLabel m o = case KM.lookup "label" o of
            Nothing → badBand ("with min " <> tshow m <> " authors no \
                \label — every band needs the text its quality range \
                \renders as")
            Just (Aeson.String t)
                | T.null (T.strip t) → badBand ("with min " <> tshow m
                    <> " must author a non-empty label. A blank label \
                       \renders as no tier at all, which is exactly what \
                       \an item with no tiers looks like")
                | otherwise → pure t
            Just other → badBand ("with min " <> tshow m <> " must author \
                \a textual label, got " <> tshow other)

    -- Identity on the NARROWED Float, which is the value the runtime
    -- will actually compare; `sort` is safe here because `parseBand`
    -- has already rejected every NaN.
    firstDuplicate ∷ [Float] → Maybe Float
    firstDuplicate = go ∘ sort
      where
        go (a:b:rest) | a ≡ b     = Just a
                      | otherwise = go (b:rest)
        go _                      = Nothing

-- | One stat-modifier conferred by equipping an item.
--   YAML shape: `{ stat: perception, amount: 1, scales_with_condition: true }`.
--   `percent` is fractional and matches the unit-level modifiers block
--   (0.1 = +10%); a buff can declare `amount`, `percent`, or both
--   (#392). Both default to 0 so either can stand alone.
data ItemYamlBuff = ItemYamlBuff
    { iybStat                ∷ !Text
    , iybAmount              ∷ !Float
    , iybPercent             ∷ !Float
    , iybScalesWithCondition ∷ !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlBuff where
    parseJSON = withObject "ItemYamlBuff" $ \v → ItemYamlBuff
        ⊚ v .:  "stat"
        ⊛ v .:? "amount"  .!= 0.0
        ⊛ v .:? "percent" .!= 0.0
        ⊛ v .:? "scales_with_condition" .!= False

-- | Optional weapon block on an item def. Geometric + material
--   reference; material physical properties live in the
--   SubstanceManager and get joined at use time.
data ItemYamlWeapon = ItemYamlWeapon
    { iywBladeLength    ∷ !Float
    , iywBaseSharpness  ∷ !Float
    , iywStabEff        ∷ !Float
    , iywSlashEff       ∷ !Float
    , iywBluntEff       ∷ !Float
    , iywWeaponClass    ∷ !Text   -- ^ skill name (dagger/unarmed/…)
    , iywAttackCooldown ∷ !Float  -- ^ seconds between swings
    , iywLength         ∷ !Float  -- ^ cm total; 0 ⇒ use blade_length
    , iywCenterOfMass   ∷ !Float  -- ^ 0..1 from grip
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlWeapon where
    parseJSON = withObject "ItemYamlWeapon" $ \v → ItemYamlWeapon
        ⊚ v .:  "blade_length"
        ⊛ v .:  "base_sharpness"
        ⊛ v .:? "stab_effectiveness"   .!= 0
        ⊛ v .:? "slash_effectiveness"  .!= 0
        ⊛ v .:? "blunt_effectiveness"  .!= 0
        ⊛ v .:? "weapon_class"         .!= "unarmed"
        ⊛ v .:? "attack_cooldown"      .!= 1.5
        ⊛ v .:? "length"               .!= 0
        ⊛ v .:? "center_of_mass"       .!= 0.5

-- | Optional armour block on an item def. The protective material is
--   the item's top-level `material`; this adds the thickness and the
--   body parts it covers.
data ItemYamlArmor = ItemYamlArmor
    { iyaThickness ∷ !Float
    , iyaCovers    ∷ ![Text]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlArmor where
    parseJSON = withObject "ItemYamlArmor" $ \v → ItemYamlArmor
        ⊚ v .:? "thickness" .!= 1.0
        ⊛ v .:? "covers"    .!= []

-- | Weight as declared in YAML: a plain number (every instance the
--   same) or @{mean, range}@ for per-instance truncated-normal rolls
--   (raw gems vary per find).
data ItemYamlWeight
    = WeightFixed !Float
    | WeightSpec !Float !Float   -- ^ mean, range
    deriving (Show, Eq, Generic)

instance FromJSON ItemYamlWeight where
    parseJSON v = case v of
        Aeson.Object o → WeightSpec
            ⊚ o .:  "mean"
            ⊛ o .:? "range" .!= 0.0
        _ → WeightFixed <$> parseJSON v

data ItemYamlDef = ItemYamlDef
    { iydName        ∷ !Text
    , iydDisplayName ∷ !Text
    , iydSprite      ∷ !Text                       -- ^ texture path
    , iydWeight      ∷ !ItemYamlWeight             -- ^ empty weight (kg)
    , iydBulk        ∷ !Float                      -- ^ external bulk (litres),
                                                   --   REQUIRED: finite,
                                                   --   strictly positive, no
                                                   --   default (#1233)
    , iydKind        ∷ !Text                       -- ^ equipment slot kind;
                                                   --   defaults to "misc"
    , iydCategory    ∷ !Text                       -- ^ inventory tab;
                                                   --   defaults to "Misc"
    , iydMake        ∷ !Text                       -- ^ crafting tradition;
                                                   --   defaults to ""
    , iydMaterial    ∷ !Text                       -- ^ substance name;
                                                   --   defaults to ""
    , iydQuality     ∷ !(Maybe ItemYamlRollSpec)   -- ^ quality roll range
    , iydQualityTiers ∷ ![ItemYamlQualityTier]     -- ^ quality→label
                                                   --   overrides (#345);
                                                   --   [] ⇒ default set.
                                                   --   A non-empty list is
                                                   --   validated against
                                                   --   'parseItemYamlQualityTiers'
                                                   --   (#1739), so every value
                                                   --   here labels the whole
                                                   --   0..100 range
    , iydContainer   ∷ !(Maybe ItemYamlContainer)
    , iydContents    ∷ ![ItemYamlContent]            -- ^ item-container defaults
    , iydStorage     ∷ !(Maybe ItemYamlStorage)      -- ^ portable item-storage
                                                     --   capacity (#1233)
    , iydFood        ∷ !(Maybe ItemYamlFood)      -- ^ nutrition; validated
                                                 --   against the two
                                                 --   mutually exclusive
                                                 --   modes and against
                                                 --   'iydContainer' (#1716)
    , iydWeapon      ∷ !(Maybe ItemYamlWeapon)
    , iydArmor       ∷ !(Maybe ItemYamlArmor)
    , iydUnequippable ∷ !Bool
    , iydBuffs       ∷ ![ItemYamlBuff]
    , iydInsulation  ∷ !Float                      -- ^ thermal insulation when
                                                   --   worn (slows heat loss);
                                                   --   defaults to 0
    } deriving (Show, Eq, Generic)

-- | Every entry under @items:@ is a PHYSICAL item — this schema has no
--   abstract/incorporeal item class — so @bulk@ is unconditionally
--   required (#1233 requirement 1) and the definition is rejected
--   outright when it is missing or not a finite positive number
--   (requirement 2).
--
--   The name is read FIRST and the two physical blocks parsed
--   monadically against it, so every rejection names the offending
--   definition rather than a bare JSON index; everything else keeps its
--   original applicative shape and its original defaults.
--
--   @storage:@ is read with an explicit key lookup rather than @.:?@,
--   because those two disagree about the one case that matters here:
--   @.:?@ reports an explicit @storage: null@ (or @~@, or a bare
--   @storage:@ with no value) as ABSENT, which would accept a definition
--   that visibly authored the block as though it had never mentioned it —
--   quietly turning "this crate stores things" into "this crate does
--   not". A key the author WROTE is present, so a null value is a
--   half-authored block and fails like any other invalid one
--   (requirement 3), while a truly missing key stays the legitimate
--   optional case. Same trap CLAUDE.md records for @asset_units:@.
--
--   @food:@ is parsed by 'parseItemYamlFood' rather than a 'FromJSON'
--   instance, and monadically AFTER @container:@, because #1716's
--   mutual-exclusion and bulk-without-container rules need the
--   definition's own name for the diagnostic and its sibling
--   @container:@ field for the correlation — neither of which an
--   instance can reach.
--
--   @quality_tiers:@ is likewise parsed by
--   'parseItemYamlQualityTiers' and monadically AFTER @quality:@,
--   because #1739's requirement 5 is a correlation between those two
--   sibling keys. An absent, null, or empty list still decodes to @[]@
--   exactly as @.:? \"quality_tiers\" .!= []@ did.
--
--   @condition:@ is REJECTED outright for every value, @null@ included
--   (#1421). Condition stopped being authorable when it became pure
--   runtime wear state, and merely dropping the field would let aeson
--   ignore a retired key that an author still believed was doing
--   something — a definition asking for 70--100 would silently spawn at
--   100 instead. An explicit lookup is the only way to see the key at
--   all, for the same reason @storage:@ needs one.
instance FromJSON ItemYamlDef where
    parseJSON = withObject "ItemYamlDef" $ \v → do
        name    ← v .: "name"
        bulk    ← requirePositiveQuantity name "bulk" "litres" v "bulk"
        storage ← case KM.lookup "storage" v of
            Nothing         → pure Nothing
            Just Aeson.Null → fail ∘ T.unpack $
                "item definition '" <> name <> "': storage is present but \
                \null — a storage: block must author both a positive \
                \weight_capacity (kilograms) and a positive bulk_capacity \
                \(litres); omit the key entirely for an item that is not \
                \portable storage"
            Just val        → Just <$> parseItemYamlStorage name val
        case KM.lookup "condition" v of
            Nothing → pure ()
            Just _  → fail ∘ T.unpack $
                "item definition '" <> name <> "': condition is no longer \
                \an item property (#1421) — condition is runtime wear \
                \state, every freshly made item starts at 100, and only \
                \the ground-salvage path (item.spawnGround) starts one \
                \worn. Delete the condition: block."
        -- `container:` and `food:` are read HERE, monadically and in
        -- this order, because #1716's bulk-without-container rule is a
        -- CORRELATION between two sibling keys and the applicative
        -- chain below cannot express one.
        --
        -- The two keys treat an explicit null DIFFERENTLY, and both
        -- readings are deliberate. `container:` keeps `.:?`, so a null
        -- is an ABSENT container: that is exactly what the runtime will
        -- see (`iydContainer` is the field it reads), so a bulk food
        -- authoring `container: null` must fail as bulk-WITHOUT-
        -- container, naming the real problem, rather than as some
        -- separate container-schema fault. `food: null` is rejected
        -- instead, for the reason `storage:` above is: a key the author
        -- WROTE is present, and silently reading it as "this item is
        -- not edible" is the same quiet capability drop.
        container ← v .:? "container"
        food ← case KM.lookup "food" v of
            Nothing         → pure Nothing
            Just Aeson.Null → fail ∘ T.unpack $
                "item definition '" <> name <> "': food is present but \
                \null — a food: block must author a nutrition: block \
                \with exactly one of calories (kcal per item) or \
                \calories_per_kg (kcal per kilogram of fill) strictly \
                \positive; omit the key entirely for an item that is \
                \not edible"
            Just val → Just <$> parseItemYamlFood name (isJust container) val
        -- `quality:` is read HERE, monadically and before
        -- `quality_tiers:`, for the same reason `container:` is read
        -- before `food:`: #1739's requirement 5 is a CORRELATION
        -- between two sibling keys, and the applicative chain below
        -- cannot express one. `.:?` is the right reading of a null
        -- `quality:` because `iydQuality` is the field the runtime
        -- sees, so a table authored beside `quality: null` must fail
        -- as a table with no quality spec — naming the real problem.
        quality ← v .:? "quality"
        qualityTiers ← parseItemYamlQualityTiers name (isJust quality) v
        ItemYamlDef name
            ⊚ v .:? "display_name" .!= ""
            ⊛ v .:  "sprite"
            ⊛ v .:? "weight"       .!= WeightFixed 0.0
            ⊛ pure bulk
            ⊛ v .:? "kind"         .!= "misc"
            ⊛ v .:? "category"     .!= "Misc"
            ⊛ v .:? "make"         .!= ""
            ⊛ v .:? "material"     .!= ""
            ⊛ pure quality
            ⊛ pure qualityTiers
            ⊛ pure container
            ⊛ v .:? "contents"     .!= []
            ⊛ pure storage
            ⊛ pure food
            ⊛ v .:? "weapon"
            ⊛ v .:? "armor"
            ⊛ v .:? "unequippable" .!= False
            ⊛ v .:? "buffs"        .!= []
            ⊛ v .:? "insulation"   .!= 0.0

newtype ItemYamlFile = ItemYamlFile
    { iyfItems ∷ [ItemYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlFile where
    parseJSON = withObject "ItemYamlFile" $ \v → ItemYamlFile
        ⊚ v .: "items"

-- | 'loadItemYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadItemYaml'.
loadItemYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [ItemYamlDef])
loadItemYamlOutcome logger =
    loadYamlListOutcome logger "item" "item definitions" iyfItems

loadItemYaml ∷ LoggerState → FilePath → IO [ItemYamlDef]
loadItemYaml logger path = fromMaybe [] ⊚ loadItemYamlOutcome logger path
