-- | Strict decoding of the @gameplayTags@ Lua arguments (#2706, SCN-06 of
--   epic #2698): targets, tag collections, query expressions and query
--   options.
--
--   Every read checks the value's actual Lua type before converting it,
--   and every table is walked RAW ('Lua.next', 'Lua.rawget',
--   'Lua.rawgeti'), so no metamethod ever runs while a request is
--   decoded and a numeric string is never coerced into a number. The
--   decoder only reads: a refusal is reported before anything is
--   mutated or evaluated, and the verb restores the Lua stack whichever
--   way decoding ends.
--
--   == Integers
--
--   'Lua.ltype' reports 'Lua.TypeNumber' for both number subtypes, and
--   'Lua.tointeger' accepts numeric strings and integral floats, so
--   neither alone is the rule. The rule is:
--
--     * a string is refused outright (the type is checked first);
--     * an INTEGER-subtype value is accepted at its full 64-bit value,
--       which is what keeps a flora occurrence id or an item id above
--       2^53 exact;
--     * a FLOAT is accepted only when it is finite, integral and at most
--       2^53 in magnitude — the range where a float names exactly one
--       integer — and refused otherwise (fractions, NaN, infinities and
--       large floats alike).
--
--   == Expressions
--
--   An expression is a tree of tables (see 'readExpression'). Decoding
--   is bounded: at most 'maxExpressionDepth' nested levels and at most
--   'maxExpressionElements' visited elements (each node, and each tag
--   inside a filter, counted every time it is reached, so a table shared
--   between operands counts once per use). A table that is its own
--   ancestor is refused as cyclic — the check compares table identities
--   along the current path, so a table legitimately shared between two
--   operands is not mistaken for a cycle. Exceeding a bound returns a
--   diagnostic; an expression is never truncated or reinterpreted.
module Engine.Scripting.Lua.API.GameplayTags.Args
    ( -- * The decoder
      Dec
    , runDec
    , refuse
      -- * Values
    , readInteger
    , readText
      -- * Targets
    , TargetRef(..)
    , readTargetRef
      -- * Tags
    , readTag
    , readTagSet
      -- * Queries
    , readExpression
    , maxExpressionDepth
    , maxExpressionElements
    , QueryOptions(..)
    , readQueryOptions
    , categoryName
    ) where

import UPrelude
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Gameplay.Tags.Query (TagQuery(..), TagFilter(..), tagged, unionQueries, intersectQueries)
import Gameplay.Tags.Types
import World.Flora.Identity (FloraInstanceId, floraInstanceIdFromLua)
import World.Page.Types (WorldPageId(..))

-- * The decoder

-- | A decoding step: a Lua action that either produces a value or
--   refuses with a diagnostic.
newtype Dec α = Dec (ExceptT Text (Lua.LuaE Lua.Exception) α)
    deriving newtype (Functor, Applicative, Monad)

runDec ∷ Dec α → Lua.LuaE Lua.Exception (Either Text α)
runDec (Dec m) = runExceptT m

dec ∷ Lua.LuaE Lua.Exception (Either Text α) → Dec α
dec = Dec . ExceptT

lua ∷ Lua.LuaE Lua.Exception α → Dec α
lua = Dec . lift

refuse ∷ Text → Dec α
refuse = Dec . throwError

io ∷ IO α → Dec α
io = lua . Lua.liftIO

-- * Values

-- | An integer under the rule in the module haddock.
readInteger ∷ Text → Lua.StackIndex → Dec Int64
readInteger what idx = do
    ty ← lua (Lua.ltype idx)
    unless (ty ≡ Lua.TypeNumber) $ refuse (what <> " must be an integer, got " <> typeWord ty)
    isInt ← lua (Lua.isinteger idx)
    if isInt
        then lua (Lua.tointeger idx) ≫= \case
            Just (Lua.Integer n) → pure n
            Nothing → refuse (what <> " must be an integer")
        else lua (Lua.tonumber idx) ≫= \case
            Just (Lua.Number d)
                | not (isNaN d ∨ isInfinite d)
                , abs d ≤ 9007199254740992
                , d ≡ fromIntegral (truncate d ∷ Int64) → pure (truncate d)
            _ → refuse (what <> " must be an integer; a float must be finite, integral \
                         \and at most 2^53 in magnitude")

-- | An integer in @[lo, hi]@.
readIntegerIn ∷ Text → Int64 → Int64 → Lua.StackIndex → Dec Int64
readIntegerIn what lo hi idx = do
    n ← readInteger what idx
    unless (n ≥ lo ∧ n ≤ hi) $
        refuse (what <> " is out of range " <> tshow lo <> " .. " <> tshow hi)
    pure n

-- | A string that is valid UTF-8. Never converts a number.
readText ∷ Text → Lua.StackIndex → Dec Text
readText what idx = do
    ty ← lua (Lua.ltype idx)
    unless (ty ≡ Lua.TypeString) $ refuse (what <> " must be a string, got " <> typeWord ty)
    bytes ← lua (Lua.tostring idx)
    case TE.decodeUtf8' (fromMaybe BS.empty bytes) of
        Left _  → refuse (what <> " is not valid UTF-8")
        Right t → pure t

typeWord ∷ Lua.Type → Text
typeWord = \case
    Lua.TypeNone          → "nothing"
    Lua.TypeNil           → "nil"
    Lua.TypeBoolean       → "a boolean"
    Lua.TypeLightUserdata → "a userdata"
    Lua.TypeNumber        → "a number"
    Lua.TypeString        → "a string"
    Lua.TypeTable         → "a table"
    Lua.TypeFunction      → "a function"
    Lua.TypeUserdata      → "a userdata"
    Lua.TypeThread        → "a thread"

-- * Tables

requireTable ∷ Text → Lua.StackIndex → Dec Lua.StackIndex
requireTable what idx = do
    ty ← lua (Lua.ltype idx)
    unless (ty ≡ Lua.TypeTable) $ refuse (what <> " must be a table, got " <> typeWord ty)
    ok ← lua (Lua.checkstack 8)
    unless ok $ refuse (what <> ": the Lua stack cannot grow")
    lua (Lua.absindex idx)

-- | The table's keys, raw. Every key must be a string naming one of
--   @allowed@; the first that is not is refused by name.
fieldNames ∷ Text → [Text] → Lua.StackIndex → Dec [Text]
fieldNames what allowed idx = do
    t ← requireTable what idx
    lua Lua.pushnil
    let go acc = lua (Lua.next t) ≫= \case
            False → pure (reverse acc)
            True → do
                kty ← lua (Lua.ltype (-2))
                if kty ≢ Lua.TypeString
                    then lua (Lua.pop 2) *> refuse (what <> " has a key that is " <> typeWord kty
                                                     <> "; only named fields are allowed")
                    else do
                        k ← lua (Lua.tostring (-2))
                        let name = TE.decodeUtf8' (fromMaybe BS.empty k)
                        case name of
                            Right n | n ∈ allowed → lua (Lua.pop 1) *> go (n : acc)
                            Right n → lua (Lua.pop 2) *> refuse
                                (what <> " has an unknown field '" <> n <> "'")
                            Left _ → lua (Lua.pop 2) *> refuse
                                (what <> " has a field name that is not valid UTF-8")
    go []

-- | Decode the raw field @name@ of the table at @t@ (an absolute index).
withField ∷ Lua.StackIndex → Text → (Lua.StackIndex → Dec α) → Dec α
withField t name act = dec $ do
    Lua.pushstring (TE.encodeUtf8 name)
    _ ← Lua.rawget t
    r ← runDec (act (-1))
    Lua.pop 1
    pure r

-- | An optional raw field: 'Nothing' when absent (or nil).
optionalField ∷ Lua.StackIndex → Text → (Lua.StackIndex → Dec α) → Dec (Maybe α)
optionalField t name act = withField t name $ \i → do
    ty ← lua (Lua.ltype i)
    if ty ≡ Lua.TypeNil then pure Nothing else Just ⊚ act i

-- | A raw SEQUENCE: every key an integer-subtype 1 … n with no gaps and
--   no other keys, decoded in order.
readSequence ∷ Text → Lua.StackIndex → (Int → Lua.StackIndex → Dec α) → Dec [α]
readSequence what idx each = do
    t ← requireTable what idx
    n ← lua (Lua.rawlen t)
    lua Lua.pushnil
    let keys (count ∷ Int) = lua (Lua.next t) ≫= \case
            False → pure count
            True → do
                isInt ← lua (Lua.isinteger (-2))
                k ← if isInt then lua (Lua.tointeger (-2)) else pure Nothing
                case k of
                    Just (Lua.Integer i) | i ≥ 1, fromIntegral i ≤ n →
                        lua (Lua.pop 1) *> keys (count + 1)
                    _ → lua (Lua.pop 2) *> refuse
                            (what <> " must be a list: only the keys 1 .. n are allowed")
    count ← keys 0
    unless (count ≡ n) $ refuse (what <> " must be a list without gaps")
    forM [1 .. n] $ \i → dec $ do
        _ ← Lua.rawgeti t (fromIntegral i)
        r ← runDec (each i (-1))
        Lua.pop 1
        pure r

-- * Targets

-- | A target as the script named it, before its page is resolved and
--   its coordinates canonicalised against the live page.
data TargetRef
    = RefUnit !Word32
    | RefBuilding !Word32
    | RefItem !Word64
    | RefLocation !(Maybe WorldPageId) !Int
    | RefPlant !(Maybe WorldPageId) !FloraInstanceId
    | RefPlot !(Maybe WorldPageId) !Int !Int
    | RefTile !(Maybe WorldPageId) !Int !Int !Int
    deriving (Show, Eq)

-- | Tile coordinates are bounded to the 32-bit range, far beyond any
--   page, so canonicalisation can never overflow.
coordMin, coordMax ∷ Int64
coordMin = -2147483648
coordMax = 2147483647

-- | A target table: @{ kind = …, … }@ with exactly the kind's fields.
--   See the reference for the encodings.
readTargetRef ∷ Lua.StackIndex → Dec TargetRef
readTargetRef idx = do
    _ ← fieldNames "a target" ["kind", "id", "page", "x", "y", "z"] idx
    t ← lua (Lua.absindex idx)
    kind ← withField t "kind" (readText "a target's kind")
    let fields = case kind of
            "unit"     → Just ["kind", "id"]
            "building" → Just ["kind", "id"]
            "item"     → Just ["kind", "id"]
            "location" → Just ["kind", "id", "page"]
            "plant"    → Just ["kind", "id", "page"]
            "plot"     → Just ["kind", "page", "x", "y"]
            "tile"     → Just ["kind", "page", "x", "y", "z"]
            _          → Nothing
    allowed ← maybe (refuse ("unknown target kind '" <> kind <> "'; expected unit, building, \
                            \location, plant, plot, item or tile")) pure fields
    let target = article kind <> " target"
    present ← fieldNames target allowed t
    let required = filter (≢ "page") allowed
    forM_ required $ \f → unless (f ∈ present) $
        refuse (target <> " is missing its '" <> f <> "' field")
    let int f lo hi = withField t f (readIntegerIn (target <> "'s " <> f) lo hi)
        coord f = fromIntegral ⊚ int f coordMin coordMax
        page = optionalField t "page" (readPage (target <> "'s page"))
    case kind of
        "unit"     → RefUnit . fromIntegral ⊚ int "id" 0 4294967295
        "building" → RefBuilding . fromIntegral ⊚ int "id" 0 4294967295
        "item"     → RefItem . fromIntegral ⊚ int "id" 1 maxBound
        "location" → RefLocation <$> page <*> (fromIntegral ⊚ int "id" minBound maxBound)
        "plant"    → do
            p ← page
            n ← int "id" 1 maxBound
            case floraInstanceIdFromLua n of
                Nothing  → refuse "a plant target's id is not a flora occurrence id"
                Just fid → pure (RefPlant p fid)
        "plot"     → RefPlot <$> page <*> coord "x" <*> coord "y"
        _          → RefTile <$> page <*> coord "x" <*> coord "y" <*> coord "z"

-- | "a unit", "an item".
article ∷ Text → Text
article w = (if w ≡ "item" then "an " else "a ") <> w

readPage ∷ Text → Lua.StackIndex → Dec WorldPageId
readPage what idx = do
    p ← readText what idx
    when (T.null p) $ refuse (what <> " must not be empty")
    pure (WorldPageId p)

-- * Tags

-- | One tag: a nonempty UTF-8 string ('mkGameplayTag''s rule).
readTag ∷ Lua.StackIndex → Dec GameplayTag
readTag idx = do
    t ← readText "a tag" idx
    maybe (refuse "a tag must not be the empty string") pure (mkGameplayTag t)

-- | A tag collection: one tag string, or a list of them. Repeats are
--   one tag; the empty list is the empty set.
readTagSet ∷ Lua.StackIndex → Dec (Set.Set GameplayTag)
readTagSet = readTagSetWith (pure ())

-- | 'readTagSet', running @each@ before EVERY input element is decoded —
--   repeats included, and before deduplication — so a bound it enforces
--   stops the traversal itself.
readTagSetWith ∷ Dec () → Lua.StackIndex → Dec (Set.Set GameplayTag)
readTagSetWith each idx = do
    ty ← lua (Lua.ltype idx)
    case ty of
        Lua.TypeString → each *> (Set.singleton ⊚ readTag idx)
        Lua.TypeTable  → Set.fromList ⊚ readSequence "a tag list" idx (\_ i → each *> readTag i)
        _ → refuse ("tags must be a string or a list of strings, got " <> typeWord ty)

-- * Expressions

-- | The deepest an expression may nest.
maxExpressionDepth ∷ Int
maxExpressionDepth = 64

-- | The most elements one expression may visit (nodes and filter tags,
--   each counted every time it is reached).
maxExpressionElements ∷ Int
maxExpressionElements = 65536

-- | A query expression (see the reference for the grammar):
--
--   > { tag = "t" }
--   > { all = tags, any = tags, none = tags }    -- any nonempty subset of the three
--   > { universe = true }
--   > { union = { e, … } }  { intersection = { e, … } }    -- one or more operands
--   > { difference = { a, b } }                            -- exactly two
--   > { complement = e }
readExpression ∷ Lua.StackIndex → Dec TagQuery
readExpression idx = do
    budget ← io (newIORef 0)
    expression budget [] 0 idx

expressionKeys ∷ [Text]
expressionKeys = ["tag", "all", "any", "none", "universe", "union", "intersection", "difference", "complement"]

spend ∷ IORef Int → Dec ()
spend budget = do
    n ← io (readIORef budget)
    when (n ≥ maxExpressionElements) $
        refuse ("the expression visits more than " <> tshow maxExpressionElements <> " elements")
    io (writeIORef budget (n + 1))

-- | Enter a table on the current path, refusing a cycle.
enter ∷ Text → [Ptr ()] → Lua.StackIndex → Dec (Lua.StackIndex, [Ptr ()])
enter what path idx = do
    t ← requireTable what idx
    p ← lua (Lua.topointer t)
    when (p ∈ path) $ refuse "the expression is cyclic: a table contains itself"
    pure (t, p : path)

expression ∷ IORef Int → [Ptr ()] → Int → Lua.StackIndex → Dec TagQuery
expression budget path0 depth idx = do
    when (depth ≥ maxExpressionDepth) $
        refuse ("the expression nests deeper than " <> tshow maxExpressionDepth <> " levels")
    spend budget
    (t, path) ← enter "an expression" path0 idx
    keys ← fieldNames "an expression" expressionKeys t
    let sub = expression budget path (depth + 1)
        operands what = withField t what $ \i → do
            (arr, path') ← enter ("the " <> what <> " operand list") path i
            readSequence ("the " <> what <> " operand list") arr
                (\_ e → expression budget path' (depth + 1) e)
        filterTags f = fromMaybe Set.empty ⊚ optionalField t f (countedTagSet budget)
    case sort keys of
        ["tag"] → tagged ⊚ withField t "tag" readTag
        ["universe"] → withField t "universe" $ \i → do
            ty ← lua (Lua.ltype i)
            b ← lua (Lua.toboolean i)
            unless (ty ≡ Lua.TypeBoolean ∧ b) $ refuse "universe must be true"
            pure QUniverse
        ["union"] → operands "union" ≫= \case
            [] → refuse "union needs at least one operand"
            qs → pure (unionQueries qs)
        ["intersection"] → operands "intersection" ≫= \case
            [] → refuse "intersection needs at least one operand"
            qs → pure (intersectQueries qs)
        ["difference"] → operands "difference" ≫= \case
            [a, b] → pure (QDifference a b)
            _ → refuse "difference needs exactly two operands"
        ["complement"] → QComplement ⊚ withField t "complement" sub
        ks | not (null ks), all (∈ ["all", "any", "none"]) ks →
            QMatch <$> (TagFilter <$> filterTags "all" <*> filterTags "any" <*> filterTags "none")
        _ → refuse "an expression node takes exactly one of: tag, all/any/none, universe, \
                   \union, intersection, difference, complement"

-- | 'readTagSet', each input element spending one element of the budget
--   as it is reached.
countedTagSet ∷ IORef Int → Lua.StackIndex → Dec (Set.Set GameplayTag)
countedTagSet budget = readTagSetWith (spend budget)

-- * Query options

-- | Which categories a query ranges over, and on which page.
data QueryOptions = QueryOptions
    { qoCategories ∷ ![TagCategory]
    , qoPage       ∷ !(Maybe WorldPageId)
      -- ^ 'Nothing' for the active page.
    } deriving (Show, Eq)

categoryName ∷ TagCategory → Text
categoryName = \case
    UnitCategory     → "unit"
    BuildingCategory → "building"
    LocationCategory → "location"
    PlantCategory    → "plant"
    ItemCategory     → "item"
    TileCategory     → "tile"

-- | @nil@ or @{ categories = …, page = … }@. Categories are one name or
--   a nonempty list of names; omitted means every category.
readQueryOptions ∷ Lua.StackIndex → Dec QueryOptions
readQueryOptions idx = do
    ty ← lua (Lua.ltype idx)
    if ty ∈ [Lua.TypeNone, Lua.TypeNil] then pure (QueryOptions allTagCategories Nothing) else do
        _ ← fieldNames "query options" ["categories", "page"] idx
        t ← lua (Lua.absindex idx)
        cats ← optionalField t "categories" readCategories
        page ← optionalField t "page" (readPage "the query page")
        pure (QueryOptions (fromMaybe allTagCategories cats) page)

readCategories ∷ Lua.StackIndex → Dec [TagCategory]
readCategories idx = do
    ty ← lua (Lua.ltype idx)
    names ← case ty of
        Lua.TypeString → (: []) ⊚ readText "a category" idx
        Lua.TypeTable  → readSequence "the category list" idx (const (readText "a category"))
        _ → refuse ("categories must be a category name or a list of them, got " <> typeWord ty)
    when (null names) $ refuse "the category list must name at least one category"
    forM names $ \n → case [ c | c ← allTagCategories, categoryName c ≡ n ] of
        (c : _) → pure c
        [] → refuse ("unknown category '" <> n <> "'; expected unit, building, location, \
                     \plant, item or tile")
