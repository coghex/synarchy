-- | The declarative registration contract for the Haskell/Lua
--   boundary (#2479, epic #1995 LAC-2 decision D-3).
--
--   'Engine.Scripting.Lua.API.Internal.registerLuaFunction' carries a
--   raw name and an action: arity, argument kinds and return shape are
--   implied by the action's body and discoverable by nothing. #1996
--   closed the NAME half of that gap with a blocking gate; this module
--   is the descriptor the other half needs — a plain Haskell record,
--   not a manifest and not generated bindings, so it sits beside the
--   action it describes and moves with it.
--
--   It is METADATA, not a validator. Nothing here runs at call time:
--   'Engine.Scripting.Lua.API.Internal.registerLuaVerb' installs the
--   action exactly as 'registerLuaFunction' does and never consults
--   the descriptor's arguments or return shape. A verb whose descriptor
--   is wrong misdescribes itself; it does not misbehave. That is
--   deliberate — this slice is a representation change, and adding
--   runtime argument checking would change what every UI verb accepts.
--
--   The shapes below are sized to the surface they must describe, not
--   to Lua in general (see #2479's spec amendments):
--
--     * zero results ('ReturnsNothing') is distinct from one nil result
--       (@'ReturnsValues' [… 'TNullable' …]@) — @UI.setPosition@ pushes
--       nothing, while @UI.getTextInput@ always pushes exactly one value
--       that may be nil.
--     * ordered multiple results — @UI.placePopup@ pushes @x, y,
--       flipped@ as three bare values with no table, and
--       @UI.findHoverTarget@ pushes two nullable ones.
--     * named fields with their own kinds ('TRecord'), nested nullable
--       records ('TNullable' of a 'TRecord', e.g. @getElementInfo@'s
--       @effectiveClip@), and arrays of records ('TArray' of a
--       'TRecord', e.g. @UI.getVisibleElements@).
module Engine.Scripting.Lua.API.Descriptor
  ( -- * The descriptor
    LuaVerb(..)
  , LuaArg(..)
  , LuaType(..)
  , LuaField(..)
  , LuaResult(..)
  , LuaReturn(..)
    -- * Construction
  , luaVerb
  , argReq
  , argOpt
  , retNone
  , retVals
  , resVal
  , recField
    -- * Derived facts
  , verbArity
  , verbRequiredArity
  , returnArity
  , verbMalformations
  ) where

import UPrelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

-- | A value crossing the boundary, described as precisely as the UI
--   surface needs. Used for both argument kinds and result shapes: a
--   tooltip content table is as much a record as @getElementInfo@'s
--   result is.
data LuaType
  = TString                 -- ^ read with @Lua.tostring@, which also
                            --   accepts a number
  | TNumber                 -- ^ an argument read with @Lua.tonumber@;
                            --   a result pushed with @Lua.pushnumber@,
                            --   so it carries Lua 5.4's FLOAT subtype
                            --   (@math.type@ reports @\"float\"@)
  | TInteger                -- ^ an argument read with @Lua.tointeger@,
                            --   which also accepts a numeric string
                            --   (#1497); a result pushed with
                            --   @Lua.pushinteger@, so it carries Lua
                            --   5.4's INTEGER subtype (@math.type@
                            --   reports @\"integer\"@)
  | TBoolean                -- ^ read with @Lua.toboolean@, which
                            --   coerces rather than validating: any
                            --   value but @nil@/@false@ is true
  | TNullable LuaType       -- ^ the value, or Lua @nil@
  | TRecord [LuaField]      -- ^ a table with named fields
  | TArray LuaType          -- ^ a 1-based sequence of one element type
  deriving (Eq, Show)

-- | One named field of a 'TRecord'.
data LuaField = LuaField
  { fieldName ∷ Text
  , fieldType ∷ LuaType
  , fieldDoc  ∷ Text
  } deriving (Eq, Show)

-- | One positional argument. Position is the list index in
--   'verbArgs' — the order is the Lua call order, always.
data LuaArg = LuaArg
  { argName     ∷ Text
  , argType     ∷ LuaType
  , argOptional ∷ Bool
    -- ^ True when the verb accepts the argument omitted or @nil@.
    --   'argDoc' says what it falls back to.
  , argDoc      ∷ Text
  } deriving (Eq, Show)

-- | One pushed result. 'resultName' is documentation, not a Lua table
--   key: these are bare stack values in order.
data LuaResult = LuaResult
  { resultName ∷ Text
  , resultType ∷ LuaType
  , resultDoc  ∷ Text
  } deriving (Eq, Show)

-- | What a verb pushes. 'ReturnsValues' is never empty — zero results
--   is 'ReturnsNothing', which is what keeps "pushes nothing" and
--   "pushes one nil" distinguishable. 'retVals' enforces that;
--   'verbMalformations' reports a hand-built violation.
data LuaReturn
  = ReturnsNothing
  | ReturnsValues [LuaResult]
  deriving (Eq, Show)

-- | One verb's whole contract: the name it is installed under, its
--   positional arguments, what it pushes, and prose.
data LuaVerb = LuaVerb
  { verbName    ∷ BS.ByteString
  , verbArgs    ∷ [LuaArg]
  , verbReturns ∷ LuaReturn
  , verbDoc     ∷ Text
  } deriving (Eq, Show)

-- | Build a descriptor. The only constructor registrar modules use, so
--   the audit gate (@tools/lua_registration_audit.py@) has exactly one
--   spelling to recognize.
luaVerb ∷ BS.ByteString → [LuaArg] → LuaReturn → Text → LuaVerb
luaVerb = LuaVerb

-- | A required positional argument: omitting it changes what the verb
--   does (usually to nothing at all).
argReq ∷ Text → LuaType → Text → LuaArg
argReq name ty doc = LuaArg name ty False doc

-- | An argument the verb accepts omitted or @nil@. 'argDoc' must say
--   what it falls back to.
argOpt ∷ Text → LuaType → Text → LuaArg
argOpt name ty doc = LuaArg name ty True doc

-- | Pushes nothing at all.
retNone ∷ LuaReturn
retNone = ReturnsNothing

-- | Pushes these values, in this order. An empty list is
--   'ReturnsNothing' rather than a degenerate 'ReturnsValues'.
retVals ∷ [LuaResult] → LuaReturn
retVals [] = ReturnsNothing
retVals rs = ReturnsValues rs

resVal ∷ Text → LuaType → Text → LuaResult
resVal = LuaResult

recField ∷ Text → LuaType → Text → LuaField
recField = LuaField

-- | How many positional arguments the verb reads at all.
verbArity ∷ LuaVerb → Int
verbArity = length ∘ verbArgs

-- | How many leading arguments are required. Optional arguments are
--   trailing across this surface, so this is a count and not a mask;
--   'verbMalformations' rejects a descriptor where it would not be.
verbRequiredArity ∷ LuaVerb → Int
verbRequiredArity = length ∘ filter (not ∘ argOptional) ∘ verbArgs

-- | How many values the verb pushes.
returnArity ∷ LuaReturn → Int
returnArity ReturnsNothing     = 0
returnArity (ReturnsValues rs) = length rs

-- | Every structural problem with a descriptor, as prose. Empty means
--   well-formed. This is what a test asserts over the whole registered
--   set; it deliberately says nothing about whether the metadata
--   MATCHES the action, which only calling the verb can establish.
verbMalformations ∷ LuaVerb → [Text]
verbMalformations verb = concat
    [ [ "verb name is empty" | BS.null (verbName verb) ]
    , [ "verb documentation is empty" | T.null (T.strip (verbDoc verb)) ]
    , [ "ReturnsValues with no values (use ReturnsNothing)"
      | verbReturns verb ≡ ReturnsValues [] ]
    , [ "optional argument " <> argName a <> " precedes required " <> argName b
      | (a, b) ← optionalBeforeRequired (verbArgs verb) ]
    , concatMap argProblems (verbArgs verb)
    , concatMap resultProblems (returnValues (verbReturns verb))
    , duplicates "argument" (map argName (verbArgs verb))
    ]
  where
    returnValues ReturnsNothing     = []
    returnValues (ReturnsValues rs) = rs

    optionalBeforeRequired args =
        [ (a, b)
        | (i, a) ← zip [0 ∷ Int ..] args, argOptional a
        , b ← drop (i + 1) args, not (argOptional b)
        ]

    argProblems a = concat
        [ [ "argument with an empty name" | T.null (T.strip (argName a)) ]
        , [ "argument " <> argName a <> " is undocumented"
          | T.null (T.strip (argDoc a)) ]
        , typeProblems ("argument " <> argName a) (argType a)
        ]

    resultProblems r = concat
        [ [ "result with an empty name" | T.null (T.strip (resultName r)) ]
        , [ "result " <> resultName r <> " is undocumented"
          | T.null (T.strip (resultDoc r)) ]
        , typeProblems ("result " <> resultName r) (resultType r)
        ]

    typeProblems ctx ty = case ty of
        TRecord []  → [ctx <> " is a record with no fields"]
        TRecord fs  → concat
            [ concatMap (fieldProblems ctx) fs
            , duplicates ("field of " <> ctx) (map fieldName fs)
            ]
        TNullable t → typeProblems ctx t
        TArray t    → typeProblems ctx t
        _           → []

    fieldProblems ctx f = concat
        [ [ ctx <> " has a field with an empty name"
          | T.null (T.strip (fieldName f)) ]
        , [ ctx <> " field " <> fieldName f <> " is undocumented"
          | T.null (T.strip (fieldDoc f)) ]
        , typeProblems (ctx <> "." <> fieldName f) (fieldType f)
        ]

    -- Emitted once per duplicated name, at its second occurrence.
    duplicates ∷ Text → [Text] → [Text]
    duplicates what names =
        [ "duplicate " <> what <> " name " <> n
        | (i, n) ← zip [0 ∷ Int ..] names
        , length (filter (≡ n) (take (i + 1) names)) ≡ 2 ]
