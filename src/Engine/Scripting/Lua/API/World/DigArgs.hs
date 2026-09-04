{-# LANGUAGE Strict #-}
-- | The argument boundary of @world.digTile@ (#2338).
--
--   The verb used to narrow every numeric slot with a bare @round@ or
--   @realToFrac@ and enqueue 'World.Command.Types.WorldDigTile'
--   unconditionally. Nothing downstream raises on the result — GHC's
--   @floor@ answers 0 for a non-finite input — so a NaN amount, an
--   infinite one, a NaN skill or a skill of @1e9@ all reached the mine
--   state as ordinary numbers and did silent, DURABLE damage there: an
--   unfinishable designation, or a tile that never yields another
--   chunk, both of which round-trip through the save verbatim.
--
--   This module is the refusal. It reads one slot, reports exactly what
--   was wrong with it, and leaves the enqueue to the caller. The domains
--   themselves are NOT defined here — they live in "World.Command.Types"
--   next to the constructor whose fields they describe, so the world
--   thread's own defensive check tests the same predicates this boundary
--   does.
--
--   It is deliberately a SIBLING of
--   "Engine.Scripting.Lua.API.Units.MotionArgs" rather than a reuse of
--   it: the shape and both rules below are the ones #2290 established,
--   but the domains differ — an amount and a skill are not coordinates,
--   and a tile coordinate is an 'Int' rather than a narrowed 'Float'.
--
--   Three things about the reading are load-bearing:
--
--     * The reader classifies SHAPE only. Every domain question is
--       asked once, by the combinator for the slot that asks it, so no
--       predicate is applied twice and flipping any one of them changes
--       exactly one slot's behaviour.
--     * Finiteness is therefore tested AFTER narrowing to the 'Float'
--       the command carries, so a finite Lua @1e39@ that becomes
--       @Infinity@ in the field is caught. The 'Double' the reader
--       keeps beside it is what the diagnostic quotes, so an author
--       reads back the @1e39@ they wrote rather than an @Infinity@ they
--       never typed.
--     * Absence is read from 'Lua.ltype', not from 'Lua.tonumber'
--       returning 'Nothing'. @tonumber@ conflates \"the slot is empty\"
--       with \"the slot holds a table\", and for an OPTIONAL argument
--       those are different answers; it also coerces a numeric STRING,
--       which a slot documented as a number should not silently accept.
module Engine.Scripting.Lua.API.World.DigArgs
    ( DigArg(..)
    , DigTileArg(..)
    , readDigArg
    , readDigTileArg
    , requiredTileCoordinate
    , requiredPosition
    , requiredAmount
    , defaultingSkill
    , defaultingPerception
    ) where

import UPrelude
import qualified HsLua as Lua
import World.Command.Types
    ( digAmountInDomain, digPerceptionInDomain, digPositionInDomain
    , digSkillInDomain, digTileCoordinate )

-- | The SHAPE one 'Float'-carrying dig slot turned out to have.
data DigArg
    = DigAbsent
      -- ^ The slot is past the end of the call, or holds an explicit
      --   @nil@. Lua cannot tell a trailing @nil@ from a missing
      --   argument, so neither does this.
    | DigNotNumber !Text
      -- ^ The slot holds something that is not a Lua number; the 'Text'
      --   names the type it does hold, for the diagnostic. A numeric
      --   STRING lands here too, which is the point.
    | DigNumber !Float !Double
      -- ^ A Lua number, narrowed to the 'Float' the command carries and
      --   paired with the value as Lua had it. Whether it is USABLE is
      --   the slot's own domain question, never this constructor's: a
      --   NaN, an infinity, a negative amount and a skill of @1e9@ all
      --   arrive here alike.
    deriving (Show, Eq)

-- | The shape one TILE-coordinate slot turned out to have. Separate
--   from 'DigArg' because @gx@\/@gy@ are 'Int' fields: they are never
--   narrowed to a 'Float', and the bound they must respect is 'Int''s
--   own rather than finiteness.
data DigTileArg
    = DigTileAbsent
    | DigTileNotNumber !Text
    | DigTileNumber !Double
    deriving (Show, Eq)

-- | Read one 'Float'-carrying slot into a 'DigArg'.
readDigArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception DigArg
readDigArg ix = do
    ty ← Lua.ltype ix
    case ty of
        Lua.TypeNone   → pure DigAbsent
        Lua.TypeNil    → pure DigAbsent
        Lua.TypeNumber → do
            mN ← Lua.tonumber ix
            pure $ case mN of
                Just (Lua.Number d) → DigNumber (realToFrac d) d
                -- Unreachable in practice: the slot IS a number. Read
                -- as a malformed number rather than as an absent one,
                -- so a future HsLua change cannot silently turn a
                -- required argument into a defaulted one. NaN is in no
                -- slot's domain, so every combinator refuses it.
                Nothing → DigNumber (0 / 0) (0 / 0)
        _ → pure (DigNotNumber (luaTypeName ty))

-- | Read one tile-coordinate slot into a 'DigTileArg'.
readDigTileArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception DigTileArg
readDigTileArg ix = do
    ty ← Lua.ltype ix
    case ty of
        Lua.TypeNone   → pure DigTileAbsent
        Lua.TypeNil    → pure DigTileAbsent
        Lua.TypeNumber → do
            mN ← Lua.tonumber ix
            pure $ case mN of
                Just (Lua.Number d) → DigTileNumber d
                Nothing             → DigTileNumber (0 / 0)
        _ → pure (DigTileNotNumber (luaTypeName ty))

-- | A REQUIRED tile-coordinate slot: the 'Int' tile it names, or the
--   warning that refuses the call. @verb@ and @argName@ are what let a
--   caller find the offending call site from the log alone.
--
--   'digTileCoordinate' is the whole decision AND the conversion, so
--   there is exactly one place a coordinate can be judged. The
--   finiteness test below is only to word the diagnostic — a NaN and an
--   absurd-but-finite @1e30@ are different caller mistakes and read
--   better said apart — and never decides admission.
requiredTileCoordinate ∷ Text → Text → DigTileArg → Either Text Int
requiredTileCoordinate verb argName arg = case arg of
    DigTileNumber d → case digTileCoordinate d of
        Just i  → Right i
        Nothing
            | isNaN d ∨ isInfinite d →
                Left (message verb argName
                          ("must be a finite number, got " <> tshow d))
            | otherwise →
                Left (message verb argName
                          ("must name a tile, got " <> tshow d
                             <> " which does not round into the "
                             <> "tile-coordinate range"))
    DigTileAbsent      →
        Left (message verb argName "is required, but was omitted or nil")
    DigTileNotNumber t →
        Left (message verb argName ("must be a number, got a " <> t))

-- | A REQUIRED tile-space position slot ('digPositionInDomain').
requiredPosition ∷ Text → Text → DigArg → Either Text Float
requiredPosition verb argName =
    checkDigArg verb argName digPositionInDomain "must be a finite number"

-- | A REQUIRED amount slot ('digAmountInDomain').
requiredAmount ∷ Text → Text → DigArg → Either Text Float
requiredAmount verb argName =
    checkDigArg verb argName digAmountInDomain
        "must be a finite, non-negative number"

-- | An OPTIONAL skill slot ('digSkillInDomain'): an omitted or
--   explicitly-@nil@ one keeps @def@ untouched, and anything actually
--   supplied is judged.
--
--   The split matters: @tonumber@ alone would map a table, a string and
--   an omission onto one 'Nothing', so a typo'd seventh argument would
--   quietly inherit the default instead of being refused.
defaultingSkill ∷ Text → Text → Float → DigArg → Either Text Float
defaultingSkill _    _       def DigAbsent = Right def
defaultingSkill verb argName _   arg       =
    checkDigArg verb argName digSkillInDomain
        "must be a finite number within the 0-100 skill scale" arg

-- | An OPTIONAL perception slot ('digPerceptionInDomain'), defaulting
--   exactly as 'defaultingSkill' does.
defaultingPerception ∷ Text → Text → Float → DigArg → Either Text Float
defaultingPerception _    _       def DigAbsent = Right def
defaultingPerception verb argName _   arg       =
    checkDigArg verb argName digPerceptionInDomain
        "must be a finite number" arg

-- | One slot against one domain: the 'Float' the command will carry, or
--   the warning that refuses the call. @expected@ states the domain in
--   the caller's own words; the value quoted back is the 'Double' Lua
--   held, not the narrowing of it.
checkDigArg ∷ Text → Text → (Float → Bool) → Text → DigArg
            → Either Text Float
checkDigArg verb argName inDomain expected arg = case arg of
    DigNumber f d
        | inDomain f → Right f
        | otherwise  →
            Left (message verb argName (expected <> ", got " <> tshow d))
    DigAbsent      →
        Left (message verb argName "is required, but was omitted or nil")
    DigNotNumber t →
        Left (message verb argName ("must be a number, got a " <> t))

-- | One warning sentence, shaped exactly like the motion verbs'
--   refusals (#2290) so every argument refusal in the engine reads the
--   same and a log filter written for one finds all of them.
message ∷ Text → Text → Text → Text
message verb argName why =
    verb <> ": argument '" <> argName <> "' " <> why <> " — call refused"

-- | The Lua type name to quote back at a caller who supplied the wrong
--   kind of thing. 'Lua.TypeNone' and 'Lua.TypeNil' never reach here —
--   both readers resolve them to their absent constructor first.
luaTypeName ∷ Lua.Type → Text
luaTypeName ty = case ty of
    Lua.TypeNone          → "nothing"
    Lua.TypeNil           → "nil"
    Lua.TypeBoolean       → "boolean"
    Lua.TypeLightUserdata → "userdata"
    Lua.TypeNumber        → "number"
    Lua.TypeString        → "string"
    Lua.TypeTable         → "table"
    Lua.TypeFunction      → "function"
    Lua.TypeUserdata      → "userdata"
    Lua.TypeThread        → "thread"
