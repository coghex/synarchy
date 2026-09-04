{-# LANGUAGE Strict #-}
-- | The argument boundary of the three unit MOTION verbs — @unit.setPos@,
--   @unit.moveTo@ and @unit.setMoveSpeed@ (#2290).
--
--   Every one of them used to convert its numeric slots with a bare
--   @realToFrac@ and substitute a zero for a slot that was missing or
--   not a number, then enqueue unconditionally. Nothing downstream
--   rejected the result, so a NaN, an infinity, a dropped argument or a
--   negative speed all reached the simulation as ordinary values and
--   did silent, durable damage there rather than failing at the call.
--
--   This module is the refusal: it reads one slot, reports exactly what
--   was wrong with it, and leaves the enqueue to the caller. The domain
--   itself is NOT defined here — 'motionCoordinateInDomain' and
--   'motionSpeedInDomain' live next to the command constructors, so the
--   unit thread's own defensive checks test the same predicate this
--   boundary does.
--
--   Two things about the reading are load-bearing:
--
--     * The finiteness test runs AFTER narrowing to the 'Float' the
--       command carries. A Lua number is a 'Double', so a perfectly
--       ordinary @1e300@ is finite in Lua and 'Infinity' in the field
--       it is about to be written to; checking before narrowing would
--       let exactly that through.
--     * Absence is read from 'Lua.ltype', not from 'Lua.tonumber'
--       returning 'Nothing'. @tonumber@ conflates \"the slot is empty\"
--       with \"the slot holds a table\", and the two are not the same
--       answer for an OPTIONAL argument: omission keeps a default,
--       while a supplied value that is not a number is a caller bug.
--       It also coerces a numeric STRING, which an argument slot
--       documented as a number should not silently accept.
module Engine.Scripting.Lua.API.Units.MotionArgs
    ( MotionArg(..)
    , readMotionArg
    , requiredCoordinate
    , requiredSpeed
    , defaultingSpeed
    ) where

import UPrelude
import qualified HsLua as Lua
import Unit.Command.Types (motionCoordinateInDomain, motionSpeedInDomain)

-- | What one numeric motion slot turned out to hold, already narrowed
--   to the 'Float' the command would carry.
data MotionArg
    = MotionAbsent
      -- ^ The slot is past the end of the call, or holds an explicit
      --   @nil@. Lua cannot tell a trailing @nil@ from a missing
      --   argument, so neither does this — both mean \"the caller did
      --   not supply one\".
    | MotionNotNumber !Text
      -- ^ The slot holds something that is not a Lua number; the 'Text'
      --   names the type it does hold, for the diagnostic.
    | MotionNotFinite !Double
      -- ^ A Lua number that is NaN or infinite once narrowed to
      --   'Float'. The 'Double' is the value as Lua had it, so an
      --   overflowing @1e300@ is reported as what the author wrote
      --   rather than as a bare @Infinity@ they never typed.
    | MotionFinite !Float
      -- ^ A number in 'motionCoordinateInDomain'. Whether it is in the
      --   narrower speed domain is 'requiredSpeed'\'s question.
    deriving (Show, Eq)

-- | Read one numeric slot into a 'MotionArg'.
readMotionArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception MotionArg
readMotionArg ix = do
    ty ← Lua.ltype ix
    case ty of
        Lua.TypeNone   → pure MotionAbsent
        Lua.TypeNil    → pure MotionAbsent
        Lua.TypeNumber → do
            mN ← Lua.tonumber ix
            pure $ case mN of
                Just (Lua.Number d) →
                    let f = realToFrac d ∷ Float
                    in if motionCoordinateInDomain f
                         then MotionFinite f
                         else MotionNotFinite d
                -- Unreachable in practice: the slot IS a number. Read
                -- as a malformed number rather than as an absent one,
                -- so a future HsLua change cannot silently turn a
                -- required argument into a defaulted one.
                Nothing → MotionNotFinite (0 / 0)
        _ → pure (MotionNotNumber (luaTypeName ty))

-- | A REQUIRED coordinate slot: the finite 'Float' it names, or the
--   warning that refuses the call. @verb@ and @argName@ are what let a
--   caller find the offending call site from the log alone.
requiredCoordinate ∷ Text → Text → MotionArg → Either Text Float
requiredCoordinate verb argName arg = case arg of
    MotionFinite f → Right f
    _              → Left (refusal verb argName arg)

-- | A REQUIRED speed slot: as 'requiredCoordinate', plus the
--   non-negativity 'motionSpeedInDomain' adds.
requiredSpeed ∷ Text → Text → MotionArg → Either Text Float
requiredSpeed verb argName arg = do
    f ← requiredCoordinate verb argName arg
    if motionSpeedInDomain f
      then Right f
      else Left (message verb argName
                    ("must not be negative, got " <> tshow f))

-- | An OPTIONAL speed slot: an omitted or explicitly-@nil@ one keeps
--   @def@ untouched, and anything actually supplied must satisfy
--   'requiredSpeed'.
--
--   The split matters: @tonumber@ alone would map a table, a string and
--   an omission onto one 'Nothing', so a typo'd fourth argument would
--   quietly inherit the default instead of being refused.
defaultingSpeed ∷ Text → Text → Float → MotionArg → Either Text Float
defaultingSpeed _    _       def MotionAbsent = Right def
defaultingSpeed verb argName _   arg          = requiredSpeed verb argName arg

-- | The warning an unusable slot earns, by the shape it was unusable in.
refusal ∷ Text → Text → MotionArg → Text
refusal verb argName arg = message verb argName $ case arg of
    MotionAbsent      → "is required, but was omitted or nil"
    MotionNotNumber t → "must be a number, got a " <> t
    MotionNotFinite d → "must be a finite number, got " <> tshow d
    -- Not a refusal shape; kept total rather than partial.
    MotionFinite f    → "is " <> tshow f

-- | One warning sentence, shaped like @unit.moveTo@'s existing
--   hazard-token refusal so every motion-verb refusal reads the same
--   and a log filter written for one finds all of them.
message ∷ Text → Text → Text → Text
message verb argName why =
    verb <> ": argument '" <> argName <> "' " <> why <> " — call refused"

-- | The Lua type name to quote back at a caller who supplied the wrong
--   kind of thing. 'Lua.TypeNone' and 'Lua.TypeNil' never reach here —
--   'readMotionArg' resolves both to 'MotionAbsent' first.
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
