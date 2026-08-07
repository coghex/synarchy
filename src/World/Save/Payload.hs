{-# LANGUAGE Strict #-}
-- | The two payloads the save command carries across the Haskell/Lua
--   persistence boundary (issue #1103): one Lua-owned component's spec
--   ('LuaComponentSpec') and one reference edge a Lua component's
--   @references()@ hook reported ('LuaRefEdge'). Both are records with
--   named fields rather than the bare tuples they used to be — the
--   reference edge in particular carries three adjacent 'Text' fields
--   (@component@, @kind@, @path@) that a positional assembly could
--   permute without the compiler noticing, and that assembly happens in
--   "Engine.Scripting.Lua.API.Save.Bridge" from Lua fields the writer
--   already named. Build and read both with LABELLED fields; positional
--   construction would put the hazard straight back.
--
--   This module is deliberately a LEAF — like "World.Save.Reference",
--   it depends on nothing local — because the same two records are
--   named by otherwise-unrelated layers: the HsLua stack readers that
--   build them ("Engine.Scripting.Lua.API.Save.Bridge"), the
--   world-thread command that transports them ("World.Command.Types"),
--   and the encode/validate consumers ("World.Save.Envelope",
--   "World.Save.Integrity"). A leaf home is what lets
--   "World.Command.Types" name the canonical types instead of
--   re-spelling their shapes inline, without gaining a dependency on
--   the Save/Envelope module graph.
module World.Save.Payload
    ( LuaComponentSpec(..)
    , LuaRefEdge(..)
    ) where

import UPrelude
import qualified Data.ByteString as BS

-- | One Lua-owned component: its bare (unprefixed) registry id, schema
--   version, the writer's own required/optional declaration, and its
--   already-canonically-encoded payload bytes (see
--   @scripts/lib/data_codec.lua@) — the same four facts
--   "World.Save.Envelope.Codec"'s @ComponentSpec@ carries for the
--   Haskell set, minus the id-namespacing "World.Save.Envelope" owns
--   (that module prefixes 'lcsId' into the reserved @lua.@ namespace —
--   issue #761). @ComponentSpec@ itself is deliberately still a bare
--   tuple: its four component types are mutually distinct, so unlike
--   'LuaRefEdge' it has no permutation that type-checks.
--
--   Re-exported by "World.Save.Envelope", which is where the encode and
--   decode sides of the envelope both name it.
data LuaComponentSpec = LuaComponentSpec
    { lcsId       ∷ !Text
    , lcsVersion  ∷ !Word32
    , lcsRequired ∷ !Bool
    , lcsPayload  ∷ !BS.ByteString
    } deriving (Show, Eq)

-- | One reference a Lua save component's @references()@ hook reported —
--   the raw @{kind=.., id=..}@ shape, plus which Lua component it came
--   from (for diagnostic attribution), the OWNING unit id when the hook
--   supplied one (@lreOwner@ — every @unitAiReferences@ entry is emitted
--   from inside a per-unit loop, so it always has one;
--   @buildingSpawnReferences@ entries never need one, since its
--   "unit"/"building" kinds resolve session-wide regardless), and
--   (issue #764) the actual field path this edge came
--   from (@lrePath@ — e.g. @"unit[7].attackTargetUid"@,
--   @"building[12].lastUid"@), in the SAME dotted-path style
--   "World.Save.Integrity"'s @refEdgeError@ already uses for
--   Haskell-side findings — see
--   @unit_ai_save_refs.lua@'s @unitAiReferences@ and
--   @building_spawn.lua@'s @buildingSpawnReferences@, which build it.
--   @lrePage@ (#915) is the edge's OWN declared world page, for the one
--   reference kind whose id means nothing without it
--   (@location_instance@ — a per-page allocator whose durable identity
--   IS @(page, id)@, see #911). Unlike @craft_bill@/@ground_item@,
--   which borrow the owning unit's page, a location memory must name
--   its page itself: the page is part of what the unit remembers, not
--   an incidental fact about where the unit currently stands.
--
--   Re-exported by "World.Save.Integrity", the module whose
--   @luaReferenceErrors@ consumes it. It is the SAME record all the way
--   from the HsLua stack reader that decodes it, so nothing on the path
--   re-assembles it positionally.
data LuaRefEdge = LuaRefEdge
    { lreComponent ∷ !Text
    , lreKind      ∷ !Text
    , lreId        ∷ !Int
    , lreOwner     ∷ !(Maybe Int)
    , lrePath      ∷ !Text
    , lrePage      ∷ !(Maybe Text)
    } deriving (Show, Eq)
