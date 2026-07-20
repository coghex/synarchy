{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving #-}
-- | Canonical typed-persistent-reference vocabulary (issue #764,
--   save-overhaul C3). Every durable cross-component reference — a
--   craft bill's station, a power node's host building, a Lua AI
--   claim's target — is described the same way: what KIND of entity it
--   names ('RefKind'), and whether the target is expected on the SAME
--   page as the referencing record or in some other, explicitly
--   declared scope ('RefScope'). "World.Save.Integrity" consumes this
--   vocabulary to build the shared save/load integrity graph.
--
--   This module is deliberately a LEAF: no dependency on
--   "World.Save.Snapshot" or any component module, so both directions
--   (component DTOs that WRAP a reference field, and the integrity
--   graph that VALIDATES one) can import it without a cycle.
--
--   === Why a thin wrapper, not a fatter wire record
--
--   Requirement 2 asks a persisted reference to "carry enough
--   information to determine" its expected kind, scope, and
--   optionality — not that every one of those facts must be new BYTES
--   on the wire. For a same-page reference (the only kind any field
--   uses today — 'CraftBillDTO''s station/claimant,
--   'PowerNodeDTO''s host building, see "World.Save.Component.Entities"),
--   the owning page is always the SAME page as the record carrying the
--   reference — a fact fixed by which component the field lives in,
--   not by a value that could vary per-entry. 'SamePageRef' therefore
--   carries that scope declaration at the TYPE level (a field typed
--   @SamePageRef BuildingId@ unambiguously means "same-page building
--   reference" to both the compiler and to "World.Save.Integrity"'s
--   field-level checks) while staying wire-IDENTICAL to the bare id it
--   wraps (a newtype over a single-constructor record has no
--   discriminant in cereal's generic encoding). Optionality is carried
--   by wrapping in 'Maybe' at the call site ('bilClaimant' is
--   @Maybe (SamePageRef UnitId)@), never folded into this type.
--
--   A reference explicitly permitted to cross pages would need the
--   target's OWNING page as real payload (there is nothing structural
--   to infer it from) — 'CrossPageRef' exists for that future case, but
--   no shipped field uses it yet; "World.Save.Integrity"'s test suite
--   exercises it directly against a synthetic edge so the "permitted
--   cross-page" acceptance path has real coverage without inventing a
--   fake gameplay field.
--
--   'SamePageRef' also derives 'Hashable' (round-3 review, issue #764):
--   a same-page reference can be the very thing a wire DTO's
--   'Data.HashMap.Strict.HashMap' is keyed BY, not only a field value
--   inside one — see "World.Save.Component.Entities"'s @PageSimDTO@,
--   whose @psSim@ map key is a unit-simulation state's OWNING unit,
--   itself a same-page cross-component reference exactly like a bill's
--   station.
module World.Save.Reference
    ( RefKind(..)
    , ContentKind(..)
    , RefScope(..)
    , refKindText
    , contentKindText
    , SamePageRef(..)
    , CrossPageRef(..)
    ) where

import UPrelude
import Data.Serialize (Serialize)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import World.Page.Types (WorldPageId)

-- | Every durable entity kind a persisted reference can name
--   (requirement 1). 'RefContent' further distinguishes WHICH gameplay
--   content catalogue a def-name reference resolves against — see
--   'ContentKind'.
data RefKind
    = RefPage
    | RefUnit
    | RefBuilding
    | RefItemInstance
    | RefBill
    | RefPowerNode
    | RefGroundItem
    | RefContent !ContentKind
    deriving (Show, Eq, Ord, Generic, Serialize)

-- | Gameplay content catalogues a stable, kind-qualified def-name
--   reference can resolve against (requirement 1's closing bullet,
--   requirement 5). Mirrors the content kinds
--   "Engine.Scripting.Lua.API.Save"'s existing missing-def-reference
--   ladder already validates per field (building/unit/item/recipe/
--   material/flora/construct-pack/location/infection) — this enum is
--   the shared vocabulary those checks and the new integrity graph
--   both speak, without requiring every one of those already-working
--   validators to be rewritten onto one generic traversal.
data ContentKind
    = ContentUnit
    | ContentItem
    | ContentBuilding
    | ContentMaterial
    | ContentRecipe
    | ContentFlora
    | ContentConstruct
    | ContentLocation
    | ContentInfection
    deriving (Show, Eq, Ord, Generic, Serialize)

-- | Whether a reference's target must live on the same page as the
--   record that carries it, or is a session-global identity (an
--   allocator shared across every page — item instances, building ids,
--   unit ids). No field is cross-page-permitted yet; the vocabulary
--   exists so "World.Save.Integrity" can express (and test) the
--   distinction requirement 4 draws between same-page, global, and
--   explicitly-permitted-cross-page.
data RefScope
    = ScopeGlobal
    | ScopeSamePage
    | ScopeCrossPage
    deriving (Show, Eq, Ord, Generic, Serialize)

-- | Render a 'RefKind' the same short, stable way on both sides of the
--   Haskell/Lua boundary — this text is exactly the @kind@ string a
--   Lua @references()@ hook uses (e.g. @"unit"@, @"building"@,
--   @"craft_bill"@, @"item_instance"@, @"ground_item"@; see
--   "Engine.Scripting.Lua.API.Save"'s Lua-reference validation), so a
--   diagnostic naming a Haskell-side kind and one naming a Lua-side
--   kind read identically.
refKindText ∷ RefKind → Text
refKindText k = case k of
    RefPage         → "page"
    RefUnit         → "unit"
    RefBuilding     → "building"
    RefItemInstance → "item_instance"
    RefBill         → "craft_bill"
    RefPowerNode    → "power_node"
    RefGroundItem   → "ground_item"
    RefContent ck   → "content_" <> contentKindText ck

contentKindText ∷ ContentKind → Text
contentKindText ck = case ck of
    ContentUnit       → "unit_def"
    ContentItem       → "item_def"
    ContentBuilding   → "building_def"
    ContentMaterial   → "material_def"
    ContentRecipe     → "recipe_def"
    ContentFlora      → "flora_def"
    ContentConstruct  → "construct_def"
    ContentLocation   → "location_def"
    ContentInfection  → "infection_def"

-- | A reference whose target is declared, at the type level, to live
--   on the SAME page as the record carrying it. Wire-identical to the
--   wrapped id (see the module haddock) — a newtype over a single-field
--   record has no cereal discriminant, so wrapping an existing field in
--   'SamePageRef' changes its Haskell TYPE (a genuine schema event,
--   handled with the frozen-DTO discipline's explicit migration seam —
--   see "World.Save.Component.Entities"'s craft-bill/power-node v1→v2
--   migration) without changing its encoded bytes.
newtype SamePageRef a = SamePageRef { unSamePageRef ∷ a }
    deriving stock (Show, Eq, Generic)
    deriving newtype (Serialize, Hashable)

-- | A reference explicitly permitted to cross pages: the target's own
--   owning page travels as real payload, since (unlike 'SamePageRef')
--   there is no structural fact to infer it from. Introduced for
--   vocabulary completeness (requirement 4) and exercised directly by
--   "World.Save.Integrity"'s test suite; no shipped field uses it yet.
data CrossPageRef a = CrossPageRef
    { cprPage  ∷ !WorldPageId
    , cprValue ∷ !a
    } deriving (Show, Eq, Generic, Serialize)
