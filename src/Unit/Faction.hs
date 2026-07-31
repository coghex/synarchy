{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The typed faction model (#912): the single definition of who a unit
--   belongs to and how two factions regard each other.
--
--   Before this module one 'Text' field and @==@ answered five different
--   questions across four call sites. They agreed only because there were
--   effectively two factions in play. Two of those questions are
--   PROPERTIES of a single faction and three are RELATIONS between two,
--   and the whole point of this module is that they stay distinct:
--
--   * 'isPlayerOwned' — "is this the player's own unit?" Governs location
--     discovery eligibility ("Location.Discovery"). ONLY 'FactionPlayer'.
--   * 'isPlayerCommandable' — "can this unit take orders?" Governs the
--     right-click command menu. 'FactionPlayer' and 'FactionDebug'.
--   * 'hasUnrestrictedCombat' — "does this faction ignore friendly fire?"
--     ONLY 'FactionDebug', which exists so the debug overlay can stage
--     acolyte-vs-acolyte fights.
--   * 'factionRelation' — ally / neutral / hostile, the one answer to how
--     two factions regard each other.
--
--   __The trap this module exists to prevent:__ ownership must not
--   collapse into alliance. 'FactionDebug' is allied with
--   'FactionPlayer' (a debug medic patches up player units) but is NOT
--   player-owned — a debug unit walking through a ruin must never
--   trigger its discovery. Those are different questions; asking the
--   wrong one is a silent, player-visible behavior change.
--
--   __Wire format:__ a faction is NOT serialized as an enum. Saves carry
--   the lowercase 'factionTag' text and parse back through
--   'factionFromTag' at the load boundary, so this type can grow
--   constructors in any order without the append-only constraint the
--   positional @Generic Serialize@ policy imposes (see
--   "World.Save.Types"). There is no migration and no
--   @currentSaveVersion@ interaction.
--
--   __Wildlife is deliberately one faction.__ Every animal is mutually
--   allied. That is an interim, not the end state: predation (a hungry
--   bear hunting a squirrel) needs predator and prey NOT to be allies,
--   and that decision belongs with the aggression work, not here.
module Unit.Faction
    ( Faction(..)
    , FactionRelation(..)
    , allFactions
      -- * Wire tags
    , factionTag
    , parseFaction
    , factionFromTag
    , fallbackFaction
    , defaultSpawnFaction
      -- * Properties of one faction
    , isPlayerOwned
    , isPlayerCommandable
    , hasUnrestrictedCombat
      -- * Relations between two factions
    , factionRelation
    , relationTag
    , areAllies
    , canAttack
    ) where

import UPrelude
import GHC.Generics (Generic)

-- | Every faction a shipped unit can belong to.
--
--   Constructor ORDER is not load-bearing: this type is never serialized
--   positionally (see the module header) — saves carry 'factionTag'.
data Faction
    = FactionPlayer
      -- ^ The player's own colonists. The only player-OWNED faction.
    | FactionWildlife
      -- ^ Animals. One mutually-allied faction by design (see header).
      --   Also the documented default for a 'unit.spawn' with no tag.
    | FactionHostile
      -- ^ Ruin occupants and other units placed to be fought.
      --   @scripts/locations.lua@ spawns location contents with this tag
      --   unless the content entry names another.
    | FactionNeutral
      -- ^ Fights nobody and is fought by nobody. Also the documented
      --   fallback for an unrecognized tag ('fallbackFaction') — the
      --   inert choice, so a corrupt or hand-edited tag can never make a
      --   unit newly hostile.
    | FactionDebug
      -- ^ Debug-overlay spawns. Player-commandable and allied with the
      --   player, but NOT player-owned, and the only faction with
      --   'hasUnrestrictedCombat' so staged test fights are possible.
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | How two factions regard each other. Answers only the STATIC question
--   ("how does A regard B?"); whether a unit acts on it right now — FOV,
--   distance, threat scoring, pursuit — is the aggression system's job.
data FactionRelation
    = RelAlly
      -- ^ Medics treat them; they rally into a swarm together.
    | RelNeutral
      -- ^ Neither side has business with the other.
    | RelHostile
      -- ^ Attacking is permitted and threat detection should fire.
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

allFactions ∷ [Faction]
allFactions = [minBound .. maxBound]

-- | The canonical lowercase wire tag. This is what a save carries, what
--   @unit.getFaction@ returns to Lua, and what @unit.spawn@ accepts.
factionTag ∷ Faction → Text
factionTag f = case f of
    FactionPlayer   → "player"
    FactionWildlife → "wildlife"
    FactionHostile  → "hostile"
    FactionNeutral  → "neutral"
    FactionDebug    → "debug"

-- | Strict parse: 'Nothing' for a tag outside the known vocabulary. Use
--   this when the caller needs to KNOW the tag was unrecognized (to warn
--   about it); use 'factionFromTag' when it just needs a faction.
parseFaction ∷ Text → Maybe Faction
parseFaction t = case t of
    "player"   → Just FactionPlayer
    "wildlife" → Just FactionWildlife
    "hostile"  → Just FactionHostile
    "neutral"  → Just FactionNeutral
    "debug"    → Just FactionDebug
    _          → Nothing

-- | What an unrecognized tag resolves to. 'FactionNeutral' is chosen
--   because it is inert in every direction: a unit that lands here fights
--   nobody, is fought by nobody, owns nothing, and commands nothing. A
--   bad tag degrades a unit rather than failing a load.
fallbackFaction ∷ Faction
fallbackFaction = FactionNeutral

-- | Total parse. An unrecognized tag becomes 'fallbackFaction'. Callers
--   that want to warn about the unrecognized tag should go through
--   'parseFaction' so they can see it.
factionFromTag ∷ Text → Faction
factionFromTag = fromMaybe fallbackFaction . parseFaction

-- | What @unit.spawn(def, x, y)@ assigns when no faction tag is given.
--   Deliberate and long-standing: world-gen animal spawns are the
--   overwhelmingly common tag-less caller, and every unit source that
--   means something else (portal spawns → 'FactionPlayer', debug overlay
--   → 'FactionDebug', location contents → 'FactionHostile') passes its
--   tag explicitly.
defaultSpawnFaction ∷ Faction
defaultSpawnFaction = FactionWildlife

-- | Is a unit of this faction one of the PLAYER'S OWN?
--
--   Deliberately narrower than "friendly to the player": 'FactionDebug'
--   is an ally but answers 'False' here, which is what keeps a debug unit
--   from discovering a location by walking through it.
isPlayerOwned ∷ Faction → Bool
isPlayerOwned f = f ≡ FactionPlayer

-- | Can a unit of this faction receive player orders (move, attack)?
isPlayerCommandable ∷ Faction → Bool
isPlayerCommandable f = f ≡ FactionPlayer ∨ f ≡ FactionDebug

-- | Does this faction ignore friendly-fire restrictions entirely?
--
--   The debug overlay's whole purpose is staging fights between units
--   that would normally refuse to attack each other, so the property
--   applies when a debug unit is EITHER the attacker or the target (see
--   'canAttack').
hasUnrestrictedCombat ∷ Faction → Bool
hasUnrestrictedCombat f = f ≡ FactionDebug

-- | How @a@ regards @b@. Total (every pair has an answer) and symmetric
--   (@factionRelation a b ≡ factionRelation b a@) — there is no
--   documented asymmetric case today, and a future one must say so here.
--
--   The table:
--
--   * a faction is always allied with itself (this is what preserves the
--     same-tag medic behavior and wildlife's internal alliance);
--   * 'FactionPlayer' and 'FactionDebug' are allied — the declared form
--     of the pairing that used to be hardcoded inside medic logic;
--   * any distinct pair involving 'FactionNeutral' is neutral;
--   * every other distinct pair is hostile.
factionRelation ∷ Faction → Faction → FactionRelation
factionRelation a b
    | a ≡ b                        = RelAlly
    | isPlayerSide a ∧ isPlayerSide b = RelAlly
    | a ≡ FactionNeutral ∨ b ≡ FactionNeutral = RelNeutral
    | otherwise                    = RelHostile
  where
    isPlayerSide f = f ≡ FactionPlayer ∨ f ≡ FactionDebug

-- | Lowercase wire tag for a relation, as Lua sees it.
relationTag ∷ FactionRelation → Text
relationTag r = case r of
    RelAlly    → "ally"
    RelNeutral → "neutral"
    RelHostile → "hostile"

-- | Would a unit of faction @a@ fight on the same side as one of @b@?
--   Medic triage and swarm rallying both ask exactly this.
areAllies ∷ Faction → Faction → Bool
areAllies a b = factionRelation a b ≡ RelAlly

-- | May a unit of faction @attacker@ be ordered to attack one of
--   @target@?
--
--   Hostility is the normal permission, but 'hasUnrestrictedCombat' on
--   EITHER participant overrides it — that is what preserves every
--   direction the debug overlay allows today (player→debug, debug→player,
--   debug→debug) without a special case inside the context menu.
canAttack ∷ Faction → Faction → Bool
canAttack attacker target =
       factionRelation attacker target ≡ RelHostile
    ∨ hasUnrestrictedCombat attacker
    ∨ hasUnrestrictedCombat target
