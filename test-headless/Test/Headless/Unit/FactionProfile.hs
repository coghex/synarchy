{-# LANGUAGE Strict #-}
-- | The "Unit faction profile policy" gate (#2500, FTS-1 of #2496): the
--   pure controller/tag/capability profile model in
--   "Unit.Faction.Profile" — its tag validation, its observer-relative
--   properties, the five-tier directed relation precedence, the
--   cause-scoped live overlay, the reverse-hostility predicate, and the
--   exact legacy compatibility matrices.
--
--   Pure fixtures only, no engine and no save path. The scalar model's
--   own gates are "Test.Headless.Unit.Faction" and
--   "Test.Headless.Lua.Faction"; both stay green and untouched by this
--   slice, and later slices port them.
--
--   The base relation table below is TEST-LOCAL on purpose. FTS-2 moves
--   it into validated YAML; until then the only thing that knows the
--   shipped D-28 compatibility relations is this file.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit faction profile policy"'@.
module Test.Headless.Unit.FactionProfile (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Unit.Faction
    ( Faction(..), FactionRelation(..), allFactions, canAttack
    , factionRelation, hasUnrestrictedCombat, isPlayerCommandable
    , isPlayerOwned )
import Unit.Faction.Profile

-- * Fixtures

-- | The one local controller the engine provides until multiplayer
--   exists. Every observer-relative assertion below names it explicitly.
localPlayer ∷ ControllerId
localPlayer = humanController "local"

otherPlayer ∷ ControllerId
otherPlayer = humanController "player_2"

-- | Same NAME as 'localPlayer', different kind: the two must never be
--   the same authority.
aiNamedLocal ∷ ControllerId
aiNamedLocal = aiController "local"

aiTeam ∷ ControllerId
aiTeam = aiController "raiders"

-- | Fails loudly rather than silently degrading, so a fixture typo can
--   never make one of these specs vacuously pass with fewer tags than it
--   reads as having.
tag ∷ Text → FactionTag
tag t = fromMaybe (error ("invalid fixture tag: " ⧺ show t)) (mkFactionTag t)

tagNomad, tagRedTribe, tagPlayerTeam ∷ FactionTag
tagNomad      = tag "nomad"
tagRedTribe   = tag "red_tribe"
tagPlayerTeam = tag "player_team"

tagTeamA, tagTeamB, tagFightA, tagFightB ∷ FactionTag
tagTeamA  = tag "team_a"
tagTeamB  = tag "team_b"
tagFightA = tag "fight_team_A"
tagFightB = tag "fight_team_B"

-- | The D-28 compatibility base table: symmetric hostility for exactly
--   four pairs and nothing else. @nomad@\/@wildlife@ and
--   @nomad@\/@legacy_hostile@ stay undeclared and therefore neutral.
legacyBase ∷ FactionPolicy
legacyBase = mkFactionPolicy
    [ SymmetricBase tagAcolyte       tagNomad    RelHostile
    , SymmetricBase tagAcolyte       tagWildlife RelHostile
    , SymmetricBase tagLegacyHostile tagAcolyte  RelHostile
    , SymmetricBase tagLegacyHostile tagWildlife RelHostile ]

causeOrder, causeReport, causeDiplomacy ∷ RelationCauseId
causeOrder     = relationCauseId "attack_order_7"
causeReport    = relationCauseId "incident_report_3"
causeDiplomacy = relationCauseId "diplomacy_ceasefire"

-- | @legacyProfile@ for each of the five scalar values, in a fixed order
--   so the matrix specs below can index rows and columns by it.
legacyOrder ∷ [Faction]
legacyOrder = [ FactionPlayer, FactionWildlife, FactionHostile
              , FactionNeutral, FactionDebug ]

profileOf ∷ Faction → FactionProfile
profileOf = legacyProfile localPlayer

-- | The expected 5×5 relation matrix, written out rather than derived,
--   so a precedence regression cannot move the expectation with the
--   implementation. Rows are the subject, columns the target, in
--   'legacyOrder'.
expectedLegacyRelations ∷ [[FactionRelation]]
expectedLegacyRelations =
    --            player      wildlife    hostile     neutral     debug
    [ {- player   -} [ RelAlly,    RelHostile, RelHostile, RelNeutral, RelNeutral ]
    , {- wildlife -} [ RelHostile, RelAlly,    RelHostile, RelNeutral, RelNeutral ]
    , {- hostile  -} [ RelHostile, RelHostile, RelAlly,    RelNeutral, RelNeutral ]
    , {- neutral  -} [ RelNeutral, RelNeutral, RelNeutral, RelNeutral, RelNeutral ]
    , {- debug    -} [ RelNeutral, RelNeutral, RelNeutral, RelNeutral, RelNeutral ]
    ]

-- | The eight ordered pairs whose relation differs from today's
--   'factionRelation', per the review correction on #2500 and D-27.
expectedLegacyDifferences ∷ [(Faction, Faction)]
expectedLegacyDifferences =
    [ (FactionPlayer,   FactionDebug)
    , (FactionWildlife, FactionDebug)
    , (FactionHostile,  FactionDebug)
    , (FactionNeutral,  FactionNeutral)
    , (FactionDebug,    FactionPlayer)
    , (FactionDebug,    FactionWildlife)
    , (FactionDebug,    FactionHostile)
    , (FactionDebug,    FactionDebug)
    ]

-- | Every ordered pair of the five legacy values.
legacyPairs ∷ [(Faction, Faction)]
legacyPairs = [ (a, b) | a ← legacyOrder, b ← legacyOrder ]

relate ∷ FactionPolicy → FactionProfile → FactionProfile → FactionRelation
relate = relationFromTo

spec ∷ Spec
spec = describe "Unit faction profile policy" $ do
    tagValidationSpec
    controllerSpec
    propertySpec
    precedenceSpec
    liveOverlaySpec
    baseTableSpec
    reverseHostilitySpec
    emptyProfileSpec
    desiredExperienceSpec
    legacyMatrixSpec

-- * Tag validation

tagValidationSpec ∷ Spec
tagValidationSpec = describe "tag validation" $ do
    it "rejects the empty string" $
        mkFactionTag "" `shouldBe` Nothing

    it "rejects every flavour of embedded whitespace" $ do
        mkFactionTag "red tribe"  `shouldBe` Nothing
        mkFactionTag " acolyte"   `shouldBe` Nothing
        mkFactionTag "acolyte "   `shouldBe` Nothing
        mkFactionTag "red\ttribe" `shouldBe` Nothing
        mkFactionTag "red\ntribe" `shouldBe` Nothing
        mkFactionTag " "          `shouldBe` Nothing

    it "accepts an unknown but syntactically valid tag" $ do
        -- The whole point of D-12: runtime systems mint identifiers this
        -- module has never heard of, and they are legitimate tags.
        (factionTagText ⊚ mkFactionTag "minted_team_0f3a")
            `shouldBe` Just "minted_team_0f3a"
        (factionTagText ⊚ mkFactionTag "🜂") `shouldBe` Just "🜂"

    it "round-trips a valid tag through its text" $
        factionTagText tagRedTribe `shouldBe` "red_tribe"

    it "drops malformed tags when building a profile, keeping valid ones" $ do
        let p = profileFromTagText Nothing ["acolyte", "", "red tribe", "nomad"] []
        Set.toList (profileTags p) `shouldBe` [tagAcolyte, tagNomad]

    it "cannot create alliance or hostility out of malformed data" $ do
        -- Both profiles' only "shared" text is the malformed one. If a
        -- malformed tag were admitted, these would be allies.
        let a = profileFromTagText Nothing ["bad tag"] []
            b = profileFromTagText Nothing ["bad tag"] []
        profileTags a `shouldBe` Set.empty
        relate legacyBase a b `shouldBe` RelNeutral

    it "an all-malformed profile degrades to the inert profile" $
        profileFromTagText Nothing ["", " ", "a b"] [] `shouldBe` emptyProfile

-- * Controller identity

controllerSpec ∷ Spec
controllerSpec = describe "controller identity" $ do
    it "distinguishes human from AI authorities sharing one name" $ do
        aiNamedLocal `shouldNotBe` localPlayer
        controllerKind localPlayer   `shouldBe` ControllerHuman
        controllerKind aiNamedLocal  `shouldBe` ControllerAI
        controllerName localPlayer   `shouldBe` "local"
        controllerName aiNamedLocal  `shouldBe` "local"

    it "treats equal controllers as one authority" $
        humanController "local" `shouldBe` localPlayer

    it "allies two profiles carrying the same controller" $ do
        let a = mkProfile (Just localPlayer) [tagAcolyte] []
            b = mkProfile (Just localPlayer) [tagNomad] []
        relate legacyBase a b `shouldBe` RelAlly
        relate legacyBase b a `shouldBe` RelAlly

    it "allies two profiles carrying the same AI controller" $ do
        let a = mkProfile (Just aiTeam) [tagAcolyte] []
            b = mkProfile (Just aiTeam) [tagNomad] []
        relate legacyBase a b `shouldBe` RelAlly

    it "does not ally a human and an AI controller of the same name" $ do
        let a = mkProfile (Just localPlayer)  [tagAcolyte] []
            b = mkProfile (Just aiNamedLocal) [tagNomad] []
        relate legacyBase a b `shouldBe` RelHostile

    it "never creates hostility from controller inequality alone" $ do
        -- Two different players, no tag relation at all.
        let a = mkProfile (Just localPlayer) [tag "alpha"] []
            b = mkProfile (Just otherPlayer) [tag "beta"] []
        relate legacyBase a b `shouldBe` RelNeutral
        relate legacyBase b a `shouldBe` RelNeutral

    it "never treats two absent controllers as a shared controller" $ do
        -- Both lack a controller and share no tag: a fabricated `none`
        -- authority would make these allies (D-5).
        let a = mkProfile Nothing [tag "alpha"] []
            b = mkProfile Nothing [tag "beta"] []
        profileController a `shouldBe` Nothing
        relate legacyBase a b `shouldBe` RelNeutral

    it "does not ally a controlled profile with an uncontrolled one" $ do
        let a = mkProfile (Just localPlayer) [tag "alpha"] []
            b = mkProfile Nothing            [tag "beta"] []
        relate legacyBase a b `shouldBe` RelNeutral
        relate legacyBase b a `shouldBe` RelNeutral

-- * Observer-relative properties

propertySpec ∷ Spec
propertySpec = describe "properties of one profile" $ do
    it "is player-owned only for the observing controller" $ do
        let p = mkProfile (Just localPlayer) [tagAcolyte] []
        isProfilePlayerOwned localPlayer p `shouldBe` True
        isProfilePlayerOwned otherPlayer p `shouldBe` False
        isProfilePlayerOwned aiNamedLocal p `shouldBe` False

    it "is owned by nobody when it has no controller" $ do
        let p = mkProfile Nothing [tagWildlife] []
        isProfilePlayerOwned localPlayer p `shouldBe` False
        isProfilePlayerOwned otherPlayer p `shouldBe` False

    it "commands an owned profile without any capability" $ do
        let p = mkProfile (Just localPlayer) [tagAcolyte] []
        isProfileCommandable localPlayer p `shouldBe` True
        isProfileCommandable otherPlayer p `shouldBe` False

    it "commands an unowned profile through the capability only" $ do
        let p = mkProfile Nothing [] [CapLocalCommandable]
        isProfilePlayerOwned localPlayer p `shouldBe` False
        isProfileCommandable localPlayer p `shouldBe` True
        -- Commandability is observer-relative through ownership, but the
        -- capability itself is not: it is a property of the profile.
        isProfileCommandable otherPlayer p `shouldBe` True

    it "reports unrestricted combat from the capability alone" $ do
        profileHasUnrestrictedCombat (mkProfile Nothing [] [CapUnrestrictedCombat])
            `shouldBe` True
        profileHasUnrestrictedCombat (mkProfile Nothing [] [CapLocalCommandable])
            `shouldBe` False
        profileHasUnrestrictedCombat emptyProfile `shouldBe` False

    it "carries both capabilities in allFactionCapabilities" $ do
        allFactionCapabilities
            `shouldBe` [CapLocalCommandable, CapUnrestrictedCombat]
        profileCapabilities (mkProfile Nothing [] allFactionCapabilities)
            `shouldBe` Set.fromList [CapLocalCommandable, CapUnrestrictedCombat]

    it "keeps capabilities out of the relation query" $ do
        -- Two profiles differing ONLY in capabilities relate identically:
        -- capabilities are not tags (D-24).
        let bare    = mkProfile Nothing [tag "alpha"] []
            capable = mkProfile Nothing [tag "alpha"] allFactionCapabilities
            other   = mkProfile Nothing [tagAcolyte] []
        relate legacyBase bare other `shouldBe` relate legacyBase capable other
        relate legacyBase other bare `shouldBe` relate legacyBase other capable

-- * Precedence

precedenceSpec ∷ Spec
precedenceSpec = describe "directed relation precedence" $ do
    -- Tiers, highest first: (1) live cause, (2) same controller,
    -- (3) shared tag, (4) base table, (5) neutral default. Tiers 2 and 3
    -- both answer ally, so their relative order has no observable
    -- consequence; every other ordered pair of tiers is asserted here.

    it "a live cause outranks the same controller" $ do
        let a   = mkProfile (Just localPlayer) [tagFightA] []
            b   = mkProfile (Just localPlayer) [tagFightB] []
            pol = addRelationCause causeOrder tagFightA tagFightB
                                   RelHostile emptyFactionPolicy
        relate emptyFactionPolicy a b `shouldBe` RelAlly
        relate pol a b `shouldBe` RelHostile

    it "a live cause outranks a shared tag" $ do
        let a   = mkProfile Nothing [tagAcolyte, tagTeamA] []
            b   = mkProfile Nothing [tagAcolyte, tagTeamB] []
            pol = addRelationCause causeOrder tagTeamA tagTeamB
                                   RelHostile emptyFactionPolicy
        relate emptyFactionPolicy a b `shouldBe` RelAlly
        relate pol a b `shouldBe` RelHostile

    it "a live cause outranks the base table in both directions" $ do
        let a   = mkProfile Nothing [tagAcolyte] []
            b   = mkProfile Nothing [tagNomad] []
            calm = addRelationCause causeDiplomacy tagAcolyte tagNomad
                                    RelAlly legacyBase
        relate legacyBase a b `shouldBe` RelHostile
        relate calm a b `shouldBe` RelAlly

    it "a live cause outranks the neutral default" $ do
        let a   = mkProfile Nothing [tag "alpha"] []
            b   = mkProfile Nothing [tag "beta"] []
            pol = addRelationCause causeOrder (tag "alpha") (tag "beta")
                                   RelHostile emptyFactionPolicy
        relate emptyFactionPolicy a b `shouldBe` RelNeutral
        relate pol a b `shouldBe` RelHostile

    it "the same controller outranks the base table" $ do
        let a = mkProfile (Just localPlayer) [tagAcolyte] []
            b = mkProfile (Just localPlayer) [tagNomad] []
        -- The same tags without the shared controller are hostile.
        relate legacyBase (mkProfile Nothing [tagAcolyte] [])
                          (mkProfile Nothing [tagNomad] [])
            `shouldBe` RelHostile
        relate legacyBase a b `shouldBe` RelAlly

    it "the same controller outranks the neutral default" $ do
        let a = mkProfile (Just aiTeam) [tag "alpha"] []
            b = mkProfile (Just aiTeam) [tag "beta"] []
        relate emptyFactionPolicy a b `shouldBe` RelAlly

    it "a shared tag outranks the base table" $ do
        let a = mkProfile Nothing [tagAcolyte, tagRedTribe] []
            b = mkProfile Nothing [tagNomad,   tagRedTribe] []
        relate legacyBase a b `shouldBe` RelAlly

    it "a shared tag outranks the neutral default" $ do
        let a = mkProfile Nothing [tagRedTribe, tag "alpha"] []
            b = mkProfile Nothing [tagRedTribe, tag "beta"] []
        relate emptyFactionPolicy a b `shouldBe` RelAlly

    it "the base table outranks the neutral default" $ do
        let a = mkProfile Nothing [tagAcolyte] []
            b = mkProfile Nothing [tagWildlife] []
        relate emptyFactionPolicy a b `shouldBe` RelNeutral
        relate legacyBase a b `shouldBe` RelHostile

    it "answers neutral when no tier applies" $ do
        let a = mkProfile Nothing [tagNomad] []
            b = mkProfile Nothing [tagWildlife] []
        -- D-28 leaves nomad/wildlife undeclared on purpose.
        relate legacyBase a b `shouldBe` RelNeutral

    it "reduces many overlapping tag pairs by severity, not by order" $ do
        -- Four tags each, two base pairs matching with different values.
        let a = mkProfile Nothing [tag "a1", tag "a2", tag "a3", tag "a4"] []
            b = mkProfile Nothing [tag "b1", tag "b2", tag "b3", tag "b4"] []
            pol = mkFactionPolicy
                [ DirectedBase (tag "a4") (tag "b1") RelAlly
                , DirectedBase (tag "a1") (tag "b3") RelHostile
                , DirectedBase (tag "a2") (tag "b2") RelNeutral ]
            reordered = mkFactionPolicy
                [ DirectedBase (tag "a2") (tag "b2") RelNeutral
                , DirectedBase (tag "a1") (tag "b3") RelHostile
                , DirectedBase (tag "a4") (tag "b1") RelAlly ]
        relate pol a b `shouldBe` RelHostile
        relate reordered a b `shouldBe` RelHostile

    it "lets a neutral base pair suppress an ally base pair" $ do
        let a = mkProfile Nothing [tag "a1", tag "a2"] []
            b = mkProfile Nothing [tag "b1", tag "b2"] []
            pol = mkFactionPolicy
                [ DirectedBase (tag "a1") (tag "b1") RelAlly
                , DirectedBase (tag "a2") (tag "b2") RelNeutral ]
        relate pol a b `shouldBe` RelNeutral

    it "spells the severity order out explicitly" $
        map relationSeverity [RelAlly, RelNeutral, RelHostile]
            `shouldBe` [0, 1, 2]

-- * The live overlay

liveOverlaySpec ∷ Spec
liveOverlaySpec = describe "live directed causes" $ do
    let a = mkProfile Nothing [tagTeamA] []
        b = mkProfile Nothing [tagTeamB] []
        withCauses order = foldl (\pol (cid, r) →
                addRelationCause cid tagTeamA tagTeamB r pol)
            emptyFactionPolicy order

    it "reduces hostile over neutral over ally in every insertion order" $ do
        let causes = [ (causeOrder, RelHostile)
                     , (causeReport, RelNeutral)
                     , (causeDiplomacy, RelAlly) ]
            orders = [ causes
                     , reverse causes
                     , [causes !! 1, causes !! 2, causes !! 0]
                     , [causes !! 2, causes !! 0, causes !! 1]
                     , [causes !! 1, causes !! 0, causes !! 2]
                     , [causes !! 0, causes !! 2, causes !! 1] ]
        map (\o → relate (withCauses o) a b) orders
            `shouldBe` replicate (length orders) RelHostile

    it "lets a live neutral cause suppress a live ally cause" $ do
        let pol = withCauses [ (causeDiplomacy, RelAlly)
                             , (causeReport, RelNeutral) ]
            flipped = withCauses [ (causeReport, RelNeutral)
                                 , (causeDiplomacy, RelAlly) ]
        relate pol a b `shouldBe` RelNeutral
        relate flipped a b `shouldBe` RelNeutral

    it "answers ally only when every live cause is ally" $ do
        let pol = withCauses [ (causeDiplomacy, RelAlly)
                             , (causeReport, RelAlly) ]
        relate pol a b `shouldBe` RelAlly

    it "counts the same cause identity added twice as one cause" $ do
        let once  = addRelationCause causeOrder tagTeamA tagTeamB
                                     RelHostile emptyFactionPolicy
            twice = addRelationCause causeOrder tagTeamA tagTeamB
                                     RelHostile once
        Map.size (liveCausesFor twice tagTeamA tagTeamB) `shouldBe` 1
        twice `shouldBe` once
        -- One removal therefore clears it: a duplicate add did not
        -- install a second cause that would survive.
        relate (removeRelationCause causeOrder tagTeamA tagTeamB twice) a b
            `shouldBe` RelNeutral

    it "merges a conflicting value under one identity by severity" $ do
        -- Re-adding an identity with a different relation must not be
        -- last-write-wins, or insertion order would decide the answer.
        -- Softening is done by REMOVING the cause (D-15).
        let hostileFirst =
                addRelationCause causeOrder tagTeamA tagTeamB RelAlly
                    (addRelationCause causeOrder tagTeamA tagTeamB
                                      RelHostile emptyFactionPolicy)
            allyFirst =
                addRelationCause causeOrder tagTeamA tagTeamB RelHostile
                    (addRelationCause causeOrder tagTeamA tagTeamB
                                      RelAlly emptyFactionPolicy)
        hostileFirst `shouldBe` allyFirst
        Map.size (liveCausesFor hostileFirst tagTeamA tagTeamB) `shouldBe` 1
        relate hostileFirst a b `shouldBe` RelHostile
        relate allyFirst a b `shouldBe` RelHostile
        relate (removeRelationCause causeOrder tagTeamA tagTeamB hostileFirst) a b
            `shouldBe` RelNeutral

    it "removes one cause without disturbing another's" $ do
        let pol = withCauses [ (causeOrder, RelHostile)
                             , (causeReport, RelHostile) ]
            afterOne = removeRelationCause causeOrder tagTeamA tagTeamB pol
        relate pol a b `shouldBe` RelHostile
        relate afterOne a b `shouldBe` RelHostile
        Map.keys (liveCausesFor afterOne tagTeamA tagTeamB)
            `shouldBe` [causeReport]
        relate (removeRelationCause causeReport tagTeamA tagTeamB afterOne) a b
            `shouldBe` RelNeutral

    it "ignores the removal of a cause that was never installed" $ do
        let pol = withCauses [(causeOrder, RelHostile)]
        removeRelationCause causeDiplomacy tagTeamA tagTeamB pol
            `shouldBe` pol
        removeRelationCause causeOrder tagTeamB tagTeamA pol `shouldBe` pol
        removeRelationCause causeOrder tagTeamA tagTeamB emptyFactionPolicy
            `shouldBe` emptyFactionPolicy

    it "restores the shared-tag result when the last cause is removed" $ do
        let x = mkProfile Nothing [tagAcolyte, tagTeamA] []
            y = mkProfile Nothing [tagAcolyte, tagTeamB] []
            pol = addRelationCause causeOrder tagTeamA tagTeamB
                                   RelHostile emptyFactionPolicy
        relate pol x y `shouldBe` RelHostile
        relate (removeRelationCause causeOrder tagTeamA tagTeamB pol) x y
            `shouldBe` RelAlly

    it "restores the controller result when the last cause is removed" $ do
        let x = mkProfile (Just localPlayer) [tagFightA] []
            y = mkProfile (Just localPlayer) [tagFightB] []
            pol = addRelationCause causeOrder tagFightA tagFightB
                                   RelHostile emptyFactionPolicy
        relate pol x y `shouldBe` RelHostile
        relate (removeRelationCause causeOrder tagFightA tagFightB pol) x y
            `shouldBe` RelAlly

    it "restores the base-table result when the last cause is removed" $ do
        let x = mkProfile Nothing [tagAcolyte] []
            y = mkProfile Nothing [tagNomad] []
            pol = addRelationCause causeDiplomacy tagAcolyte tagNomad
                                   RelAlly legacyBase
        relate pol x y `shouldBe` RelAlly
        relate (removeRelationCause causeDiplomacy tagAcolyte tagNomad pol) x y
            `shouldBe` RelHostile

    it "restores the neutral default when the last cause is removed" $ do
        let pol = withCauses [(causeOrder, RelHostile)]
        relate (removeRelationCause causeOrder tagTeamA tagTeamB pol) a b
            `shouldBe` RelNeutral

    it "keeps a cause scoped to its own ordered tag pair" $ do
        let pol = addRelationCause causeOrder tagTeamA tagTeamB
                                   RelHostile emptyFactionPolicy
        relate pol a b `shouldBe` RelHostile
        relate pol b a `shouldBe` RelNeutral
        liveCausesFor pol tagTeamB tagTeamA `shouldBe` Map.empty

    it "round-trips a cause identity through its text" $
        relationCauseIdText causeOrder `shouldBe` "attack_order_7"

-- * The base table

baseTableSpec ∷ Spec
baseTableSpec = describe "base relation table" $ do
    it "expands the symmetric shorthand into both directions" $ do
        let pol = mkFactionPolicy [SymmetricBase tagTeamA tagTeamB RelHostile]
        baseRelationFor pol tagTeamA tagTeamB `shouldBe` Just RelHostile
        baseRelationFor pol tagTeamB tagTeamA `shouldBe` Just RelHostile

    it "leaves a directed entry one-way" $ do
        let pol = mkFactionPolicy [DirectedBase tagTeamA tagTeamB RelHostile]
            a   = mkProfile Nothing [tagTeamA] []
            b   = mkProfile Nothing [tagTeamB] []
        baseRelationFor pol tagTeamA tagTeamB `shouldBe` Just RelHostile
        baseRelationFor pol tagTeamB tagTeamA `shouldBe` Nothing
        relate pol a b `shouldBe` RelHostile
        relate pol b a `shouldBe` RelNeutral

    it "has no entry for an undeclared pair" $ do
        baseRelationFor legacyBase tagNomad tagWildlife `shouldBe` Nothing
        baseRelationFor legacyBase tagNomad tagLegacyHostile `shouldBe` Nothing

    it "declares exactly the four D-28 pairs, symmetrically" $ do
        let declared = [ (tagAcolyte, tagNomad), (tagAcolyte, tagWildlife)
                       , (tagLegacyHostile, tagAcolyte)
                       , (tagLegacyHostile, tagWildlife) ]
        map (\(s, t) → baseRelationFor legacyBase s t) declared
            `shouldBe` replicate 4 (Just RelHostile)
        map (\(s, t) → baseRelationFor legacyBase t s) declared
            `shouldBe` replicate 4 (Just RelHostile)

    it "builds the same table whatever order the entries arrive in" $ do
        let entries = [ SymmetricBase tagAcolyte tagNomad RelHostile
                      , SymmetricBase tagAcolyte tagWildlife RelHostile
                      , SymmetricBase tagLegacyHostile tagAcolyte RelHostile
                      , SymmetricBase tagLegacyHostile tagWildlife RelHostile ]
        mkFactionPolicy (reverse entries) `shouldBe` legacyBase

    it "is empty in emptyFactionPolicy" $ do
        baseRelationFor emptyFactionPolicy tagAcolyte tagNomad
            `shouldBe` Nothing
        liveCausesFor emptyFactionPolicy tagAcolyte tagNomad
            `shouldBe` Map.empty

-- * Reverse hostility

reverseHostilitySpec ∷ Spec
reverseHostilitySpec = describe "reverse-hostility predicate" $ do
    let a = mkProfile Nothing [tagTeamA] []
        b = mkProfile Nothing [tagTeamB] []

    it "is false with no live cause at all" $
        hasLiveReverseHostility emptyFactionPolicy a b `shouldBe` False

    it "is false for forward-only live hostility" $ do
        let pol = addRelationCause causeOrder tagTeamA tagTeamB
                                   RelHostile emptyFactionPolicy
        relate pol a b `shouldBe` RelHostile
        hasLiveReverseHostility pol a b `shouldBe` False

    it "is false for reverse base-table hostility alone" $ do
        let x   = mkProfile Nothing [tagAcolyte] []
            y   = mkProfile Nothing [tagNomad] []
        -- The base table is hostile in BOTH directions here, and that is
        -- still not an installed reaction to anything the subject did.
        relate legacyBase y x `shouldBe` RelHostile
        hasLiveReverseHostility legacyBase x y `shouldBe` False

    it "is false for unrestricted combat alone" $ do
        let x = mkProfile Nothing [tagTeamA] allFactionCapabilities
            y = mkProfile Nothing [tagTeamB] allFactionCapabilities
        canProfileAttack emptyFactionPolicy y x `shouldBe` True
        hasLiveReverseHostility emptyFactionPolicy x y `shouldBe` False

    it "is false for a reverse live cause that is not hostile" $ do
        let pol = addRelationCause causeReport tagTeamB tagTeamA
                                   RelNeutral emptyFactionPolicy
        hasLiveReverseHostility pol a b `shouldBe` False

    it "is true for a matching reverse live hostile cause" $ do
        let pol = addRelationCause causeReport tagTeamB tagTeamA
                                   RelHostile emptyFactionPolicy
        hasLiveReverseHostility pol a b `shouldBe` True
        -- Directed: the subject is not itself the target of the question.
        hasLiveReverseHostility pol b a `shouldBe` False

    it "is false once the last reverse hostile cause is removed" $ do
        let pol = addRelationCause causeDiplomacy tagTeamB tagTeamA RelHostile
                    (addRelationCause causeReport tagTeamB tagTeamA
                                      RelHostile emptyFactionPolicy)
            afterOne = removeRelationCause causeReport tagTeamB tagTeamA pol
        hasLiveReverseHostility afterOne a b `shouldBe` True
        hasLiveReverseHostility
            (removeRelationCause causeDiplomacy tagTeamB tagTeamA afterOne) a b
            `shouldBe` False

    it "sees a reverse cause on any of the profiles' tags" $ do
        let x   = mkProfile Nothing [tagAcolyte, tagTeamA] []
            y   = mkProfile Nothing [tagNomad, tagTeamB] []
            pol = addRelationCause causeReport tagTeamB tagAcolyte
                                   RelHostile emptyFactionPolicy
        hasLiveReverseHostility pol x y `shouldBe` True

-- * The empty profile

emptyProfileSpec ∷ Spec
emptyProfileSpec = describe "the empty profile" $ do
    let others = [ emptyProfile
                 , mkProfile (Just localPlayer) [tagAcolyte] []
                 , mkProfile Nothing [tagWildlife] []
                 , mkProfile Nothing [tagLegacyHostile] []
                 , mkProfile Nothing [] allFactionCapabilities ]

    it "has no controller, no tags, and no capabilities" $ do
        profileController emptyProfile   `shouldBe` Nothing
        profileTags emptyProfile         `shouldBe` Set.empty
        profileCapabilities emptyProfile `shouldBe` Set.empty

    it "is neutral toward everything, in both directions" $ do
        map (relate legacyBase emptyProfile) others
            `shouldBe` replicate (length others) RelNeutral
        map (\o → relate legacyBase o emptyProfile) others
            `shouldBe` replicate (length others) RelNeutral

    it "owns nothing and commands nothing" $ do
        isProfilePlayerOwned localPlayer emptyProfile `shouldBe` False
        isProfileCommandable localPlayer emptyProfile `shouldBe` False
        profileHasUnrestrictedCombat emptyProfile `shouldBe` False

    it "may neither attack nor be attacked without unrestricted combat" $ do
        let plain = mkProfile Nothing [tagWildlife] []
        canProfileAttack legacyBase emptyProfile plain `shouldBe` False
        canProfileAttack legacyBase plain emptyProfile `shouldBe` False
        canProfileAttack legacyBase emptyProfile emptyProfile `shouldBe` False

    it "is attackable through unrestricted combat on the other side" $ do
        let bypass = mkProfile Nothing [] [CapUnrestrictedCombat]
        canProfileAttack legacyBase bypass emptyProfile `shouldBe` True
        canProfileAttack legacyBase emptyProfile bypass `shouldBe` True

-- * The design's desired-experience table

desiredExperienceSpec ∷ Spec
desiredExperienceSpec = describe "the design's desired experience" $ do
    it "allies a red-tribe acolyte and a red-tribe nomad" $ do
        let acolyte = mkProfile Nothing [tagAcolyte, tagRedTribe] []
            nomad   = mkProfile Nothing [tagNomad,   tagRedTribe] []
        relate legacyBase acolyte nomad `shouldBe` RelAlly
        relate legacyBase nomad acolyte `shouldBe` RelAlly

    it "keeps an ordinary wilderness nomad hostile to an acolyte" $ do
        let acolyte = mkProfile (Just localPlayer) [tagAcolyte] []
            nomad   = mkProfile Nothing [tagNomad] []
        relate legacyBase acolyte nomad `shouldBe` RelHostile
        relate legacyBase nomad acolyte `shouldBe` RelHostile
        isProfilePlayerOwned localPlayer nomad `shouldBe` False
        isProfileCommandable localPlayer nomad `shouldBe` False

    it "allies the local player's nomad with the local player's acolyte" $ do
        let acolyte = mkProfile (Just localPlayer) [tagPlayerTeam, tagAcolyte] []
            nomad   = mkProfile (Just localPlayer) [tagPlayerTeam, tagNomad] []
        relate legacyBase acolyte nomad `shouldBe` RelAlly
        isProfilePlayerOwned localPlayer nomad `shouldBe` True
        isProfileCommandable localPlayer nomad `shouldBe` True

    it "allies two distinct acolyte teams until diplomacy says otherwise" $ do
        let teamA = mkProfile (Just localPlayer) [tagAcolyte, tagTeamA] []
            teamB = mkProfile (Just otherPlayer) [tagAcolyte, tagTeamB] []
            atWar = addRelationCause causeOrder tagTeamA tagTeamB
                                     RelHostile legacyBase
        relate legacyBase teamA teamB `shouldBe` RelAlly
        relate atWar teamA teamB `shouldBe` RelHostile
        -- Conflict begins through tag diplomacy, never through controller
        -- inequality: B has not answered yet.
        relate atWar teamB teamA `shouldBe` RelAlly

    it "allies two naturally occurring uncontrolled acolytes" $ do
        let x = mkProfile Nothing [tagAcolyte] []
            y = mkProfile Nothing [tagAcolyte] []
        relate legacyBase x y `shouldBe` RelAlly

    it "commands an AI-controlled acolyte team from its own authority" $ do
        let team = mkProfile (Just aiTeam) [tagAcolyte, tagTeamA] []
        isProfilePlayerOwned localPlayer team `shouldBe` False
        isProfilePlayerOwned aiTeam team `shouldBe` True
        -- Diplomacy still relates its team tag like anyone else's.
        relate (addRelationCause causeOrder tagTeamA tagTeamB RelHostile legacyBase)
               team (mkProfile Nothing [tagTeamB] [])
            `shouldBe` RelHostile

    it "fights one controller's roster through temporary conflict tags" $ do
        let sideA = mkProfile (Just localPlayer)
                        [tagPlayerTeam, tagAcolyte, tagFightA] []
            sideB = mkProfile (Just localPlayer)
                        [tagPlayerTeam, tagAcolyte, tagFightB] []
            fight = addRelationCause causeOrder tagFightA tagFightB
                                     RelHostile legacyBase
        relate legacyBase sideA sideB `shouldBe` RelAlly
        relate fight sideA sideB `shouldBe` RelHostile
        canProfileAttack fight sideA sideB `shouldBe` True
        -- A-to-B without B-to-A: B has not been informed.
        relate fight sideB sideA `shouldBe` RelAlly
        canProfileAttack fight sideB sideA `shouldBe` False
        hasLiveReverseHostility fight sideA sideB `shouldBe` False
        -- Removing the order-owned cause reunifies the roster.
        relate (removeRelationCause causeOrder tagFightA tagFightB fight)
               sideA sideB
            `shouldBe` RelAlly

    it "escalates once team B's reverse hostility is installed" $ do
        let sideA = mkProfile (Just localPlayer) [tagFightA] []
            sideB = mkProfile (Just localPlayer) [tagFightB] []
            fight = addRelationCause causeOrder tagFightA tagFightB
                                     RelHostile legacyBase
            escalated = addRelationCause causeReport tagFightB tagFightA
                                         RelHostile fight
        hasLiveReverseHostility escalated sideA sideB `shouldBe` True
        relate escalated sideB sideA `shouldBe` RelHostile
        -- Clearing A's own cause does not clear B's knowledge.
        hasLiveReverseHostility
            (removeRelationCause causeOrder tagFightA tagFightB escalated)
            sideA sideB
            `shouldBe` True

-- * Legacy compatibility

legacyMatrixSpec ∷ Spec
legacyMatrixSpec = describe "legacy compatibility" $ do
    it "builds the D-26 profile for each legacy value" $ do
        profileController (profileOf FactionPlayer) `shouldBe` Just localPlayer
        profileTags (profileOf FactionPlayer) `shouldBe` Set.singleton tagAcolyte
        profileCapabilities (profileOf FactionPlayer) `shouldBe` Set.empty

        profileController (profileOf FactionWildlife) `shouldBe` Nothing
        profileTags (profileOf FactionWildlife)
            `shouldBe` Set.singleton tagWildlife

        profileController (profileOf FactionHostile) `shouldBe` Nothing
        profileTags (profileOf FactionHostile)
            `shouldBe` Set.singleton tagLegacyHostile

        profileOf FactionNeutral `shouldBe` emptyProfile

        profileController (profileOf FactionDebug) `shouldBe` Nothing
        profileTags (profileOf FactionDebug) `shouldBe` Set.empty
        profileCapabilities (profileOf FactionDebug)
            `shouldBe` Set.fromList allFactionCapabilities

    it "covers all five legacy values" $
        legacyOrder `shouldBe` allFactions

    it "reproduces the documented 5x5 relation matrix" $
        map (\a → map (\b → relate legacyBase (profileOf a) (profileOf b))
                      legacyOrder)
            legacyOrder
            `shouldBe` expectedLegacyRelations

    it "differs from the scalar model in exactly eight ordered pairs" $ do
        let differing = [ (a, b)
                        | (a, b) ← legacyPairs
                        , relate legacyBase (profileOf a) (profileOf b)
                            ≢ factionRelation a b ]
        differing `shouldBe` expectedLegacyDifferences

    it "answers neutral on every one of those eight pairs" $
        map (\(a, b) → relate legacyBase (profileOf a) (profileOf b))
            expectedLegacyDifferences
            `shouldBe` replicate 8 RelNeutral

    it "matches canAttack on all 25 ordered pairs" $
        map (\(a, b) → canProfileAttack legacyBase (profileOf a) (profileOf b))
            legacyPairs
            `shouldBe` map (\(a, b) → canAttack a b) legacyPairs

    it "permits every debug direction through the capability" $ do
        -- The six debug pairs that stopped being hostile: still attackable.
        let debugPairs = [ (a, b)
                         | (a, b) ← legacyPairs
                         , a ≡ FactionDebug ∨ b ≡ FactionDebug ]
        map (\(a, b) → canProfileAttack legacyBase (profileOf a) (profileOf b))
            debugPairs
            `shouldBe` replicate (length debugPairs) True

    it "matches the scalar properties for all five values" $ do
        map (isProfilePlayerOwned localPlayer ∘ profileOf) legacyOrder
            `shouldBe` map isPlayerOwned legacyOrder
        map (isProfileCommandable localPlayer ∘ profileOf) legacyOrder
            `shouldBe` map isPlayerCommandable legacyOrder
        map (profileHasUnrestrictedCombat ∘ profileOf) legacyOrder
            `shouldBe` map hasUnrestrictedCombat legacyOrder
