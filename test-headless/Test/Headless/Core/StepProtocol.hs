-- | Pure transition traces for the dormant coordinated-step protocol (#2482).
-- Fixture rates/budgets are deliberately explicit and are not runtime defaults.
module Test.Headless.Core.StepProtocol (spec) where

import UPrelude
import Test.Hspec
import Data.Either (isLeft)
import Data.Ratio ((%))
import Engine.Core.StepProtocol

fixturePlan ∷ [(Stage, Owner)]
fixturePlan = [(Admission, UnitOwner), (Motion, UnitOwner)
    , (Consequences, CombatOwner), (Consequences, LuaOwner)
    , (Productive, WorldOwner), (Decisions, LuaOwner), (Completion, UnitOwner)]

must ∷ Show ε ⇒ Either ε α → α
must = either (error . show) id

fresh ∷ Protocol
fresh = must (newProtocol 10 (7 % 5) 3 2 fixturePlan)

-- A quarter wall second at four-times fixture speed admits one second.
ready ∷ Protocol
ready = must (sampleWall 0.25 4 (must (sampleWall 0 4 fresh)))

begin ∷ Protocol → Protocol
begin p = must (startStep (scopeValue p [81, 82]) [11, 22, 33] p)

grantOf ∷ Protocol → Grant
grantOf = fromMaybe (error "fixture has no grant") . nextGrant

stepOf ∷ Protocol → StepView
stepOf = fromMaybe (error "fixture has no step") . viewStep . inspectProtocol

completePhase ∷ Protocol → Protocol
completePhase p = must (acknowledgePhase grant applied)
  where
    grant = grantOf p
    executing = case stepProgress (stepOf p) of
        AwaitingGrant → must (acceptGrant grant p)
        Suspended → must (resumePhase grant p)
        Executing → p
    remaining = length (stepMembers (stepOf executing)) - stepCursor (stepOf executing)
    applied = if remaining ≡ 0 then executing
              else must (applyBatch grant remaining executing)

completeStep ∷ Protocol → Protocol
completeStep p
    | viewMode (inspectProtocol p) ≡ RunningStep = completeStep (completePhase p)
    | otherwise = p

-- Seed new pacing samples after every completed fixture step; the input uses
-- the real transition, and committed time never derives from these samples.
oneStep ∷ Protocol → Protocol
oneStep p = completeStep (begin funded)
  where
    funded = must (sampleWall 0.25 4 (must (sampleWall 0 4 p)))

spec ∷ Spec
spec = describe "coordinated simulation step protocol" $ do
    it "rejects duplicate grants and acknowledged phases without applying effects" $ do
        let p = begin ready
            grant = grantOf p
            active = must (acceptGrant grant p)
            credited = must (applyBatch grant 1 active)
            advanced = completePhase credited
        acceptGrant grant credited `shouldBe` Left WrongProgress
        acceptGrant grant advanced `shouldBe` Left WrongGrant
        acknowledgePhase grant advanced `shouldBe` Left WrongGrant
        stepApplied (stepOf credited) `shouldBe` [(grantPhase grant, 11)]
        stepAcknowledged (stepOf advanced) `shouldBe` [grantPhase grant]
        length (stepApplied (stepOf advanced)) `shouldBe` 3

    it "rejects wrong epochs and step ordinals on grants and acknowledgements" $ do
        let p = begin ready
            grant = grantOf p
            active = must (acceptGrant grant p)
            wrong = [grant { grantEpoch = 9 }, grant { grantStep = 0 }
                    ,grant { grantStep = 2 }]
        forM_ wrong $ \bad → do
            acceptGrant bad p `shouldBe` Left WrongGrant
            applyBatch bad 1 active `shouldBe` Left WrongGrant
            acknowledgePhase bad active `shouldBe` Left WrongGrant
        stepApplied (stepOf active) `shouldBe` []
        committedSeconds active `shouldBe` 7 % 5

    it "rejects the wrong owner and not-yet-enabled phase without effects" $ do
        let p = begin ready
            grant = grantOf p
            phase = grantPhase grant
            active = must (acceptGrant grant p)
            wrong = [grant { grantPhase = phase { phaseOwner = WorldOwner } }
                    ,grant { grantPhase = Phase 1 Motion UnitOwner }
                    ,grant { grantEnd = grantEnd grant + 1 }
                    ,grant { grantDuration = 1 }]
        forM_ wrong $ \bad → do
            acceptGrant bad p `shouldBe` Left WrongGrant
            applyBatch bad 1 active `shouldBe` Left WrongGrant
            acknowledgePhase bad active `shouldBe` Left WrongGrant
        stepApplied (stepOf active) `shouldBe` []

    it "admits at most one in-flight step" $ do
        let p = begin ready
        startStep (scopeValue p []) [] p `shouldBe` Left WrongMode
        viewCompleted (inspectProtocol p) `shouldBe` 0
        grantStep (grantOf p) `shouldBe` 1

    it "publishes no time for an incomplete, unacknowledged or failed phase" $ do
        let p = begin ready
            grant = grantOf p
            active = must (acceptGrant grant p)
            partial = must (applyBatch grant 1 active)
            full = must (applyBatch grant 2 partial)
        acknowledgePhase grant p `shouldBe` Left WrongProgress
        acknowledgePhase grant partial `shouldBe` Left IncompletePhase
        forM_ [p, partial, full, completePhase full, faultProtocol full] $ \state →
            committedSeconds state `shouldBe` 7 % 5
        viewCompleted (inspectProtocol (completeStep full)) `shouldBe` 1
        committedSeconds (completeStep full) `shouldBe` 26 % 15

    it "keeps exact origin plus N over rate with no floating-point drift" $ do
        let steps = 1000
            final = iterate oneStep fresh !! steps
        viewCompleted (inspectProtocol final) `shouldBe` fromIntegral steps
        committedSeconds final `shouldBe` 7 % 5 + fromIntegral steps / 3
        -- One rounded public-seconds conversion is explicitly NOT the oracle.
        toRational (fromRational (committedSeconds final) ∷ Double)
            `shouldNotBe` committedSeconds final

    it "validates numeric and phase-plan domains so time cannot be negative" $ do
        forM_ [(0, 1), (-3, 1), (3, -1)] $ \(rate, origin) →
            newProtocol 1 origin rate 2 fixturePlan `shouldBe` Left InvalidParameters
        newProtocol 0 0 3 2 fixturePlan `shouldBe` Left InvalidParameters
        newProtocol 1 0 3 0 fixturePlan `shouldBe` Left InvalidParameters
        newProtocol 1 0 3 2 [] `shouldBe` Left InvalidParameters
        newProtocol 1 0 3 2 (reverse fixturePlan) `shouldBe` Left InvalidParameters
        let zero = must (newProtocol 1 0 3 2 fixturePlan)
        map committedSeconds (take 20 (iterate oneStep zero))
            `shouldSatisfy` all (≥ 0)

    it "caps and counts only unstarted demand while preserving admitted work" $ do
        let p = begin ready
            grant = grantOf p
            active = must (applyBatch grant 1 (must (acceptGrant grant p)))
            overloaded = must (sampleWall 0.5 100 active)
            view = inspectProtocol overloaded
            expectedDiscard = 2 % 3 + 25 - 2
        viewDemand view `shouldBe` 2
        viewDiscardedSeconds view `shouldBe` expectedDiscard
        viewDiscardCount view `shouldBe` 1
        viewStep view `shouldBe` viewStep (inspectProtocol active)
        committedSeconds (completeStep overloaded) `shouldBe` 26 % 15
        viewDemand (inspectProtocol (completeStep overloaded)) `shouldBe` 2

    it "exposes requested pause immediately but settles only after completion" $ do
        let p = begin ready
            pending = requestPause p
        viewPauseRequested (inspectProtocol pending) `shouldBe` True
        viewMode (inspectProtocol pending) `shouldBe` RunningStep
        viewStep (inspectProtocol pending) `shouldBe` viewStep (inspectProtocol p)
        startStep (scopeValue pending []) [] pending `shouldBe` Left WrongMode
        resumeProtocol pending `shouldBe` Left WrongMode
        let settled = completeStep pending
        viewMode (inspectProtocol settled) `shouldBe` Paused
        committedSeconds settled `shouldBe` 26 % 15
        startStep (scopeValue settled []) [] settled `shouldBe` Left WrongMode
        startStep (scopeValue ready []) [] (requestPause ready)
            `shouldBe` Left WrongMode

    it "rebases pause pacing without replay or changing a suspended interval" $ do
        let p = begin ready
            grant = grantOf p
            partial = must (applyBatch grant 1 (must (acceptGrant grant p)))
            suspended = must (suspendPhase grant partial)
            pending = requestPause suspended
            waited = must (sampleWall 100000 100
                (must (sampleWall 99999.75 100 pending)))
        viewDemand (inspectProtocol pending) `shouldBe` 0
        viewDemand (inspectProtocol waited) `shouldBe` 0
        viewStep (inspectProtocol waited) `shouldBe` viewStep (inspectProtocol suspended)
        let settled = completeStep waited
            resumed = must (resumeProtocol settled)
            rebased = must (sampleWall 200000 100 resumed)
        viewDemand (inspectProtocol resumed) `shouldBe` 0
        viewDemand (inspectProtocol rebased) `shouldBe` 0
        startStep (scopeValue rebased []) [] rebased `shouldBe` Left InsufficientDemand
        viewDemand (inspectProtocol (must (sampleWall 200000.125 1 rebased)))
            `shouldBe` 1 % 8

    it "resumes the same membership cursor prefix and interval exactly once" $ do
        let p = begin ready
            grant = grantOf p
            partial = must (applyBatch grant 1 (must (acceptGrant grant p)))
            suspended = must (suspendPhase grant partial)
            resumed = must (resumePhase grant suspended)
        applyBatch grant 1 suspended `shouldBe` Left WrongProgress
        acknowledgePhase grant suspended `shouldBe` Left WrongProgress
        acceptGrant grant suspended `shouldBe` Left WrongProgress
        startStep (scopeValue suspended []) [] suspended `shouldBe` Left WrongMode
        stepOf resumed `shouldBe` stepOf partial
        stepMembers (stepOf resumed) `shouldBe` [11, 22, 33]
        stepPrefix (stepOf resumed) `shouldBe` [81, 82]
        let done = must (applyBatch grant 2 resumed)
        stepApplied (stepOf done) `shouldBe`
            [(grantPhase grant, 11), (grantPhase grant, 22), (grantPhase grant, 33)]
        applyBatch grant 1 done `shouldBe` Left InvalidBatch
        applyBatch grant 0 resumed `shouldBe` Left InvalidBatch

    it "latches faults and refuses every ordinary continuation" $ do
        let p = begin ready
            grant = grantOf p
            failed = faultProtocol (must (acceptGrant grant p))
            waited = must (sampleWall 1000 100 failed)
        viewMode (inspectProtocol waited) `shouldBe` Faulted
        viewDemand (inspectProtocol waited) `shouldBe` 0
        viewMode (inspectProtocol (requestPause waited)) `shouldBe` Faulted
        startStep (scopeValue waited []) [] waited `shouldBe` Left WrongMode
        acceptGrant grant waited `shouldBe` Left WrongMode
        applyBatch grant 1 waited `shouldBe` Left WrongMode
        acknowledgePhase grant waited `shouldBe` Left WrongMode
        resumeProtocol waited `shouldBe` Left WrongMode
        enterTransaction waited `shouldBe` Left WrongMode
        committedSeconds waited `shouldBe` committedSeconds fresh

    it "invalidates old epoch grants prefixes cadence cursors and pacing debt" $ do
        let p = begin ready
            oldGrant = grantOf p
            oldPrefix = scopeValue p [91, 92]
            oldCadence = scopeValue p (2 ∷ Integer, 5 ∷ Integer)
            replacement = must (replaceSession 11 9 p)
            funded = must (sampleWall 100.25 4
                (must (sampleWall 100 4 replacement)))
            newStep = begin funded
        viewEpoch (inspectProtocol replacement) `shouldBe` 11
        viewStep (inspectProtocol replacement) `shouldBe` Nothing
        viewDemand (inspectProtocol replacement) `shouldBe` 0
        viewCompleted (inspectProtocol replacement) `shouldBe` 0
        committedSeconds replacement `shouldBe` 9
        acceptGrant oldGrant newStep `shouldBe` Left WrongGrant
        resolveScoped replacement oldPrefix `shouldBe` Left StaleScope
        startStep oldPrefix [] funded `shouldBe` Left StaleScope
        resolveScoped replacement oldCadence `shouldBe` Left StaleScope
        -- GT-14 may restore identical progress under fresh runtime identity.
        resolveScoped replacement (scopeValue replacement (2 ∷ Integer, 5 ∷ Integer))
            `shouldBe` Right (2, 5)
        replaceSession 10 9 replacement `shouldBe` Left StaleScope
        replaceSession 11 9 replacement `shouldBe` Left StaleScope

    it "keeps transaction and empty-roster completion at whole-step boundaries" $ do
        enterTransaction (begin ready) `shouldBe` Left WrongMode
        let transaction = must (enterTransaction ready)
            waited = must (sampleWall 100000 100 transaction)
        viewMode (inspectProtocol waited) `shouldBe` Transaction
        viewDemand (inspectProtocol waited) `shouldBe` 0
        startStep (scopeValue waited []) [] waited `shouldBe` Left WrongMode
        viewMode (inspectProtocol (must (finishTransaction waited))) `shouldBe` Paused
        let empty = completeStep (must (startStep (scopeValue ready []) [] ready))
        committedSeconds empty `shouldBe` 26 % 15

    it "sanitises wall samples without admitting invalid demand" $ do
        forM_ [0/0, 1/0, -1/0, -20] $ \now → do
            let seeded = must (sampleWall 0 1 fresh)
                sampled = must (sampleWall now 1 seeded)
                next = must (sampleWall (now + 0.125) 1 sampled)

            viewDemand (inspectProtocol sampled) `shouldBe` 0
            viewDemand (inspectProtocol next) `shouldSatisfy` (≥ 0)
        sampleWall 1 (-1) fresh `shouldBe` Left InvalidParameters
        let capped = must (sampleWall 3600 1 (must (sampleWall 0 1 fresh)))
        viewDemand (inspectProtocol capped) `shouldBe` 1 % 4
        viewDemand (inspectProtocol (must (sampleWall 3600.125 1 capped)))
            `shouldBe` 3 % 8
        startStep (scopeValue ready []) [1,1] ready `shouldBe` Left InvalidMembership
        resolveScoped fresh (scopeValue fresh True) `shouldSatisfy` (not . isLeft)
