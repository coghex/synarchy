{-# LANGUAGE Strict #-}
-- | Deterministic concept-root derivation (#710 requirements 7, 8, 9,
--   16). Every concept gets a native root built from syllables of the
--   profile's own consonant/vowel inventory and shapes — a pure
--   function of (profile, concept id, retry attempt), never of
--   spelling, lookup order, or any other concept's assignment.
--
--   Which concept wins a COLLISION is the one place order matters, and
--   that order is the catalogue's recorded append-only
--   'ConceptOrdinals' (#1868), supplied by the caller rather than read
--   from disk here: this module stays pure, and there is no order it
--   could reconstruct on its own.
module Language.Generated.Root
    ( generateRoot
    , assignRoots
    , assignLanguageRoots
    , minNativeWordLength
    , RootCapacity(..)
    , rootSpaceCapacity
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types (ConceptId(..), ConceptOrdinals, placementOrder)
import Language.Generated.Types
import Language.Generated.Hash
import Language.Generated.Boundary (joinMorphemes, joinSyllables)
import Language.Generated.Bound (LanguageRoots(..), assignBoundForms)

-- | Assign every concept in @ids@ a stable, unique native root under
--   @prof@. Concepts are processed in the catalogue's recorded
--   'ConceptOrdinals' placement order — the catalogue's own intrinsic
--   order, not whatever order @ids@ happens to arrive in — so two calls
--   over the same concept set always agree regardless of the caller's
--   traversal order (#710 requirement 8).
--
--   A raw root that collides (case-insensitively) with an
--   earlier-placed root in that canonical order is deterministically
--   rerolled via an incrementing attempt counter until it is unique;
--   the reroll depends only on (profile, concept id, attempt), so which
--   concept "already had" the colliding root is fixed by placement
--   order, not by request order (#710 requirement 16).
--
--   That reroll is unbounded, and deliberately stays so — the sequence
--   it walks is part of every existing language's output. What guards
--   it is a CAPACITY GATE in front (#2206): a profile whose own
--   production rules cannot render one distinct root per concept could
--   never finish the walk, so it is rejected with
--   'Language.Generated.Types.InsufficientRootSpace' before the first
--   placement rather than looping forever. The gate can only reject a
--   profile that would have hung: a completed assignment produced a
--   distinct root per concept by construction, so its capacity was
--   already at or above the requirement, at every supported version and
--   for every persisted seed. Sufficient profiles reach the identical
--   'foldl'' below and render byte-identical output.
--
--   The placement order is an APPEND-ONLY ordinal rather than ascending
--   id order (#1868). That is how assignment currently works, and the
--   reason is worth stating: because a reroll takes @attempt + 1@ into
--   'Language.Generated.Hash.conceptSeed', a displaced concept does not
--   get a near variant of its root — it gets an entirely different one.
--   Under ascending-id placement a NEWLY ADDED id that sorted before an
--   incumbent could take the root that incumbent would have had and
--   displace it exactly that way, which silently costs every persisted
--   'Language.Etymology.Source.EtymologySource' naming it its etymology
--   (the name itself is write-once, #1101, so nothing visible changes).
--   An appended ordinal cannot displace anything already placed, so
--   today an addition leaves every existing concept's FREE root alone.
--   Note the scope: from generator version 4 on, bound-form selection
--   ranks the complete current concept set
--   ('Language.Generated.Bound.assignBoundForms'), so an addition can
--   still move a bound form and the rendered names that use one. This
--   paragraph describes the mechanism as built; it is not a promise
--   binding a future change to assignment.
assignRoots ∷ Profile → ConceptOrdinals → [ConceptId]
            → Either GeneratorError (M.Map ConceptId Text)
assignRoots prof ords ids = case rootSpaceCapacity prof required of
    RootCapacityShort capacity → Left
        (InsufficientRootSpace (profVersion prof) (profSeed prof)
                                capacity required)
    RootCapacitySufficient → Right (foldl' place M.empty placed)
  where
    placed = placementOrder ords ids

    -- What the reroll below actually consumes: one distinct root per
    -- distinct concept, whatever order they arrive in.
    required = S.size (S.fromList placed)

    place acc cid =
        let usedLower = S.fromList (map T.toLower (M.elems acc))
            root = resolve cid 0 usedLower
        in M.insert cid root acc

    resolve cid attempt usedLower =
        let candidate = generateRoot prof cid attempt
        in if S.member (T.toLower candidate) usedLower
           then resolve cid (attempt + 1) usedLower
           else candidate

-- | A language's COMPLETE morpheme assignment: every concept's free
--   root, plus the bound form the few concepts #1096 selects
--   additionally get.
--
--   Bound-form selection is layered strictly ON TOP of 'assignRoots' —
--   it reads the finished free-root map and never influences it, so a
--   bound-form collision can no more reroll a free root than it can
--   change the catalogue. For any version below
--   'Language.Generated.Bound.boundFormVersion' the bound map is empty
--   and this is exactly 'assignRoots' in a wrapper.
assignLanguageRoots ∷ Profile → ConceptOrdinals → [ConceptId]
                    → Either GeneratorError LanguageRoots
assignLanguageRoots prof ords ids = do
    free ← assignRoots prof ords ids
    pure LanguageRoots { lrFree = free, lrBound = assignBoundForms prof free }

-- | The native root generated for one concept at one retry attempt.
--   Attempt 0 is the first candidate every caller sees before any
--   collision resolution; 'assignRoots' is the only caller that ever
--   passes a nonzero attempt.
generateRoot ∷ Profile → ConceptId → Int → Text
generateRoot prof cid attempt =
    let baseSeed = conceptSeed (profVersion prof) (profSeed prof) cid attempt
        extra = wordInRange (draw baseSeed 0) 0 (profMaxSyllables prof - profMinSyllables prof)
        targetSyll = profMinSyllables prof + extra
        (raw, nextStep) = buildSyllables prof baseSeed targetSyll 1
    in ensureMinLength prof baseSeed raw nextStep

-- | A rendered root must stand alone as a Bare-form proper name, so it
--   alone must already clear the 3-character floor of #710 requirement
--   6 — a compound built from two such roots is always longer still.
minNativeWordLength ∷ Int
minNativeWordLength = 3

-- | Top up a root that came out below the floor with whole extra
--   syllables — one of #1095's four NAMED boundary sites, so the
--   appended material meets the existing text through 'joinMorphemes':
--   the full repair, consulting #1094's admissibility relation, not the
--   triple-only guard the root's own interior syllable joins use. A
--   short @ab@ root meeting an @sa@ top-up would otherwise keep the
--   @bs@ cluster that language rejects.
--
--   The loop still terminates: a repair either inserts a segment or
--   trims the top-up's initial one while leaving it nonempty, so the
--   result is strictly longer than @raw@ every time. Versions 1-2 carry
--   no policy and concatenate raw, so their output stays byte-identical.
ensureMinLength ∷ Profile → Word64 → Text → Int → Text
ensureMinLength prof baseSeed raw step
    | T.length raw ≥ minNativeWordLength = raw
    | otherwise =
        let (extra, step') = buildSyllables prof baseSeed 1 step
        in ensureMinLength prof baseSeed (joinMorphemes prof raw extra) step'

-- | Render @n@ syllables starting at Rng @step@, returning the text and
--   the next unused step (so callers can keep drawing from the same
--   deterministic sequence, e.g. 'ensureMinLength' topping up a root
--   that came out too short).
--
--   Syllables meet through 'joinSyllables', which mediates ONLY a
--   junction that would produce a triple: repairing this join for
--   cluster admissibility as well would rewrite every root the profile's
--   own shapes produce, but leaving it raw would let a bare root carry a
--   triple no later morpheme join could remove (#1095 requirement 3).
buildSyllables ∷ Profile → Word64 → Int → Int → (Text, Int)
buildSyllables _ _ 0 step = ("", step)
buildSyllables prof baseSeed n step =
    let shapes = profSyllableShapes prof
        shape = shapes !! pickIndex (draw baseSeed step) (length shapes)
        (syll, step1) = renderShape prof baseSeed shape (step + 1)
        (rest, step2) = buildSyllables prof baseSeed (n - 1) step1
    in (joinSyllables prof syll rest, step2)

-- | Render one syllable.
--
--   A shape whose first two slots are both consonants (@CCV@ is the
--   only one in the catalogue) is an in-syllable two-consonant ONSET.
--   From version 2 on, that onset is selected as a WHOLE PAIR from the
--   profile's admissible-onset relation with a single indexed draw
--   (#1094 requirement 5) — not two independent consonant draws, and
--   with no rejection sampling, retry, search, or backtracking
--   anywhere. Selection is therefore bounded-time and deterministic,
--   and an identical-consonant onset is impossible because the relation
--   is irreflexive.
--
--   Version 1 carries an empty relation, so it falls through to the
--   historical independent-draw path below and stays byte-identical
--   (#1094 requirement 1). The two consumed steps match the two
--   consonant slots, keeping every later slot's draw where it was.
--
--   Slots are appended through 'joinSyllables' rather than concatenated,
--   for a reason that is easy to miss: a dual-role @y@ (#1094 requirement
--   6) sits in BOTH inventories, so a @CVC@ syllable can draw it into
--   all three slots and render @yyy@ — a triple inside ONE syllable,
--   with no join anywhere near it. Mediating the slot appends is what
--   makes "every piece handed to a morpheme join is itself triple-free"
--   true, which is the induction the whole no-triple guarantee rests on.
--   The guard cannot fire at slots 0-1 (a run needs three), so a word's
--   opening two-consonant onset is never disturbed.
renderShape ∷ Profile → Word64 → SyllableShape → Int → (Text, Int)
renderShape prof baseSeed shape step0 = case shapeSegments shape of
    (ConsonantSlot : ConsonantSlot : rest)
        | pairs@(_ : _) ← onsetPairs (profOnset prof) →
            let pair = pairs !! pickIndex (draw baseSeed step0) (length pairs)
                (letters, step') = drawSlots rest (step0 + 2)
            in (renderSlots prof (Just pair) letters, step')
    segs → let (letters, step') = drawSlots segs step0
           in (renderSlots prof Nothing letters, step')
  where
    -- One indexed draw per slot, in slot order, consuming exactly the
    -- step indices the historical inline fold consumed.
    drawSlots segs step = go segs step []
      where
        go [] s acc = (reverse acc, s)
        go (ConsonantSlot : rest) s acc =
            let cs = profConsonants prof
                c = cs !! pickIndex (draw baseSeed s) (length cs)
            in go rest (s + 1) (c : acc)
        go (VowelSlot : rest) s acc =
            let vs = profVowels prof
                v = vs !! pickIndex (draw baseSeed s) (length vs)
            in go rest (s + 1) (v : acc)

-- | One syllable, assembled from explicit slot choices: the whole-pair
--   onset when the shape took one, and the letters filling the
--   remaining slots in order.
--
--   The seeded path ('renderShape') and the capacity enumeration
--   ('syllableRenders') both go through this one function, so the space
--   'rootSpaceCapacity' measures is the space 'generateRoot' draws
--   from — a second transcription of the slot-append rules could drift
--   from this one, and a capacity that overcounted would let the very
--   hang the gate exists to prevent back in.
renderSlots ∷ Profile → Maybe (Char, Char) → [Char] → Text
renderSlots prof mPair letters = case mPair of
    Nothing     → tl
    Just (a, b) → joinSyllables prof (T.pack [a, b]) tl
  where
    tl = foldl' step "" letters
    step acc c = joinSyllables prof acc (T.singleton c)

-- | How a profile's root space measures against a required concept
--   count (#2206).
data RootCapacity
    = RootCapacitySufficient
      -- ^ Capacity reaches the requirement. Measurement stopped at that
      --   point, so no total is claimed — the space above the
      --   requirement is never enumerated.
    | RootCapacityShort !Int
      -- ^ The EXACT number of distinct case-insensitive roots the
      --   profile can render, which is below the requirement.
    deriving (Show, Eq)

-- | Whether @prof@ can render at least @needed@ distinct
--   case-insensitive roots — the question 'assignRoots' must answer
--   before it starts rerolling collisions.
--
--   Exact, not an estimate. It counts DISTINCT RENDERED values off the
--   profile's own production rules — its inventories, shapes, syllable
--   counts, admissible onsets, min-length repair and the joins each of
--   those goes through — rather than multiplying slot counts, because
--   the naive product overcounts: a dual-role @y@ (#1094 requirement 6)
--   sits in both inventories, so one shape's output can equal another's,
--   and 'joinSyllables' can mediate two different slot fillings onto the
--   same text. An overcount here would accept a profile that then hangs.
--
--   Bounded in both outcomes. A sufficient profile stops the walk the
--   moment the distinct count reaches @needed@, so the enormous spaces
--   ordinary profiles have are never enumerated. An insufficient one is
--   enumerated to the end, which is cheap precisely because it is
--   insufficient — a space too small to hold @needed@ roots is too
--   small to be expensive to walk.
rootSpaceCapacity ∷ Profile → Int → RootCapacity
rootSpaceCapacity prof needed
    | needed ≤ 0 = RootCapacitySufficient
    | otherwise  = walk S.empty (rootSpace prof)
  where
    -- Every recursive call is made with @S.size seen < needed@, so an
    -- exhausted list is always a genuine shortfall.
    walk seen [] = RootCapacityShort (S.size seen)
    walk seen (r : rs)
        | S.size seen' ≥ needed = RootCapacitySufficient
        | otherwise             = walk seen' rs
      -- Folded exactly the way 'assignRoots' folds its own @usedLower@
      -- set. Today's inventories are lowercase throughout, so the fold
      -- changes nothing — it is here because the EQUIVALENCE has to
      -- match the one assignment consumes roots under, not because a
      -- rendered root is observed to need it.
      where seen' = S.insert (T.toLower r) seen

-- | Every root 'generateRoot' can produce, lazily and with duplicates:
--   each syllable count it can target, each sequence of syllables at
--   that count, each min-length repair of a sequence that came out
--   short. 'rootSpaceCapacity' counts the distinct values.
rootSpace ∷ Profile → [Text]
rootSpace prof =
    [ root
    | n    ← [profMinSyllables prof .. profMaxSyllables prof]
    , raw  ← syllableSequences prof n
    , root ← topUps prof raw
    ]

-- | The renderings of exactly @n@ syllables, folded the way
--   'buildSyllables' folds them — right-nested through 'joinSyllables',
--   with the empty tail at the end.
syllableSequences ∷ Profile → Int → [Text]
syllableSequences prof n
    | n ≤ 0     = [""]
    -- The recursive call is bound INSIDE this branch on purpose. This
    -- module is @Strict@, so a @where@ binding is forced on every
    -- equation that has one — including the @n ≤ 0@ one, which would
    -- then recurse forever on negative counts.
    | otherwise =
        let shorter = syllableSequences prof (n - 1)
        in [ joinSyllables prof s rest | s ← syllableRenders prof
                                       , rest ← shorter ]

-- | 'ensureMinLength' as an enumeration: a root already at the floor
--   stands alone, a short one is topped up by one more syllable through
--   'joinMorphemes' — the same repair, so the same results — until it
--   clears the floor.
--
--   Terminating for the repair's own reason: every syllable is at least
--   two characters, and a morpheme join either passes both sides
--   through, inserts a segment, or trims one character off a right side
--   of at least two. The result is strictly longer than @raw@ every
--   time.
topUps ∷ Profile → Text → [Text]
topUps prof raw
    | T.length raw ≥ minNativeWordLength = [raw]
    | otherwise = concat [ topUps prof (joinMorphemes prof raw s)
                         | s ← syllableRenders prof ]

-- | Every syllable this profile can render, across all of its shapes.
syllableRenders ∷ Profile → [Text]
syllableRenders prof = concatMap (shapeRenders prof) (profSyllableShapes prof)

-- | Every rendering of one shape. The two branches mirror
--   'renderShape''s exactly — a two-consonant opening draws a WHOLE
--   admissible pair when the profile has one, and falls through to
--   independent per-slot letters when it does not (version 1's empty
--   relation) — so the enumeration covers precisely what the seeded
--   path can reach.
shapeRenders ∷ Profile → SyllableShape → [Text]
shapeRenders prof shape = case shapeSegments shape of
    (ConsonantSlot : ConsonantSlot : rest)
        | pairs@(_ : _) ← onsetPairs (profOnset prof) →
            [ renderSlots prof (Just pair) letters
            | pair ← pairs, letters ← slotChoices rest ]
    segs → [ renderSlots prof Nothing letters | letters ← slotChoices segs ]
  where
    slotChoices = traverse slotLetters
    slotLetters ConsonantSlot = profConsonants prof
    slotLetters VowelSlot     = profVowels prof
