-- | The "Gameplay.Tags" gate (#2700, epic #2698 SCN-02): the pure
--   sparse tag registry in "Gameplay.Tags.Registry" and the native query
--   evaluator in "Gameplay.Tags.Query".
--
--   Pure fixtures only, no engine. The model-based half drives the
--   registry and an independent reference model — a plain association
--   list evaluated by list comprehension, sharing no 'Data.Map' or
--   'Data.Set' code with the implementation — through the same seeded
--   random mutation sequences and nested queries, and requires them to
--   agree. The case table after it pins the individual contracts.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Gameplay.Tags"'@.
module Test.Headless.Gameplay.Tags (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort, sortOn)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Random as Random
import Building.Types (BuildingId(..))
import Gameplay.Tags.Query
import Gameplay.Tags.Registry
import Gameplay.Tags.Types
import Location.Instance (LocationInstanceId(..))
import Unit.Types.Manager (UnitId(..))
import World.Chunk.Types (chunkSize, wrapChunkCoordU)
import World.Flora.Identity (floraInstanceIdNone, plantedFloraInstanceId)
import World.Page.Types (WorldPageId(..))

spec ∷ Spec
spec = do
    modelSpec
    mutationSpec
    identitySpec
    querySpec

-- * Fixtures

tag ∷ Text → GameplayTag
tag t = fromMaybe (error ("bad fixture tag " <> T.unpack t)) (mkGameplayTag t)

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "page_a"
pageB = WorldPageId "page_b"

unit ∷ Word32 → TagTarget
unit = TargetUnit . UnitId

building ∷ Word32 → TagTarget
building = TargetBuilding . BuildingId

location ∷ WorldPageId → Int → TagTarget
location p i = TargetLocation (LocationTarget p (LocationInstanceId i))

plant ∷ WorldPageId → Word64 → TagTarget
plant p n = TargetPlant
    (fromMaybe (error "bad fixture plant")
               (mkPlantTarget p (plantedFloraInstanceId n)))

item ∷ Word64 → TagTarget
item n = TargetItem (fromMaybe (error "bad fixture item") (mkItemTarget n))

-- | A tile on an 8-chunk wrapped world.
tile ∷ WorldPageId → Int → Int → Int → TagTarget
tile p x y z = TargetTile (mkTileTarget (wrapChunkCoordU 8) p x y z)

tags ∷ [Text] → Set.Set GameplayTag
tags = Set.fromList . map tag

-- * The reference model

-- | The independent reference: an association list of (target, tags),
--   no empty entries. Nothing here touches 'Data.Map' or 'Data.Set'.
type Model = [(TagTarget, [GameplayTag])]

data Op
    = OpAdd TagTarget [GameplayTag]
    | OpRemove TagTarget [GameplayTag]
    | OpReplace TagTarget [GameplayTag]
    | OpClear TagTarget
    deriving Show

refTags ∷ Model → TagTarget → [GameplayTag]
refTags m x = fromMaybe [] (lookup x m)

refSet ∷ Model → TagTarget → [GameplayTag] → Model
refSet m x ts
    | null ts'  = others
    | otherwise = (x, ts') : others
  where
    others = [ e | e@(y, _) ← m, y ≢ x ]
    ts'    = sort (nub ts)

refApply ∷ Model → Op → Model
refApply m = \case
    OpAdd x ts     → refSet m x (refTags m x <> ts)
    OpRemove x ts  → refSet m x [ t | t ← refTags m x, t `notElem` ts ]
    OpReplace x ts → refSet m x ts
    OpClear x      → refSet m x []

applyOp ∷ TagRegistry → Op → TagRegistry
applyOp r = \case
    OpAdd x ts     → addTags x (Set.fromList ts) r
    OpRemove x ts  → removeTags x (Set.fromList ts) r
    OpReplace x ts → replaceTags x (Set.fromList ts) r
    OpClear x      → clearTags x r

refAssignments ∷ Model → [(TagTarget, [GameplayTag])]
refAssignments = sortOn fst

implAssignments ∷ TagRegistry → [(TagTarget, [GameplayTag])]
implAssignments r = [ (x, Set.toAscList ts) | (x, ts) ← assignments r ]

-- | Pointwise truth of a query for one universe member.
refHolds ∷ Model → TagTarget → TagQuery → Bool
refHolds m x = \case
    QUniverse → True
    QMatch f  →
        let ts = refTags m x
        in all (`elem` ts) (Set.toList (tfAll f))
           ∧ (Set.null (tfAny f) ∨ any (`elem` ts) (Set.toList (tfAny f)))
           ∧ not (any (`elem` ts) (Set.toList (tfNone f)))
    QUnion a b        → refHolds m x a ∨ refHolds m x b
    QIntersection a b → refHolds m x a ∧ refHolds m x b
    QDifference a b   → refHolds m x a ∧ not (refHolds m x b)
    QComplement a     → not (refHolds m x a)

refQuery ∷ Model → [TagTarget] → TagQuery → [TagTarget]
refQuery m universe q = sort (nub [ x | x ← universe, refHolds m x q ])

-- * Seeded generation

-- | Every category, with numeric ids that overlap across categories
--   (unit 1, building 1, item 1, location 1 ...) and page-scoped ids
--   repeated on two pages.
targetPool ∷ [TagTarget]
targetPool =
    [ unit 1, unit 2, unit 3
    , building 1, building 2, building 3
    , location pageA 1, location pageA 2, location pageB 1
    , plant pageA 1, plant pageA 2, plant pageB 1
    , item 1, item 2, item 3
    , tile pageA 0 0 0, tile pageA 1 0 0, tile pageA 0 1 1, tile pageB 0 0 0 ]

-- | Targets no mutation ever touches: the universe's untagged majority.
untaggedPool ∷ [TagTarget]
untaggedPool =
    [ unit 9, building 9, location pageA 9, plant pageA 9, item 9
    , tile pageA 5 5 0 ]

tagPool ∷ [GameplayTag]
tagPool = tag <$> ["a", "b", "c", "d", "e"]

-- | Query tags also name a tag nobody carries.
queryTagPool ∷ [GameplayTag]
queryTagPool = tagPool <> [tag "missing"]

type Gen α = Random.StdGen → (α, Random.StdGen)

pick ∷ [α] → Gen α
pick xs g = let (i, g') = Random.uniformR (0, length xs - 1) g in (xs !! i, g')

listOf ∷ Int → Gen α → Gen [α]
listOf maxLen one g0 =
    let (n, g1) = Random.uniformR (0, maxLen) g0
    in go n g1
  where
    go 0 g = ([], g)
    go k g = let (x, g')   = one g
                 (xs, g'') = go (k - 1) g'
             in (x : xs, g'')

genOp ∷ Gen Op
genOp g0 =
    let (kind, g1) = Random.uniformR (0 ∷ Int, 9) g0
        (x, g2)    = pick targetPool g1
        -- Repeats allowed: a list can name one tag twice.
        (ts, g3)   = listOf 3 (pick tagPool) g2
    in ( case kind of
           k | k ≤ 4 → OpAdd x ts
             | k ≤ 6 → OpRemove x ts
             | k ≤ 8 → OpReplace x ts
             | otherwise → OpClear x
       , g3 )

genFilter ∷ Gen TagFilter
genFilter g0 =
    let (a, g1) = listOf 2 (pick queryTagPool) g0
        (o, g2) = listOf 2 (pick queryTagPool) g1
        (n, g3) = listOf 2 (pick queryTagPool) g2
    in (tagFilter a o n, g3)

genQuery ∷ Int → Gen TagQuery
genQuery depth g0 =
    let (kind, g1) = Random.uniformR (0 ∷ Int, if depth ≤ 0 then 1 else 6) g0
    in case kind of
        0 → let (f, g2) = genFilter g1 in (QMatch f, g2)
        1 → if depth ≤ 0 then let (f, g2) = genFilter g1 in (QMatch f, g2)
                         else (QUniverse, g1)
        6 → let (a, g2) = genQuery (depth - 1) g1 in (QComplement a, g2)
        _ → let (a, g2) = genQuery (depth - 1) g1
                (b, g3) = genQuery (depth - 1) g2
                node | kind ≡ 2 ∨ kind ≡ 3 = QUnion a b
                     | kind ≡ 4           = QIntersection a b
                     | otherwise          = QDifference a b
            in (node, g3)

-- | A random subset of every target — the loaded/existing objects.
genUniverse ∷ Gen [TagTarget]
genUniverse g0 = foldr step ([], g0) (targetPool <> untaggedPool)
  where
    step x (acc, g) =
        let (keep, g') = Random.uniformR (0 ∷ Int, 2) g
        in (if keep > 0 then x : acc else acc, g')

-- * Model-based equivalence

modelSpec ∷ Spec
modelSpec = describe "model-based equivalence" $ do
    let runs = [ (seed, runSequence seed) | seed ← [1 .. 150 ∷ Int] ]
        runSequence seed =
            let (ops, _) = listOf 40 genOp (Random.mkStdGen seed)
                steps    = drop 1 (scanl (\(r, m) op → (applyOp r op, refApply m op))
                                         (emptyTagRegistry, []) ops)
            in (ops, steps)

    it "every mutation matches the reference model and keeps both indexes coherent" $
        forM_ runs $ \(seed, (_, steps)) →
            forM_ (zip [1 ∷ Int ..] steps) $ \(i, (r, m)) → do
                (seed, i, implAssignments r) `shouldBe` (seed, i, refAssignments m)
                (seed, i, registryInvariantViolations r) `shouldBe` (seed, i, [])

    it "rebuilding from the forward assignments reproduces the registry" $
        forM_ runs $ \(seed, (_, steps)) →
            forM_ steps $ \(r, _) →
                (seed, fromAssignments (assignments r)) `shouldBe` (seed, r)

    it "nested queries agree with reference set algebra in every result mode" $
        forM_ runs $ \(seed, (_, steps)) → do
            let (r, m) = lastStep steps
                qs     = fst (listOf 12 (\g → let (u, g1) = genUniverse g
                                                  (q, g2) = genQuery 3 g1
                                              in ((u, q), g2))
                                 (Random.mkStdGen (seed + 100000)))
            forM_ qs $ \(u, q) → do
                let universe = targetSetsFromList u
                    expected = refQuery m u q
                    got      = queryList r universe q
                (seed, q, got) `shouldBe` (seed, q, expected)
                queryCount r universe q `shouldBe` length got
                queryExists r universe q `shouldBe` not (null got)
                -- Querying is not a mutation.
                evaluateQuery r universe q `seq` r `shouldBe` r

  where
    lastStep [] = (emptyTagRegistry, [])
    lastStep xs = last xs

-- * Mutation contracts

mutationSpec ∷ Spec
mutationSpec = describe "mutations" $ do
    it "add, list and membership; a repeated add is idempotent" $ do
        let r1 = addTags (unit 1) (tags ["a", "b"]) emptyTagRegistry
            r2 = addTags (unit 1) (tags ["b", "a"]) r1
        r2 `shouldBe` r1
        tagsOf (unit 1) r1 `shouldBe` tags ["a", "b"]
        hasTag (unit 1) (tag "a") r1 `shouldBe` True
        hasTag (unit 1) (tag "c") r1 `shouldBe` False
        tagsOf (unit 2) r1 `shouldBe` Set.empty

    it "removing the last tag removes the entry from both indexes" $ do
        let r = removeTag (unit 1) (tag "a")
                  (addTag (unit 1) (tag "a") emptyTagRegistry)
        nullTagRegistry r `shouldBe` True
        all ((≡ RegistryCounts 0 0 0) . snd) (registryCounts r) `shouldBe` True

    it "removing an absent tag or clearing an untagged target changes nothing" $ do
        let r = addTag (unit 1) (tag "a") emptyTagRegistry
        removeTag (unit 1) (tag "zz") r `shouldBe` r
        clearTags (unit 2) r `shouldBe` r

    it "empty replacement and clear drop only that target; sharers keep the tag" $ do
        let r = addTag (unit 2) (tag "a")
                  (addTags (unit 1) (tags ["a", "b"]) emptyTagRegistry)
            expected = addTag (unit 2) (tag "a") emptyTagRegistry
        replaceTags (unit 1) Set.empty r `shouldBe` expected
        clearTags (unit 1) r `shouldBe` expected
        registryInvariantViolations (clearTags (unit 1) r) `shouldBe` []

    it "replace sets the complete tag set" $ do
        let r = replaceTags (building 4) (tags ["c"])
                  (addTags (building 4) (tags ["a", "b"]) emptyTagRegistry)
        tagsOf (building 4) r `shouldBe` tags ["c"]
        assignments r `shouldBe` [(building 4, tags ["c"])]

    it "fromAssignments unions a repeated target and drops empty sets" $ do
        let r = fromAssignments
                  [ (unit 1, tags ["a"]), (unit 1, tags ["b"])
                  , (item 2, Set.empty), (tile pageA 0 0 0, tags ["c"]) ]
        assignments r `shouldBe`
            [ (unit 1, tags ["a", "b"]), (tile pageA 0 0 0, tags ["c"]) ]
        registryInvariantViolations r `shouldBe` []

    it "every category keeps its own indexes" $ do
        let r = foldl' (\acc x → addTag x (tag "shared") acc) emptyTagRegistry
                  [ unit 1, building 1, location pageA 1, plant pageA 1
                  , item 1, tile pageA 0 0 0 ]
        (snd <$> registryCounts r) `shouldBe` replicate 6 (RegistryCounts 1 1 1)
        (fst <$> registryCounts r) `shouldBe` allTagCategories

-- * Identities

identitySpec ∷ Spec
identitySpec = describe "target identities" $ do
    it "unit 12 and building 12 are distinct targets" $ do
        let r = addTag (building 12) (tag "b")
                  (addTag (unit 12) (tag "u") emptyTagRegistry)
        tagsOf (unit 12) r `shouldBe` tags ["u"]
        tagsOf (building 12) r `shouldBe` tags ["b"]
        length (assignments r) `shouldBe` 2

    it "page-scoped ids on two pages are distinct targets" $ do
        let r = addTag (plant pageB 1) (tag "b")
                  (addTag (plant pageA 1) (tag "a")
                     (addTag (location pageA 1) (tag "a") emptyTagRegistry))
        tagsOf (plant pageA 1) r `shouldBe` tags ["a"]
        tagsOf (plant pageB 1) r `shouldBe` tags ["b"]
        tagsOf (location pageB 1) r `shouldBe` Set.empty

    it "refuses the non-identities: item 0, the reserved flora id, the empty tag" $ do
        isNothing (mkItemTarget 0) `shouldBe` True
        isJust (mkItemTarget 1) `shouldBe` True
        isNothing (mkPlantTarget pageA floraInstanceIdNone) `shouldBe` True
        isNothing (mkGameplayTag "") `shouldBe` True

    it "two seam aliases of one tile are one target and one registry entry" $ do
        -- On an 8-chunk world, shifting a tile by four chunks along +x
        -- and -y moves u by the full width: the same physical tile.
        let shift = 4 * chunkSize
            canonical = tile pageA 3 2 0
            alias     = tile pageA (3 + shift) (2 - shift) 0
            r = addTag alias (tag "b") (addTag canonical (tag "a") emptyTagRegistry)
        alias `shouldBe` canonical
        assignments r `shouldBe` [(canonical, tags ["a", "b"])]
        -- With no wrapping (an arena) the same coordinates stay apart.
        let arena x y = TargetTile (mkTileTarget id pageA x y 0)
        arena (3 + shift) (2 - shift) `shouldNotBe` arena 3 2

-- * Query contracts

querySpec ∷ Spec
querySpec = describe "queries" $ do
    let a = unit 1; b = unit 2; c = unit 3
        defenders = addTag b (tag "reserve")
                      (addTag a (tag "defender") emptyTagRegistry)
        units = unitUniverse (Set.fromList [UnitId 1, UnitId 2, UnitId 3])

    it "units minus defenders includes the untagged unit" $ do
        queryList defenders units (QDifference QUniverse (tagged (tag "defender")))
            `shouldBe` [b, c]
        queryList defenders units (QComplement (tagged (tag "defender")))
            `shouldBe` [b, c]

    it "an empty result is an empty list, zero and false" $ do
        let q = tagged (tag "missing")
        queryList defenders units q `shouldBe` []
        queryCount defenders units q `shouldBe` 0
        queryExists defenders units q `shouldBe` False

    it "an empty universe matches nothing, even a complement" $ do
        let q = QComplement (tagged (tag "defender"))
        queryList defenders emptyTargetSets q `shouldBe` []
        queryCount defenders emptyTargetSets q `shouldBe` 0
        queryExists defenders emptyTargetSets q `shouldBe` False

    it "an entirely untagged universe: positive tags match nothing, exclusions match all" $ do
        let fresh = unitUniverse (Set.fromList (UnitId <$> [10 .. 20]))
        queryList defenders fresh (tagged (tag "defender")) `shouldBe` []
        queryCount defenders fresh (QMatch (tagFilter [] [] [tag "defender"]))
            `shouldBe` 11

    it "all/any/none: empty groups impose nothing, missing tags are empty sets" $ do
        let r = foldl' (\acc (x, ts) → addTags x (tags ts) acc) emptyTagRegistry
                  [ (a, ["x", "y"]), (b, ["y"]), (c, ["z"]) ]
            run f = queryList r units (QMatch f)
        run (tagFilter [] [] []) `shouldBe` [a, b, c]
        run (tagFilter [tag "y"] [] []) `shouldBe` [a, b]
        run (tagFilter [tag "y", tag "y"] [] []) `shouldBe` [a, b]
        run (tagFilter [tag "y", tag "missing"] [] []) `shouldBe` []
        run (tagFilter [] [tag "x", tag "z"] []) `shouldBe` [a, c]
        run (tagFilter [] [tag "missing"] []) `shouldBe` []
        run (tagFilter [] [] [tag "missing"]) `shouldBe` [a, b, c]
        run (tagFilter [tag "y"] [tag "x", tag "z"] [tag "z"]) `shouldBe` [a]
        run (tagFilter [] [tag "y", tag "z"] [tag "x"]) `shouldBe` [b, c]

    it "a category-restricted universe never leaks another category" $ do
        let tk = mkTileTarget (wrapChunkCoordU 8) pageA 0 0 0
            t = TargetTile tk
            r = addTag t (tag "s") (addTag a (tag "s") emptyTagRegistry)
            tiles = tileUniverse (Set.fromList [tk])
        queryList r tiles (tagged (tag "s")) `shouldBe` [t]
        queryList r tiles (QComplement (tagged (tag "zz"))) `shouldBe` [t]
        queryList r (units <> tiles) (tagged (tag "s")) `shouldBe` [a, t]
        queryList r (selectCategories [TileCategory] (units <> tiles))
                  (tagged (tag "s")) `shouldBe` [t]

    it "mixed-category results come back in category order, then key order" $ do
        let xs = [ tile pageA 0 0 0, item 1, unit 2, building 1, plant pageA 1
                 , location pageA 1, unit 1 ]
            r = foldl' (\acc x → addTag x (tag "m") acc) emptyTagRegistry xs
            got = queryList r (targetSetsFromList xs) (tagged (tag "m"))
        got `shouldBe` sort xs
        (targetCategory <$> got) `shouldBe` sort (UnitCategory : allTagCategories)

    it "a changing loaded subset hides and restores tiles without touching assignments" $ do
        let t1 = tile pageA 0 0 0; t2 = tile pageA 1 0 0; t3 = tile pageA 2 0 0
            keyOf = \case TargetTile k → k; _ → error "not a tile"
            loaded ts = tileUniverse (Set.fromList (keyOf <$> ts))
            r = foldl' (\acc x → addTag x (tag "meeting_point") acc)
                       emptyTagRegistry [t1, t2, t3]
            q = tagged (tag "meeting_point")
        queryList r (loaded [t1, t2, t3]) q `shouldBe` [t1, t2, t3]
        queryList r (loaded [t1, t2]) q `shouldBe` [t1, t2]
        queryCount r (loaded [t2]) q `shouldBe` 1
        queryList r (loaded []) q `shouldBe` []
        -- Unloading is not deletion: the assignment is still there, and
        -- the tile comes back the moment it is loaded again.
        tagsOf t3 r `shouldBe` tags ["meeting_point"]
        queryList r (loaded [t1, t3]) q `shouldBe` [t1, t3]

    it "a broad query over a large untagged universe adds no registry entries" $ do
        let big = unitUniverse (Set.fromList (UnitId <$> [1 .. 20000]))
            countsBefore = registryCounts defenders
            q = QComplement (tagged (tag "defender"))
        queryCount defenders big q `shouldBe` 19999
        length (queryList defenders big q) `shouldBe` 19999
        queryExists defenders big q `shouldBe` True
        registryCounts defenders `shouldBe` countsBefore
