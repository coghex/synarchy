{-# LANGUAGE OverloadedStrings #-}
-- | The structure-ghost suite's fixture (#1846): the SHIPPED structure
--   packs, read out of their real YAML.
--
--   The acceptance criterion is that a ghost draws "the art, facemap and
--   slot geometry the placer would use — verified against a real pack
--   YAML", and a hand-written registration cannot verify that: it would
--   only prove the ghost agrees with the spec's own invention. So this
--   decodes @data\/structure_packs\/dungeon_1.yaml@ and @wire.yaml@ —
--   the very files @scripts\/structures.lua@ and @scripts\/wire.lua@
--   read at boot — and builds BOTH registrations from them:
--
--     * #1842's art catalogue, the ghost's source of truth, through the
--       production 'registerPackArt' with its all-or-nothing rule.
--     * #1712's wall catalogue, so a ghost wall rotates exactly as a
--       placed one does.
--
--   Handles are synthesized, because a headless run loads no textures —
--   but they are synthesized from the PATH, deterministically and
--   distinctly, which is what lets an example say "this quad carries the
--   floor's texture and not the ceiling's" at all. The shipped fixture
--   in "Test.Headless.Construct.Fixture" deliberately does not: it
--   registers one handle for every slot, which is fine for a resolver
--   spec and useless for a rendering one.
module Test.Headless.Render.StructureGhostFixture
    ( PackFixture(..)
    , loadShippedPacks
    , handleForPath
    , packCatalog
    , packWallCatalog
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Aeson ((.:), (.:?), Value, withObject)
import Data.Aeson.Key (Key, fromText)
import Data.Aeson.Types (Parser, parseEither)
import Data.Char (ord)
import Engine.Asset.Handle (TextureHandle(..))
import Structure.ArtCatalog
import Structure.Facing (WallCaps(..), WallEdge(..), wallCapsCode)
import Structure.Wire (allWireShapes, wireShapeName)
import Structure.WallCatalog
    ( StructureWallCatalog, WallArtEntry(..), emptyStructureWallCatalog
    , registerWallFamily )

-- | One shipped pack, decoded: its #1842 registration and — for a pack
--   that carries walls — the #1712 family the same YAML declares.
data PackFixture = PackFixture
    { pfName         ∷ !Text
    , pfRegistration ∷ !PackArtRegistration
    , pfWallEntries  ∷ ![WallArtEntry]
    } deriving (Show, Eq)

-- | A stable, path-derived handle. Distinct per path (an FNV-1a over the
--   path, forced positive and nonzero so 'registerPackArt' accepts it),
--   so an example can identify WHICH asset a quad is carrying.
handleForPath ∷ Text → TextureHandle
handleForPath path = TextureHandle (fromIntegral (1 + (h `mod` 1000000)))
  where
    h = T.foldl' step (2166136261 ∷ Word32) path
    step acc c = (acc `xor` fromIntegral (ord c)) * 16777619

-- | Both shipped packs, decoded from their real YAML.
loadShippedPacks ∷ IO [PackFixture]
loadShippedPacks = sequence
    [ decodePack "data/structure_packs/dungeon_1.yaml" parsePiecePack
    , decodePack "data/structure_packs/wire.yaml"      parseWirePack
    ]

decodePack ∷ FilePath → (Value → Parser PackFixture) → IO PackFixture
decodePack path parser = do
    raw ← BS.readFile path
    case Y.decodeEither' raw of
        Left err → fail (path ⧺ ": " ⧺ show err)
        Right v  → case parseEither parser v of
            Left err → fail (path ⧺ ": " ⧺ err)
            Right pf → pure pf

-- | Every decoded pack, registered. 'registerPackArt' is the production
--   entry point and refuses an incomplete pack, so a fixture that
--   silently lost a wall cap fails here rather than in an example.
packCatalog ∷ [PackFixture] → StructureArtCatalog
packCatalog = foldl' step emptyStructureArtCatalog
  where
    step cat pf = case registerPackArt (pfRegistration pf) cat of
        (cat', ArtRegistered)        → cat'
        (_,    ArtAlreadyRegistered) → cat
        (_,    ArtRegistrationRefused f) →
            error ("fixture pack refused: " ⧺ T.unpack (artFaultMessage f))

-- | The wall families the same YAML declares, for #1712's rotation.
packWallCatalog ∷ [PackFixture] → StructureWallCatalog
packWallCatalog = foldl' step emptyStructureWallCatalog
  where
    step cat pf
        | null (pfWallEntries pf) = cat
        | otherwise = fromMaybe
            (error ("fixture wall family refused: " ⧺ T.unpack (pfName pf)))
            (registerWallFamily (pfWallEntries pf) cat)

-- * Decoders
--
--   Deliberately STRICT about the shape: a pack whose YAML stops
--   matching this must fail the suite loudly rather than quietly
--   register fewer kinds and let the ghost examples pass over the ones
--   that survived.

parsePiecePack ∷ Value → Parser PackFixture
parsePiecePack = withObject "structure pack" $ \o → do
    name   ← o .: "name"
    pieces ← o .: "pieces"
    walls  ← o .: "walls"
    build  ← o .: "build"
    simple ← forM [(KFloor, "floor"), (KCeiling, "ceiling"), (KPost, "post")] $
        \(kind, key) → do
            entry ← pieces .: key
            (,) kind <$> parseArt entry
    wallArt ← forM allWallEdges $ \edge → do
        entry ← walls .: edgeKey edge
        tex   ← entry .: "texture"
        faces ← entry .: "facemaps"
        caps  ← forM allCaps $ \c →
            (,) c <$> (faces .: fromText (wallCapsCode c))
        pure (edge, tex ∷ Text, caps)
    costs ← parseBuild build
    let artFor kind = [ a | (k, a) ← simple, k ≡ kind ]
        wallEntries =
            [ (AkWall e c, asset tex face)
            | (e, tex, caps) ← wallArt, (c, face) ← caps ]
    pure PackFixture
        { pfName = name
        , pfRegistration = PackArtRegistration
            { parPack = name
            , parKinds = [ (k, isJust (M.lookup k costs), M.lookup k costs)
                         | k ← [KFloor, KCeiling, KPost, KWall] ]
            , parEntries =
                [ (key, a)
                | (kind, key) ← [(KFloor, AkFloor), (KCeiling, AkCeiling)
                                , (KPost, AkPost)]
                , a ← artFor kind ]
                ⧺ wallEntries
            }
        , pfWallEntries =
            [ WallArtEntry e Nothing tex (handleForPath tex) True
            | (e, tex, _) ← wallArt ]
            ⧺ [ WallArtEntry e (Just c) face (handleForPath face) True
              | (e, _, caps) ← wallArt, (c, face) ← caps ]
        }

parseWirePack ∷ Value → Parser PackFixture
parseWirePack = withObject "wire pack" $ \o → do
    name  ← o .: "name"
    face  ← o .: "facemap"
    conns ← o .: "connections"
    build ← o .: "build"
    costs ← parseBuild build
    entries ← forM allWireShapes $ \s → do
        tex ← conns .: fromText (wireShapeName s)
        pure (AkWire s, asset tex face)
    pure PackFixture
        { pfName = name
        , pfRegistration = PackArtRegistration
            { parPack    = name
            , parKinds   = [ (KWire, isJust (M.lookup KWire costs)
                            , M.lookup KWire costs) ]
            , parEntries = entries
            }
        , pfWallEntries = []
        }

parseArt ∷ Value → Parser PieceArt
parseArt = withObject "piece art" $ \o →
    asset <$> o .: "texture" <*> o .: "facemap"

asset ∷ Text → Text → PieceArt
asset tex face = PieceArt
    { paTexture = ArtAsset tex  (handleForPath tex)
    , paFacemap = ArtAsset face (handleForPath face)
    }

parseBuild ∷ Value → Parser (M.Map PieceKind BuildCost)
parseBuild = withObject "build block" $ \o →
    fmap (M.fromList ∘ catMaybes) $ forM allPieceKinds $ \k → do
        mEntry ← o .:? fromText (pieceKindName k)
        forM mEntry $ \e → flip (withObject "build entry") e $ \b → do
            work ← b .: "build_work"
            mats ← b .: "materials"
            pure (k, mkBuildCost work (M.toList (mats ∷ M.Map Text Int)))

allWallEdges ∷ [WallEdge]
allWallEdges = [WallNE, WallNW, WallSE, WallSW]

allCaps ∷ [WallCaps]
allCaps = [ WallCaps l r | l ← [False, True], r ← [False, True] ]

edgeKey ∷ WallEdge → Key
edgeKey e = fromText $ case e of
    WallNE → "ne"; WallNW → "nw"; WallSE → "se"; WallSW → "sw"
