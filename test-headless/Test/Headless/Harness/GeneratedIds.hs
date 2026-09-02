{-# LANGUAGE Strict #-}

-- | Deterministic 'GeneratedWorldId's for save-fixture use (#2021).
--
--   Save fixtures across this suite are top-level PURE values —
--   @minimalPage@, @richPage@ and friends — and several of them are
--   pinned: one test fingerprints every component's exact encoded bytes,
--   another compares a decode against a byte fixture tracked in git. So
--   a fixture id has to be the same on every run and in every process,
--   which the production allocator deliberately is not
--   ('World.Page.GeneratedId.newGeneratedWorldId' reads real entropy,
--   because ids must never repeat).
--
--   These helpers therefore take the OTHER production route into the
--   type: decoding. "World.Page.GeneratedId" exports no constructor —
--   the only ways to obtain an id are to mint one or to decode a stored
--   one — and a stored id is exactly 16 bytes, so a fixture chooses 16
--   bytes and decodes them, precisely as loading a save does. Nothing
--   here widens the production surface.
--
--   The bytes are derived from a caller-supplied name purely so distinct
--   fixtures get distinct, reproducible ids. That derivation is a
--   FIXTURE convenience and says nothing about the contract: production
--   ids are never derived from a page id, a seed or a name, and the
--   tests that prove it ("generated world identity") use real
--   engine-minted ids rather than anything from this module.
module Test.Headless.Harness.GeneratedIds
    ( fixtureGeneratedWorldId
    , fixtureGeneratedWorldIdForPage
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as TE
import World.Page.GeneratedId (GeneratedWorldId)
import World.Page.Types (WorldPageId(..))

-- | A distinct, reproducible id for a fixture named by @label@.
fixtureGeneratedWorldId ∷ Text → GeneratedWorldId
fixtureGeneratedWorldId label =
    case S.decode (fixtureIdBytes label) of
        Right gid → gid
        Left err  → error ("fixtureGeneratedWorldId: a 16-byte payload \
                           \failed to decode as a GeneratedWorldId: " ⧺ err)

-- | One id per page id, so a multi-page fixture never gives two pages
--   the same generated foundation — which the save components reject as
--   corruption, correctly, since no engine path can produce it.
fixtureGeneratedWorldIdForPage ∷ WorldPageId → GeneratedWorldId
fixtureGeneratedWorldIdForPage (WorldPageId name) =
    fixtureGeneratedWorldId ("page:" <> name)

-- | The 16 bytes a fixture id decodes from: two 64-bit FNV-1a hashes of
--   the label under different offset bases, big-endian, which is the
--   layout 'GeneratedWorldId''s two words serialize in. Two labels
--   colliding would silently merge two fixtures' worlds, so the two
--   halves use independent bases rather than one hash split in two.
fixtureIdBytes ∷ Text → BS.ByteString
fixtureIdBytes label =
    S.encode (mixed 0xcbf29ce484222325) <> S.encode (mixed 0x9e3779b97f4a7c15)
  where
    bytes    = TE.encodeUtf8 label
    mixed b  = finalize (fnv1a b bytes)

fnv1a ∷ Word64 → BS.ByteString → Word64
fnv1a = BS.foldl' (\acc b → (acc `xor` fromIntegral b) * 0x100000001b3)

-- | splitmix's finalizer. FNV-1a alone leaves neighbouring labels with
--   visibly similar high bytes, which would make two fixture ids LOOK
--   like variants of one another in a failure diff even though they are
--   distinct; this avalanches them apart.
finalize ∷ Word64 → Word64
finalize w0 =
    let w1 = (w0 `xor` (w0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        w2 = (w1 `xor` (w1 `shiftR` 27)) * 0x94d049bb133111eb
    in w2 `xor` (w2 `shiftR` 31)
