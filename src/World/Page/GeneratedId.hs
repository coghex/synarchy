{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}

-- | The opaque, persistent identity of a GENERATED WORLD (issue #2021,
--   world-map epic #2017, design slice WML-3).
--
--   A 'GeneratedWorldId' answers exactly one question: /which generated
--   foundation did this session descend from?/ It is assigned once, when
--   a world page is created, and travels with that page through saves,
--   loads and renames. Later slices key durable per-world artifacts
--   (WML-4's shared library, WML-7\/WML-8's map artifacts) to it, which
--   is why it must be stable, unique, and meaningless.
--
--   OPAQUE means nothing derives meaning from its contents: it is never
--   parsed, never rendered to a player, and never built from the display
--   name, the seed, the page id, a save-slot name or a filesystem path.
--   The constructor is deliberately NOT exported — the only ways to
--   obtain one are 'newGeneratedWorldId' (a fresh allocation) and
--   decoding a saved one, so no consumer can reconstruct an id from
--   world content even by accident. 'renderGeneratedWorldId' exists for
--   diagnostics and for later slices that need a filesystem-safe token;
--   it is a PRESENTATION of the id, never a definition of one, and
--   there is deliberately no parser back.
--
--   The 'Ord' instance exists so collections of ids can be written in a
--   canonical order and compared as sets (the @"metadata"@ component
--   carries one id per page and must encode deterministically). That is
--   byte-determinism, not semantics: an id that sorts earlier means
--   nothing about the world it names, and nothing may branch on the
--   comparison beyond ordering and equality.
--
--   DISTINCTNESS is the whole contract, and it spans processes and
--   restarts — two engines launched a second apart, or the same engine
--   run twice, must never mint the same id (issue #2021 requirement 3:
--   independently creating a world with the same seed and parameters
--   yields a DIFFERENT id; there is no deduplication by content and no
--   digest of 'World.Generate.Types.WorldGenParams' is frozen for that
--   purpose). That rules out every RNG this game already has:
--
--     * worldgen's streams are seeded from the world seed on purpose, so
--       equal inputs would produce equal ids — exactly the content
--       dedup requirement 3 rejects;
--     * gameplay's @math.random@ stream (CLAUDE.md, #1330) and the
--       ambient 'System.Random' global generator are deterministic
--       simulation state, and drawing from either would both perturb
--       unrelated simulation and inherit its weaknesses;
--     * @splitmix@'s own @initSMGen@ — what @random@'s @initStdGen@
--       calls — seeds from @truncate getPOSIXTime@ (WHOLE SECONDS) xor a
--       CPU-time quotient, which is precisely the "two engines launched
--       in the same second collide" failure #1330 already documents for
--       the Lua side.
--
--   So allocation reads real operating-system entropy — @\/dev\/urandom@
--   — and mixes it,
--   through SHA-256, with four independent disambiguators — the process
--   id, wall-clock nanoseconds, the monotonic clock, and a process-local
--   allocation counter. The entropy carries the strength; the other four
--   guarantee that two ids minted back to back in one process differ
--   even in the degenerate case where the entropy source is unreadable
--   and contributes nothing.
--
--   That makes this module POSIX-only, which is this project's existing
--   platform stance rather than a new constraint: it is tested on macOS
--   and runs on Linux, and "World.Save.Storage" already reaches for
--   @System.Posix@ directly for the save path's own fsync.
module World.Page.GeneratedId
    ( GeneratedWorldId
    , newGeneratedWorldId
    , renderGeneratedWorldId
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Control.Exception (SomeException, handle)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Serialize (Serialize)
import Data.Time.Clock.System (SystemTime(..), getSystemTime)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Numeric (showHex)
import System.IO
    (IOMode(..), hSetBinaryMode, withFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessID)

-- | 128 opaque bits, carried as two 'Word64's so the wire encoding is a
--   fixed 16 bytes with no length prefix, no text validation, and no
--   invalid representation to reject on decode.
--
--   Reused AS-IS by the frozen save DTOs rather than mirrored by a
--   component-owned copy, under the frozen-DTO boundary rule's explicit
--   carve-out for durable opaque id references
--   ("World.Save.Component.Types"; the same treatment 'MaterialId',
--   'Location.Instance.LocationInstanceId' and #1854's
--   'FloraInstanceId' already get). It has no internal structure that a
--   later change could reshape: it is 128 bits, permanently.
data GeneratedWorldId = GeneratedWorldId !Word64 !Word64
    deriving (Eq, Ord, Generic, Serialize)

-- | Written by hand so diagnostics print the readable hex token rather
--   than two decimal words — 'World.Save.Component.metadataErrors' and
--   the world-pages validator both report ids through 'tshow', and a
--   pair of 20-digit numbers is unreadable in a save-rejection message.
--   The rendered form still goes through 'showsPrec' on the 'Text', so
--   it is quoted and unambiguous.
instance Show GeneratedWorldId where
    showsPrec d g = showParen (d > 10) $
        showString "GeneratedWorldId " . showsPrec 11 (renderGeneratedWorldId g)

-- | The id's 32-character lowercase hexadecimal presentation, stable for
--   the life of an id. Deliberately one-way: there is no parser back,
--   because an id is only ever obtained by allocating one or decoding a
--   saved one, and a round trip through text would make the token look
--   like a definition of the id rather than a rendering of it.
renderGeneratedWorldId ∷ GeneratedWorldId → Text
renderGeneratedWorldId (GeneratedWorldId hi lo) = hex16 hi <> hex16 lo
  where
    hex16 w = let s = T.pack (showHex w "")
              in T.replicate (16 - T.length s) "0" <> s

-- | Mint a fresh id. Every call returns a distinct value, including
--   across engine restarts and between independently running processes
--   — see this module's header for why no existing generator can do
--   that.
newGeneratedWorldId ∷ IO GeneratedWorldId
newGeneratedWorldId = do
    entropy ← readSystemEntropy entropyBytes
    pid     ← getProcessID
    MkSystemTime wallSecs wallNanos ← getSystemTime
    monotonic ← getMonotonicTimeNSec
    -- Post-increment: the value mixed in is the one this call claimed,
    -- and no two calls in this process ever claim the same one.
    counter ← atomicModifyIORef' allocationCounter (\n → (n + 1, n))
    let block = BS.concat
            [ entropy
            , S.encode (fromIntegral pid        ∷ Word64)
            , S.encode (fromIntegral wallSecs   ∷ Word64)
            , S.encode (fromIntegral wallNanos  ∷ Word64)
            , S.encode monotonic
            , S.encode counter ]
    pure (idFromDigest (SHA256.hash block))

-- | How many bytes of operating-system entropy each allocation consumes.
--   Comfortably more than the 128 bits an id carries, so the digest is
--   never the narrow point.
entropyBytes ∷ Int
entropyBytes = 32

-- | Best-effort operating-system entropy. Returns fewer bytes (in
--   practice none) if @\/dev\/urandom@ cannot be read at all, which is
--   why 'newGeneratedWorldId' mixes in the clock, the process id and the
--   allocation counter as well: the digest degrades to
--   "unique within this process and this nanosecond" rather than to a
--   constant. Never throws — an entropy source that is missing must not
--   be able to fail world creation.
readSystemEntropy ∷ Int → IO BS.ByteString
readSystemEntropy n =
    handle (\(_ ∷ SomeException) → pure BS.empty) $
        withFile "/dev/urandom" ReadMode $ \h → do
            hSetBinaryMode h True
            -- A short read is a partially-trusted source; keep whatever
            -- it gave rather than discarding it, since the digest mixes
            -- it with the disambiguators either way.
            BS.hGet h n

-- | The first 128 bits of a SHA-256 digest, big-endian, as the id's two
--   words. Truncating a cryptographic digest is the standard way to
--   narrow one; the discarded half carries no information the kept half
--   depends on.
idFromDigest ∷ BS.ByteString → GeneratedWorldId
idFromDigest digest =
    let (hiBytes, rest) = BS.splitAt 8 (BS.take 16 digest)
    in GeneratedWorldId (beWord64 hiBytes) (beWord64 (BS.take 8 rest))
  where
    beWord64 = BS.foldl' (\acc b → (acc `shiftL` 8) ⌄ fromIntegral b) 0

-- | A process-local, monotonically increasing allocation counter, never
--   reset and never seeded from anything. It is NOT the id's uniqueness
--   guarantee — it cannot be, since a fresh process starts it at zero
--   again, which is exactly the "resettable process-local counter" a
--   persistent library key must not rely on. Its one job is to separate
--   two allocations that would otherwise share every other input.
--
--   Module-level mutable state under 'unsafePerformIO'\/@NOINLINE@,
--   matching 'World.Chunk.Residency''s own generation counter: it has no
--   owner thread, no lifecycle, and nothing may read it, so putting it
--   on 'World.State.Types.WorldState' or 'Engine.Core.State.EngineEnv'
--   would be inventing a persistence question where there is none.
allocationCounter ∷ IORef Word64
allocationCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE allocationCounter #-}
