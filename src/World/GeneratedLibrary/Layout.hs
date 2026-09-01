{-# LANGUAGE Strict #-}
-- | The generated-world library's on-disk layout (issue #2024): what the
--   library root may contain, how a 'GeneratedWorldId' maps to a
--   directory name and back, which payload names are acceptable, and
--   the byte format of the two record files the library writes. Pure —
--   nothing here touches the filesystem — so every rule is testable
--   without a scratch directory and reusable by every module that does.
--
--   === Names the library owns
--
--   Under the root, exactly these shapes belong to the library:
--
--   > <token>                        a FINAL entry directory
--   > <token>.staging-<suffix>       a candidate being published
--   > <token>.displaced-<suffix>     the previous complete entry of a
--   >                                republish, kept until the new one
--   >                                and the registry are durable
--   > <token>.tombstone-<suffix>     a proven-unreferenced entry detached
--   >                                by cleanup, awaiting deletion
--   > registry.synlib                the registry (an index, never the
--   >                                authority)
--   > library.lock                   the cross-process lock file
--
--   where @\<token\>@ is exactly the 32 lowercase hexadecimal characters
--   'World.Page.GeneratedId.renderGeneratedWorldId' produces and
--   @\<suffix\>@ is digits and hyphens, digit-first. Anything else is
--   'UnfamiliarName' and is never touched: not read, not indexed, not
--   removed (spec: "only reserved names carrying library ownership
--   evidence may be swept; unfamiliar directories must be retained").
--
--   === Identity to path is one-way and canonical
--
--   'entryDirectoryName' is the ONLY way an id becomes a path component,
--   and it goes through the id's own rendering — never through text a
--   caller, a registry row or a directory listing supplied. There is
--   deliberately no parser from a token back to an id ("World.Page.GeneratedId"
--   exports none): a directory is matched to an id by decoding the
--   'EntryRecord' INSIDE it and checking that the record's id renders to
--   the directory's own name ("World.GeneratedLibrary.Entry"). So a
--   malformed, non-canonical, separator-containing or traversal-shaped
--   name can only ever classify as 'UnfamiliarName'; it never reaches a
--   path join.
module World.GeneratedLibrary.Layout
    ( -- * Reserved file names
      entryRecordFileName
    , registryFileName
    , lockFileName
      -- * Root-level name grammar
    , LibraryName(..)
    , TransientKind(..)
    , renderTransientKind
    , classifyLibraryName
    , isEntryToken
    , entryDirectoryName
    , transientDirectoryName
      -- * Payload policy
    , validatePayloadName
    , validatePayload
    , payloadDescriptor
    , inventoryDigest
      -- * Record files
    , encodeEntryRecord
    , decodeEntryRecord
    , encodeRegistry
    , decodeRegistry
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Char (isControl, isDigit, isHexDigit, isLower)
import World.Page.GeneratedId (GeneratedWorldId, renderGeneratedWorldId)
import World.GeneratedLibrary.Types

-- Reserved file names -----------------------------------------------------

-- | The committed-entry record inside every entry directory. Written
--   LAST during publication, so its presence and validity is what makes
--   a directory an entry.
entryRecordFileName ∷ FilePath
entryRecordFileName = "entry.record"

registryFileName ∷ FilePath
registryFileName = "registry.synlib"

lockFileName ∷ FilePath
lockFileName = "library.lock"

-- Root-level name grammar --------------------------------------------------

-- | The three transient directory kinds. A transient is always named
--   after the id it concerns so an interrupted operation is
--   attributable: reconciliation can tell WHICH entry a displaced copy
--   belongs to without opening it.
data TransientKind = StagingDir | DisplacedDir | TombstoneDir
    deriving (Show, Eq, Ord, Enum, Bounded)

renderTransientKind ∷ TransientKind → String
renderTransientKind StagingDir   = "staging"
renderTransientKind DisplacedDir = "displaced"
renderTransientKind TombstoneDir = "tombstone"

-- | What one name in the library root means. The token carried by the
--   first four constructors is the directory's OWN name (or the id
--   portion of a transient's), already proven to have the canonical
--   shape — it is still only text, and is never turned into an id.
data LibraryName
    = FinalEntryName !Text
    | TransientName !TransientKind !Text
    | RegistryName
    | LockName
    | UnfamiliarName
    deriving (Show, Eq)

-- | Exactly 32 lowercase hexadecimal characters — the shape
--   'renderGeneratedWorldId' always produces. Uppercase is rejected on
--   purpose: it is not what the library writes, so it is not the
--   library's.
isEntryToken ∷ String → Bool
isEntryToken s = length s ≡ 32 ∧ all hexLower s
  where hexLower c = isHexDigit c ∧ (isDigit c ∨ isLower c)

-- | Classify one root-level name. Total; never fails.
classifyLibraryName ∷ FilePath → LibraryName
classifyLibraryName name
    | name ≡ registryFileName = RegistryName
    | name ≡ lockFileName     = LockName
    | isEntryToken name       = FinalEntryName (T.pack name)
    | otherwise = case L.break (≡ '.') name of
        (tok, '.' : rest) | isEntryToken tok →
            case transientKindOf rest of
                Just kind → TransientName kind (T.pack tok)
                Nothing   → UnfamiliarName
        _ → UnfamiliarName
  where
    transientKindOf rest =
        listToMaybe [ kind | kind ← [minBound .. maxBound]
                           , Just suffix ← [L.stripPrefix (renderTransientKind kind <> "-") rest]
                           , validSuffix suffix ]
    validSuffix s = case s of
        (c : _) → isDigit c ∧ all (\x → isDigit x ∨ x ≡ '-') s
        []      → False

-- | The one and only id-to-directory-name mapping.
entryDirectoryName ∷ GeneratedWorldId → FilePath
entryDirectoryName = T.unpack . renderGeneratedWorldId

-- | The name of a transient directory for @gid@, distinguished by two
--   numbers the creator supplies (the process id and a process-local
--   counter, in practice). Round-trips through 'classifyLibraryName'.
transientDirectoryName ∷ TransientKind → GeneratedWorldId → Word64 → Word64 → FilePath
transientDirectoryName kind gid a b =
    entryDirectoryName gid <> "." <> renderTransientKind kind <> "-"
        <> show a <> "-" <> show b

-- Payload policy -----------------------------------------------------------

-- | Is @name@ acceptable as a payload file's name? One safe path
--   component: non-empty, bounded, no separators, no traversal, not
--   hidden, no control characters, and not a name the library reserves
--   for itself inside or beside an entry.
validatePayloadName ∷ Text → Either Text ()
validatePayloadName name
    | T.null name             = Left "payload name cannot be empty"
    | T.length name > 128     = Left ("payload name too long (max 128): " <> name)
    | T.any isSep name        = Left ("payload name cannot contain '/' or '\\': " <> name)
    | name ≡ "." ∨ name ≡ ".." = Left ("payload name cannot be '.' or '..': " <> name)
    | T.head name ≡ '.'       = Left ("payload name cannot start with '.': " <> name)
    | T.any isControl name    = Left ("payload name cannot contain control characters: " <> name)
    | T.unpack name `elem` reserved
                              = Left ("payload name is reserved by the library: " <> name)
    | otherwise               = Right ()
  where
    isSep c = c ≡ '/' ∨ c ≡ '\\'
    reserved = [entryRecordFileName, registryFileName, lockFileName]

-- | Validate a whole payload and return its descriptors in canonical
--   (ascending-name) order. Rejects an empty payload — an entry must
--   HAVE content for the library to know it has content — and rejects
--   two names that differ only by case, because the tree is tested on a
--   case-insensitive filesystem where they would be one file.
validatePayload ∷ [PayloadFile] → Either Text [PayloadDescriptor]
validatePayload [] = Left "payload has no files"
validatePayload files = do
    mapM_ (validatePayloadName . pfName) files
    let folded = map (T.toLower . pfName) files
    case [ n | (n, i) ← zip folded [0 ∷ Int ..], n `elem` take i folded ] of
        (dup : _) → Left ("payload names collide (case-insensitively): " <> dup)
        []        → Right (L.sortOn pdName (map payloadDescriptor files))

payloadDescriptor ∷ PayloadFile → PayloadDescriptor
payloadDescriptor f = PayloadDescriptor
    { pdName   = pfName f
    , pdSize   = fromIntegral (BS.length (pfBytes f))
    , pdDigest = SHA256.hash (pfBytes f)
    }

-- | The entry's integrity value: one digest over the canonically
--   ordered inventory. Two entries with equal inventories — the same
--   names, sizes and per-file digests — have equal inventory digests
--   whatever order their descriptors were supplied in, which is what
--   makes republishing identical content recognisably idempotent.
inventoryDigest ∷ [PayloadDescriptor] → BS.ByteString
inventoryDigest = SHA256.hash . S.encode . L.sort

-- Record files ---------------------------------------------------------------

-- | Framing shared by both record files: an 8-byte magic that also
--   carries the format version, the cereal-encoded body, and a trailing
--   SHA-256 of the body. A truncated, bit-flipped, or foreign file fails
--   the length, digest or magic check before the body is ever decoded,
--   so "torn" is a classification, not a crash.
frame ∷ BS.ByteString → BS.ByteString → BS.ByteString
frame magic body = magic <> body <> SHA256.hash body

unframe ∷ BS.ByteString → Text → BS.ByteString → Either Text BS.ByteString
unframe magic what bytes
    | BS.length bytes < BS.length magic + 32 =
        Left (what <> " is truncated (" <> tshow (BS.length bytes) <> " bytes)")
    | BS.take (BS.length magic) bytes ≢ magic =
        Left (what <> " does not carry the expected magic")
    | SHA256.hash body ≢ trailer =
        Left (what <> " fails its integrity check")
    | otherwise = Right body
  where
    rest    = BS.drop (BS.length magic) bytes
    body    = BS.take (BS.length rest - 32) rest
    trailer = BS.drop (BS.length rest - 32) rest

entryMagic, registryMagic ∷ BS.ByteString
entryMagic    = BC.pack "SYNLIBE1"
registryMagic = BC.pack "SYNLIBR1"

encodeEntryRecord ∷ EntryRecord → BS.ByteString
encodeEntryRecord = frame entryMagic . S.encode

-- | Decode and structurally validate an entry record: framed, decodable,
--   at least one file, files ascending by name and unique, every digest
--   32 bytes. A record failing any of these is not an entry record.
decodeEntryRecord ∷ BS.ByteString → Either Text EntryRecord
decodeEntryRecord bytes = do
    body ← unframe entryMagic "entry record" bytes
    rec  ← either (\e → Left ("entry record does not decode: " <> T.pack e))
                  Right (S.decode body)
    let names = map pdName (erFiles rec)
    when (null names) $ Left "entry record lists no files"
    unless (names ≡ L.sort names ∧ length (L.nub names) ≡ length names) $
        Left "entry record files are not ascending and unique by name"
    mapM_ (either Left Right . validatePayloadName) names
    unless (all ((≡ 32) . BS.length . pdDigest) (erFiles rec)) $
        Left "entry record carries a digest of the wrong length"
    pure rec

encodeRegistry ∷ RegistryFile → BS.ByteString
encodeRegistry = frame registryMagic . S.encode

-- | Decode and structurally validate the registry: framed, decodable,
--   rows ascending and duplicate-free by id.
decodeRegistry ∷ BS.ByteString → Either Text RegistryFile
decodeRegistry bytes = do
    body ← unframe registryMagic "registry" bytes
    reg  ← either (\e → Left ("registry does not decode: " <> T.pack e))
                  Right (S.decode body)
    let ids = map rrId (rfRows reg)
    unless (ids ≡ L.sort ids ∧ length (L.nub ids) ≡ length ids) $
        Left "registry rows are not ascending and unique by id"
    pure reg
