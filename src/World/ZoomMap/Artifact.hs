{-# LANGUAGE Strict #-}
-- | A non-authoritative, exact cache of the expensive zoom-map
-- reconstruction.  Every read failure is a cache miss: saves remain the
-- authority and the existing pure builder remains the fallback.
module World.ZoomMap.Artifact
    ( ZoomArtifactKey(..)
    , ZoomArtifact(..)
    , buildZoomArtifactKey
    , loadZoomArtifact
    , publishZoomArtifact
    , encodeZoomArtifact
    , decodeZoomArtifact
    , zoomArtifactPath
    , zoomArtifactMaxBytes
    ) where

import UPrelude
import Control.Exception (IOException, try, finally)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.List as L
import Data.Serialize (encode)
import Data.Serialize.Get
    ( Get, getByteString, getInt32be, getWord8, getWord32be, runGet )
import Data.Serialize.Put
    ( Put, putByteString, putInt32be, putWord8, putWord32be, runPut )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , doesPathExist, getFileSize, listDirectory, pathIsSymbolicLink
    , removeFile, renameFile )
import System.FilePath (makeRelative)
import System.IO (hClose, hFlush, openBinaryTempFile)
import World.Generate.Types (WorldGenParams(..))
import World.ZoomMap.Types (ZoomChunkEntry(..), zoomTileSize)

data ZoomArtifactKey = ZoomArtifactKey
    { zakParamsDigest    ∷ !BS.ByteString
    , zakResourcesDigest ∷ !BS.ByteString
    , zakEntryCount      ∷ !Int
    } deriving (Eq, Show)

data ZoomArtifact = ZoomArtifact
    { zaEntries ∷ !(V.Vector ZoomChunkEntry)
    , zaPixels  ∷ !(V.Vector BS.ByteString)
    , zaBytes   ∷ !Int
    } deriving (Eq, Show)

zoomArtifactPath ∷ FilePath
zoomArtifactPath = "cache" ⊘ "zoom" ⊘ "current.zarf"

zoomArtifactMaxBytes ∷ Integer
zoomArtifactMaxBytes = 64 * 1024 * 1024

magic ∷ BS.ByteString
magic = "SZARF001"

schemaVersion, semanticVersion ∷ Word32
schemaVersion = 1
semanticVersion = 1

entryRecordBytes, pixelBlockBytes, headerBytes ∷ Int
entryRecordBytes = 24
pixelBlockBytes = zoomTileSize * zoomTileSize * 4
headerBytes = 8 + 5 * 4 + 4 * 32

resourceRoots ∷ [FilePath]
resourceRoots =
    [ "data/materials"
    , "data/vegetation"
    , "assets/textures/world/zoommap"
    , "assets/textures/vegetation"
    ]

-- | The full structural encoding is intentionally conservative: a field
-- that cannot affect zoom output may cause a miss, but can never permit a
-- stale hit.  Resource paths and bytes are both included in stable order.
buildZoomArtifactKey ∷ WorldGenParams → IO (Either Text ZoomArtifactKey)
buildZoomArtifactKey params = do
    result ← try $ do
        paths ← L.sort . concat ⊚ mapM listFilesRecursive resourceRoots
        fields ← forM paths $ \path → do
            content ← BS.readFile path
            let relative = TE.encodeUtf8 (T.pack (makeRelative "." path))
            pure [lengthField relative, relative, lengthField content, content]
        let resources = SHA256.finalize $
                SHA256.updates SHA256.init (concat fields)
            worldSize = wgpWorldSize params
            countInteger = toInteger worldSize * toInteger worldSize `div` 2
        if worldSize <= 0 ∨ countInteger > toInteger (maxBound ∷ Int)
          then pure $ Left "invalid zoom artifact world size"
          else pure $ Right ZoomArtifactKey
              { zakParamsDigest = SHA256.hash (encode params)
              , zakResourcesDigest = resources
              , zakEntryCount = fromInteger countInteger
              }
    pure $ case (result ∷ Either IOException (Either Text ZoomArtifactKey)) of
        Left e → Left ("cannot fingerprint zoom inputs: " <> T.pack (show e))
        Right value → value

loadZoomArtifact ∷ ZoomArtifactKey → IO (Either Text ZoomArtifact)
loadZoomArtifact key = do
    result ← try $ do
        unsafeRoot ← existingSymlink "cache"
        unsafeDir ← existingSymlink ("cache" ⊘ "zoom")
        unsafeFile ← existingSymlink zoomArtifactPath
        if unsafeRoot ∨ unsafeDir ∨ unsafeFile
          then pure $ Left "cache path contains a symbolic link"
          else do
            exists ← doesPathExist zoomArtifactPath
            if not exists
              then pure $ Left "artifact is absent"
              else do
                regular ← doesFileExist zoomArtifactPath
                if not regular
                  then pure $ Left "artifact path is not a regular file"
                  else do
                    bytesOnDisk ← getFileSize zoomArtifactPath
                    if bytesOnDisk > zoomArtifactMaxBytes
                      then pure $ Left "artifact exceeds the 64 MiB limit"
                      else decodeZoomArtifact key ⊚ BS.readFile zoomArtifactPath
    pure $ case (result ∷ Either IOException (Either Text ZoomArtifact)) of
        Left e → Left ("cannot read zoom artifact: " <> T.pack (show e))
        Right value → value

-- | Write one complete candidate next to the destination, then replace the
-- destination with one rename.  Failure never affects the world operation.
publishZoomArtifact
    ∷ ZoomArtifactKey → V.Vector ZoomChunkEntry
    → V.Vector BS.ByteString → IO (Either Text Int)
publishZoomArtifact key entries pixels =
    case encodeZoomArtifact key entries pixels of
      Left reason → pure (Left reason)
      Right bytes → do
        result ← try $ do
            let cacheRoot = "cache"
                cacheDir = cacheRoot ⊘ "zoom"
            unsafeRoot ← existingSymlink cacheRoot
            unsafeDir ← existingSymlink cacheDir
            unsafeFile ← existingSymlink zoomArtifactPath
            if unsafeRoot ∨ unsafeDir ∨ unsafeFile
              then pure $ Left "cache path contains a symbolic link"
              else do
                createDirectoryIfMissing True cacheDir
                (candidate, handle) ← openBinaryTempFile cacheDir "zoom-artifact"
                let cleanup = do
                        closeResult ← try (hClose handle)
                        case (closeResult ∷ Either IOException ()) of _ → pure ()
                        removeResult ← try (removeFile candidate)
                        case (removeResult ∷ Either IOException ()) of _ → pure ()
                (do BS.hPut handle bytes
                    hFlush handle
                    hClose handle
                    renameFile candidate zoomArtifactPath
                    pure (Right (BS.length bytes))) `finally` cleanup
        pure $ case (result ∷ Either IOException (Either Text Int)) of
            Left e → Left ("cannot publish zoom artifact: " <> T.pack (show e))
            Right value → value

encodeZoomArtifact
    ∷ ZoomArtifactKey → V.Vector ZoomChunkEntry
    → V.Vector BS.ByteString → Either Text BS.ByteString
encodeZoomArtifact key entries pixels = do
    unless (digestLengthOK key) $ Left "zoom artifact key digest length mismatch"
    unless (V.length entries ≡ zakEntryCount key
            ∧ V.length pixels ≡ zakEntryCount key) $
        Left "zoom artifact entry/block count mismatch"
    unless (V.all ((≡ pixelBlockBytes) . BS.length) pixels) $
        Left "zoom artifact pixel block size mismatch"
    entryPayload ← encodeEntries entries
    let pixelPayload = BS.concat (V.toList pixels)
        header = runPut $ do
            putByteString magic
            putWord32be schemaVersion
            putWord32be semanticVersion
            putWord32be (fromIntegral (zakEntryCount key))
            putWord32be (fromIntegral entryRecordBytes)
            putWord32be (fromIntegral pixelBlockBytes)
            mapM_ putByteString
                [ zakParamsDigest key, zakResourcesDigest key
                , SHA256.hash entryPayload, SHA256.hash pixelPayload ]
        artifact = header <> entryPayload <> pixelPayload
    unless (toInteger (BS.length artifact) <= zoomArtifactMaxBytes) $
        Left "zoom artifact exceeds the 64 MiB limit"
    pure artifact

decodeZoomArtifact ∷ ZoomArtifactKey → BS.ByteString → Either Text ZoomArtifact
decodeZoomArtifact key bytes = do
    unless (digestLengthOK key) $ Left "zoom artifact key digest length mismatch"
    unless (BS.length bytes >= headerBytes) $ Left "zoom artifact is truncated"
    (count, paramsDigest, resourcesDigest, entriesDigest, pixelsDigest) ←
        first T.pack $ runGet getHeader (BS.take headerBytes bytes)
    unless (count ≡ zakEntryCount key) $ Left "zoom artifact count is stale"
    unless (paramsDigest ≡ zakParamsDigest key) $
        Left "zoom artifact parameters are stale"
    unless (resourcesDigest ≡ zakResourcesDigest key) $
        Left "zoom artifact resources are stale"
    let expectedInteger = toInteger headerBytes
            + toInteger count * toInteger entryRecordBytes
            + toInteger count * toInteger pixelBlockBytes
    unless (expectedInteger <= zoomArtifactMaxBytes
            ∧ expectedInteger ≡ toInteger (BS.length bytes)) $
        Left "zoom artifact length is invalid"
    let entryBytes = count * entryRecordBytes
        entryPayload = BS.take entryBytes (BS.drop headerBytes bytes)
        pixelPayload = BS.drop (headerBytes + entryBytes) bytes
    unless (SHA256.hash entryPayload ≡ entriesDigest) $
        Left "zoom artifact entry hash mismatch"
    unless (SHA256.hash pixelPayload ≡ pixelsDigest) $
        Left "zoom artifact pixel hash mismatch"
    entries ← first T.pack $ runGet (V.replicateM count getEntry) entryPayload
    let pixels = V.generate count $ \i →
            BS.take pixelBlockBytes (BS.drop (i * pixelBlockBytes) pixelPayload)
    pure ZoomArtifact
        { zaEntries = entries, zaPixels = pixels, zaBytes = BS.length bytes }

getHeader ∷ Get (Int, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)
getHeader = do
    observedMagic ← getByteString 8
    unless (observedMagic ≡ magic) $ fail "zoom artifact magic mismatch"
    schema ← getWord32be
    semantic ← getWord32be
    countWord ← getWord32be
    observedEntryBytes ← getWord32be
    observedPixelBytes ← getWord32be
    unless (schema ≡ schemaVersion) $ fail "zoom artifact schema mismatch"
    unless (semantic ≡ semanticVersion) $ fail "zoom artifact semantic version mismatch"
    unless (observedEntryBytes ≡ fromIntegral entryRecordBytes) $
        fail "zoom artifact entry record size mismatch"
    unless (observedPixelBytes ≡ fromIntegral pixelBlockBytes) $
        fail "zoom artifact pixel block size mismatch"
    let countInteger = toInteger countWord
    when (countInteger > toInteger (maxBound ∷ Int)) $
        fail "zoom artifact entry count overflows Int"
    paramsDigest ← getByteString 32
    resourcesDigest ← getByteString 32
    entriesDigest ← getByteString 32
    pixelsDigest ← getByteString 32
    pure ( fromIntegral countWord, paramsDigest, resourcesDigest
         , entriesDigest, pixelsDigest )

encodeEntries ∷ V.Vector ZoomChunkEntry → Either Text BS.ByteString
encodeEntries entries = runPut . mapM_ putEntry ⊚ mapM checked (V.toList entries)
  where
    checked entry = do
        mapM_ checkedInt
            [ zceChunkX entry, zceChunkY entry, zceBaseGX entry
            , zceBaseGY entry, zceElev entry ]
        pure entry
    checkedInt n
        | toInteger n < toInteger (minBound ∷ Int32)
          ∨ toInteger n > toInteger (maxBound ∷ Int32) =
            Left ("zoom entry integer does not fit Int32: " <> tshow n)
        | otherwise = Right ()

putEntry ∷ ZoomChunkEntry → Put
putEntry entry = do
    mapM_ (putInt32be . fromIntegral)
        [ zceChunkX entry, zceChunkY entry, zceBaseGX entry
        , zceBaseGY entry, zceElev entry ]
    putWord8 (zceTexIndex entry)
    putWord8 $ boolBit 0 (zceIsOcean entry)
        ⌄ boolBit 1 (zceHasLava entry)
        ⌄ boolBit 2 (zceHasIce entry)
    putWord8 (zceVegCategory entry)
    putWord8 0

getEntry ∷ Get ZoomChunkEntry
getEntry = do
    [chunkX, chunkY, baseGX, baseGY, elev] ←
        replicateM 5 (fromIntegral ⊚ getInt32be)
    tex ← getWord8
    flags ← getWord8
    veg ← getWord8
    reserved ← getWord8
    unless (reserved ≡ 0) $ fail "zoom artifact reserved byte is nonzero"
    unless (flags <= 7) $ fail "zoom artifact flags have unknown bits"
    pure ZoomChunkEntry
        { zceChunkX = chunkX, zceChunkY = chunkY
        , zceBaseGX = baseGX, zceBaseGY = baseGY
        , zceTexIndex = tex, zceElev = elev
        , zceIsOcean = testBit flags 0, zceHasLava = testBit flags 1
        , zceVegCategory = veg, zceHasIce = testBit flags 2 }

boolBit ∷ Int → Bool → Word8
boolBit bit True = fromIntegral (2 ^ bit ∷ Int)
boolBit _ False = 0

digestLengthOK ∷ ZoomArtifactKey → Bool
digestLengthOK key = BS.length (zakParamsDigest key) ≡ 32
                  ∧ BS.length (zakResourcesDigest key) ≡ 32

lengthField ∷ BS.ByteString → BS.ByteString
lengthField bytes = runPut $ putWord32be (fromIntegral (BS.length bytes))

listFilesRecursive ∷ FilePath → IO [FilePath]
listFilesRecursive root = do
    entries ← L.sort ⊚ listDirectory root
    concat ⊚ forM entries (\entry → do
        let path = root ⊘ entry
        directory ← doesDirectoryExist path
        if directory then listFilesRecursive path else pure [path])

existingSymlink ∷ FilePath → IO Bool
existingSymlink path = do
    exists ← doesPathExist path
    if exists then pathIsSymbolicLink path else pure False
