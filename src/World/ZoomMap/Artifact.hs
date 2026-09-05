{-# LANGUAGE Strict, TemplateHaskell #-}
-- | A non-authoritative, exact cache of the expensive zoom-map
-- reconstruction.  Every read failure is a cache miss: saves remain the
-- authority and the existing pure builder remains the fallback.
module World.ZoomMap.Artifact
    ( ZoomArtifactKey(..)
    , ZoomArtifact(..)
    , buildZoomArtifactKey
    , loadZoomArtifact
    , loadZoomArtifactAt
    , publishZoomArtifact
    , publishZoomArtifactAt
    , encodeZoomArtifact
    , decodeZoomArtifact
    , zoomArtifactPath
    , zoomArtifactMaxBytes
    ) where

import UPrelude
import Control.Exception (IOException, try, finally)
import Control.Monad (filterM)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.List as L
import Data.Serialize (encode)
import Data.Serialize.Get
    ( Get, getByteString, getInt32be, getWord8, getWord32be, runGet )
import Data.Serialize.Put
    ( Put, putByteString, putInt32be, putWord8, putWord32be, putWord64be
    , runPut )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Float (castFloatToWord32)
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , doesPathExist, getFileSize, listDirectory, makeAbsolute
    , pathIsSymbolicLink, removeFile, renameFile )
import System.FilePath (makeRelative, takeDirectory, takeExtension)
import System.IO (hClose, hFlush, openBinaryTempFile)
import Language.Haskell.TH.Syntax
    ( Loc(..), addDependentFile, lift, location, runIO )
import World.Generate.Types (WorldGenParams(..))
import World.Material
    ( MaterialId(..), MaterialProps(..), MaterialRegistry
    , getMaterialProps, isKnownMaterial )
import World.ZoomMap.Types (ZoomChunkEntry(..), zoomTileSize)

data ZoomArtifactKey = ZoomArtifactKey
    { zakProducerDigest  ∷ !BS.ByteString
    , zakParamsDigest    ∷ !BS.ByteString
    , zakResourcesDigest ∷ !BS.ByteString
    , zakRegistryDigest  ∷ !BS.ByteString
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
schemaVersion = 3
-- Bump only for a deliberate artifact-semantic change. Ordinary production
-- code/build changes invalidate automatically through 'artifactProducerDigest'.
semanticVersion = 1

entryRecordBytes, pixelBlockBytes, headerBytes ∷ Int
entryRecordBytes = 24
pixelBlockBytes = zoomTileSize * zoomTileSize * 4
headerBytes = 8 + 5 * 4 + 6 * 32

-- | SHA-256 of every production Haskell/C source and Cabal build input,
-- embedded at compile time.  'addDependentFile' makes GHC rebuild this module
-- when any member changes, so an artifact produced by another code build
-- cannot become a hit merely because params and resources stayed the same.
artifactProducerDigest ∷ BS.ByteString
artifactProducerDigest = BS.pack $(do
    loc ← location
    sourcePath ← runIO $ makeAbsolute (loc_filename loc)
    let projectRoot = iterate takeDirectory sourcePath !! 4
        walk dir = do
            names ← L.sort ⊚ listDirectory dir
            concat ⊚ forM names (\name → do
                let path = dir ⊘ name
                directory ← doesDirectoryExist path
                if directory then walk path else pure [path])
        accepted path = takeExtension path ∈ [".hs", ".c", ".h"]
        length64 bytes = runPut $ putWord64be (fromIntegral (BS.length bytes))
        configNames = ["synarchy.cabal", "cabal.project", "cabal.project.freeze"]
    sourceFiles ← runIO $ concat ⊚ mapM walk
        [projectRoot ⊘ "src", projectRoot ⊘ "app", projectRoot ⊘ "cbits"]
    configFiles ← runIO $ filterM doesFileExist
        [projectRoot ⊘ name | name ← configNames]
    let inputs = L.sort (filter accepted sourceFiles <> configFiles)
    mapM_ addDependentFile inputs
    fields ← runIO $ forM inputs (\path → do
        content ← BS.readFile path
        let relative = TE.encodeUtf8 (T.pack (makeRelative projectRoot path))
        pure [length64 relative, relative, length64 content, content])
    lift $ BS.unpack $ SHA256.finalize $
        SHA256.updates SHA256.init (concat fields))

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
buildZoomArtifactKey
    ∷ WorldGenParams → MaterialRegistry → IO (Either Text ZoomArtifactKey)
buildZoomArtifactKey params registry =
  case artifactEntryCount params ≫= validateArtifactSize of
    Left reason → pure (Left reason)
    Right (_, count) → do
      result ← try $ do
        paths ← L.sort . concat ⊚ mapM listFilesRecursive resourceRoots
        fields ← forM paths $ \path → do
            content ← BS.readFile path
            let relative = TE.encodeUtf8 (T.pack (makeRelative "." path))
            pure [lengthField relative, relative, lengthField content, content]
        let resources = SHA256.finalize $
                SHA256.updates SHA256.init (concat fields)
        pure $ Right ZoomArtifactKey
            { zakProducerDigest = artifactProducerDigest
            , zakParamsDigest = SHA256.hash (encode params)
            , zakResourcesDigest = resources
            , zakRegistryDigest = materialRegistryDigest registry
            , zakEntryCount = count
            }
      pure $ case (result ∷ Either IOException (Either Text ZoomArtifactKey)) of
        Left e → Left ("cannot fingerprint zoom inputs: " <> tshow e)
        Right value → value

loadZoomArtifact ∷ ZoomArtifactKey → IO (Either Text ZoomArtifact)
loadZoomArtifact = loadZoomArtifactAt zoomArtifactPath

loadZoomArtifactAt ∷ FilePath → ZoomArtifactKey → IO (Either Text ZoomArtifact)
loadZoomArtifactAt path key = do
    result ← try $ do
        let cacheDir = takeDirectory path
            cacheRoot = takeDirectory cacheDir
        unsafeRoot ← existingSymlink cacheRoot
        unsafeDir ← existingSymlink cacheDir
        unsafeFile ← existingSymlink path
        if unsafeRoot ∨ unsafeDir ∨ unsafeFile
          then pure $ Left "cache path contains a symbolic link"
          else do
            exists ← doesPathExist path
            if not exists
              then pure $ Left "artifact is absent"
              else do
                regular ← doesFileExist path
                if not regular
                  then pure $ Left "artifact path is not a regular file"
                  else do
                    bytesOnDisk ← getFileSize path
                    if bytesOnDisk > zoomArtifactMaxBytes
                      then pure $ Left "artifact exceeds the 64 MiB limit"
                      else decodeZoomArtifact key ⊚ BS.readFile path
    pure $ case (result ∷ Either IOException (Either Text ZoomArtifact)) of
        Left e → Left ("cannot read zoom artifact: " <> tshow e)
        Right value → value

-- | Write one complete candidate next to the destination, then replace the
-- destination with one rename.  Failure never affects the world operation.
publishZoomArtifact
    ∷ ZoomArtifactKey → V.Vector ZoomChunkEntry
    → V.Vector BS.ByteString → IO (Either Text Int)
publishZoomArtifact = publishZoomArtifactAt zoomArtifactPath

publishZoomArtifactAt
    ∷ FilePath → ZoomArtifactKey → V.Vector ZoomChunkEntry
    → V.Vector BS.ByteString → IO (Either Text Int)
publishZoomArtifactAt path key entries pixels =
    case encodeZoomArtifact key entries pixels of
      Left reason → pure (Left reason)
      Right bytes → do
        result ← try $ do
            let cacheDir = takeDirectory path
                cacheRoot = takeDirectory cacheDir
            unsafeRoot ← existingSymlink cacheRoot
            unsafeDir ← existingSymlink cacheDir
            unsafeFile ← existingSymlink path
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
                    renameFile candidate path
                    pure (Right (BS.length bytes))) `finally` cleanup
        pure $ case (result ∷ Either IOException (Either Text Int)) of
            Left e → Left ("cannot publish zoom artifact: " <> tshow e)
            Right value → value

encodeZoomArtifact
    ∷ ZoomArtifactKey → V.Vector ZoomChunkEntry
    → V.Vector BS.ByteString → Either Text BS.ByteString
encodeZoomArtifact key entries pixels = do
    unless (digestLengthOK key) $ Left "zoom artifact key digest length mismatch"
    (projectedBytes, _) ← validateArtifactSize (zakEntryCount key)
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
                [ zakProducerDigest key, zakParamsDigest key
                , zakResourcesDigest key
                , zakRegistryDigest key
                , SHA256.hash entryPayload, SHA256.hash pixelPayload ]
        artifact = header <> entryPayload <> pixelPayload
    unless (toInteger (BS.length artifact) ≡ projectedBytes) $
        Left "internal zoom artifact length mismatch"
    pure artifact

decodeZoomArtifact ∷ ZoomArtifactKey → BS.ByteString → Either Text ZoomArtifact
decodeZoomArtifact key bytes = do
    unless (digestLengthOK key) $ Left "zoom artifact key digest length mismatch"
    unless (BS.length bytes >= headerBytes) $ Left "zoom artifact is truncated"
    (count, producerDigest, paramsDigest, resourcesDigest, registryDigest
        , entriesDigest, pixelsDigest) ←
        first T.pack $ runGet getHeader (BS.take headerBytes bytes)
    unless (count ≡ zakEntryCount key) $ Left "zoom artifact count is stale"
    unless (producerDigest ≡ zakProducerDigest key) $
        Left "zoom artifact producer is stale"
    unless (paramsDigest ≡ zakParamsDigest key) $
        Left "zoom artifact parameters are stale"
    unless (resourcesDigest ≡ zakResourcesDigest key) $
        Left "zoom artifact resources are stale"
    unless (registryDigest ≡ zakRegistryDigest key) $
        Left "zoom artifact material registry is stale"
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

getHeader
    ∷ Get ( Int, BS.ByteString, BS.ByteString, BS.ByteString
          , BS.ByteString, BS.ByteString, BS.ByteString )
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
    producerDigest ← getByteString 32
    paramsDigest ← getByteString 32
    resourcesDigest ← getByteString 32
    registryDigest ← getByteString 32
    entriesDigest ← getByteString 32
    pixelsDigest ← getByteString 32
    pure ( fromIntegral countWord, producerDigest, paramsDigest, resourcesDigest
         , registryDigest, entriesDigest, pixelsDigest )

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
digestLengthOK key = BS.length (zakProducerDigest key) ≡ 32
                  ∧ BS.length (zakParamsDigest key) ≡ 32
                  ∧ BS.length (zakResourcesDigest key) ≡ 32
                  ∧ BS.length (zakRegistryDigest key) ≡ 32

-- | Hash the complete effective registry in numeric material-id order.  Zoom
-- generation currently reads hardness directly, but retaining every property
-- is deliberately conservative: a future zoom-relevant use cannot turn an
-- artifact built under different runtime YAML overrides into a stale hit.
materialRegistryDigest ∷ MaterialRegistry → BS.ByteString
materialRegistryDigest registry = SHA256.hash $ runPut $
    forM_ ([0 .. 255] ∷ [Word8]) $ \mid → do
        let materialId = MaterialId mid
            props = getMaterialProps registry materialId
        putWord8 mid
        putBool (isKnownMaterial registry materialId)
        putText (mpName props)
        mapM_ (putWord32be . castFloatToWord32)
            [ mpHardness props, mpDensity props, mpAlbedo props
            , mpDrainage props, mpPickSpeed props, mpShovelSpeed props
            , mpDigBulking props, mpMoveCost props ]
        putMaybeText (mpDigSpoil props)
        putMaybeText (mpDigChunk props)
        putBool (mpDigGems props)
  where
    putBool value = putWord8 (if value then 1 else 0)
    putText value = do
        let bytes = TE.encodeUtf8 value
        putWord64be (fromIntegral (BS.length bytes))
        putByteString bytes
    putMaybeText Nothing = putWord8 0
    putMaybeText (Just value) = putWord8 1 >> putText value

artifactEntryCount ∷ WorldGenParams → Either Text Int
artifactEntryCount params =
    let worldSize = wgpWorldSize params
        countInteger = toInteger worldSize * toInteger worldSize `div` 2
    in if worldSize <= 0 ∨ countInteger > toInteger (maxBound ∷ Int)
       then Left "invalid zoom artifact world size"
       else Right (fromInteger countInteger)

-- The size is exact, so reject an unsupported world before fingerprinting
-- resources, traversing vectors, concatenating blocks, hashing, or opening a
-- candidate file.  w128 fits; w256 and larger deliberately use the unchanged
-- reconstruction path without paying any artifact materialization cost.
validateArtifactSize ∷ Int → Either Text (Integer, Int)
validateArtifactSize count =
    let projected = toInteger headerBytes
            + toInteger count
                * (toInteger entryRecordBytes + toInteger pixelBlockBytes)
    in if count < 0 ∨ toInteger count > toInteger (maxBound ∷ Word32)
       then Left "invalid zoom artifact entry count"
       else if projected > zoomArtifactMaxBytes
       then Left "zoom artifact exceeds the 64 MiB limit"
       else Right (projected, count)

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
