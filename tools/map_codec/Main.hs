-- Manual-only bridge for #2303. Compiled against the existing library;
-- it is deliberately not a Cabal component or a production CLI mode.
module Main (main) where

import UPrelude
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import qualified Codec.Picture as JP
import qualified Crypto.Hash.SHA256 as SHA
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Corpus (generateCorpus, inventories)

edge, payloadBytes ∷ Int
edge = 514
payloadBytes = edge * edge * 4

-- Copying materializes the RGBA boundary; timing never stops at a lazy
-- decoder thunk. The build disables CSE and full laziness for repeats.
encodePage ∷ String → BS.ByteString → BS.ByteString
encodePage "raw" bytes = bytes
encodePage "png" bytes = BL.toStrict $ JP.encodePng $
    let (ptr, offset, size) = BSI.toForeignPtr bytes
    in JP.Image edge edge (VS.unsafeFromForeignPtr ptr offset size) ∷ JP.Image JP.PixelRGBA8
encodePage other _ = error ("unknown codec: " ⧺ other)

decodePage ∷ String → BS.ByteString → Either String BS.ByteString
decodePage "raw" bytes
    | BS.length bytes ≡ payloadBytes = Right (BS.copy bytes)
    | otherwise = Left "incorrect raw RGBA length"
decodePage "png" bytes = do
    dyn ← JP.decodePng bytes
    let img = JP.convertRGBA8 dyn
    if JP.imageWidth img ≡ edge ∧ JP.imageHeight img ≡ edge
      then let (ptr, offset, size) = VS.unsafeToForeignPtr (JP.imageData img)
           in Right (BS.copy (BSI.fromForeignPtr ptr offset size))
      else Left "incorrect PNG dimensions"
decodePage other _ = Left ("unknown codec: " ⧺ other)

timed ∷ NFData α ⇒ IO α → IO (Word64, α)
timed action = do
    begin ← getMonotonicTimeNSec
    result ← action ≫= evaluate ∘ force
    end ← getMonotonicTimeNSec
    pure (end - begin, result)

emit ∷ A.Value → IO ()
emit = BL.putStr ∘ (<> "\n") ∘ A.encode

main ∷ IO ()
main = do
    args ← getArgs
    case args of
      ["inventory"] → inventories ≫= emit
      ["generate", manifest, output] → generateCorpus manifest output
      ["encode", codec, input, output] → do
        bytes ← BS.readFile input ≫= evaluate ∘ force
        unless (BS.length bytes ≡ payloadBytes) (fail "incorrect RGBA input")
        -- One warmup then five independently executed measured encodes.
        _ ← evaluate (force (encodePage codec (BS.copy bytes)))
        samples ← forM [1 ∷ Int .. 5] $ \_ → do
            fresh ← evaluate (force (BS.copy bytes))
            timed (pure (encodePage codec fresh))
        let outputs = map snd samples
        case outputs of
          [] → fail "no encode samples"
          firstBytes : _ → do
            BS.writeFile output firstBytes
            BS.writeFile (output ⧺ ".sha256") (SHA.hash firstBytes)
            emit $ A.object
                [ "nanoseconds" A..= map fst samples
                , "in_process_identical" A..= all (≡ firstBytes) outputs ]
      [operation, codec, input, output]
        | operation `elem` ["decode", "native", "baseline"] → do
          bytes ← BS.readFile input ≫= evaluate ∘ force
          expected ← BS.readFile (input ⧺ ".sha256") ≫= evaluate ∘ force
          if operation ≡ "baseline"
            then emit (A.object ["input_bytes" A..= BS.length bytes])
            else do
              (elapsed, decoded) ← timed $ pure $
                  if operation ≡ "decode" ∧ SHA.hash bytes ≢ expected
                  then Left "external SHA-256 mismatch"
                  else decodePage codec bytes
              case decoded of
                Left reason → emit $ A.object
                    ["ok" A..= False, "reason" A..= reason,
                     "nanoseconds" A..= elapsed]
                Right rgba → do
                    BS.writeFile output rgba
                    emit $ A.object ["ok" A..= True,
                        "nanoseconds" A..= elapsed]
      ["bulk", codec, manifest, countText] → do
        paths ← A.eitherDecodeFileStrict manifest ≫= either fail pure
        count ← maybe (fail "invalid count") pure (readMaybe countText ∷ Maybe Int)
        unless (count > 0) (fail "count must be positive")
        inputs ← forM (paths ∷ [FilePath]) $ \path → do
            b ← BS.readFile path ≫= evaluate ∘ force
            d ← BS.readFile (path ⧺ ".sha256") ≫= evaluate ∘ force
            pure (b, d)
        (elapsed, lengths) ← timed $ forM [1 .. count] $ \_ →
            forM inputs $ \(b, digest) → do
                fresh ← evaluate (force (BS.copy b))
                when (SHA.hash fresh ≢ digest) (fail "bulk integrity failure")
                rgba ← either fail (evaluate ∘ force) (decodePage codec fresh)
                pure (BS.length rgba)
        emit $ A.object ["nanoseconds" A..= elapsed,
                         "pages" A..= sum (map length lengths)]
      _ → hPutStrLn stderr "map-codec: invalid command" ≫ fail "invalid command"
