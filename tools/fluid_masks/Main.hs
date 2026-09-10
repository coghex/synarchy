{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
-- Export the production Haskell authority as raw RGBA8 for the PNG tool.
module Main (main) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as VS
import System.Environment (getArgs)
import System.FilePath ((</>))
import World.Slope.FaceMaps (generateFluidLevelFaceMap)

main ∷ IO ()
main = do
    args ← getArgs
    case args of
        [destination] → forM_ [1..8] $ \level →
            case generateFluidLevelFaceMap level of
                Nothing → fail "invalid fluid level"
                Just pixels → BS.writeFile
                    (destination </> ("isoface_level_" ++ show level ++ ".rgba"))
                    (BS.pack (VS.toList pixels))
        _ → fail "usage: fluid-mask-export OUTPUT_DIRECTORY"
