-- Cabal 3.16's C-source check compares the .c and .o timestamps only:
-- Distribution.Simple.GHC.Build.Utils.checkNeedsRecompilation. A shared-header
-- edit can therefore link incompatible struct layouts from different builds.
-- Invalidate only affected audio outputs, inside this build's own directory.
module BuildSupport.AudioDependencies (invalidateAudioDependencies) where

import Control.Monad (filterM, forM_, when)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime,
                         listDirectory, removeFile)
import System.FilePath ((</>), replaceExtension, takeExtension, makeRelative)

invalidateAudioDependencies :: FilePath -> IO ()
invalidateAudioDependencies output = do
    native <- filesUnder "cbits/audio"
    bindings <- filesUnder "src/Engine/Audio"
    let vendor = "cbits/vendor/miniaudio/miniaudio.h"
        public = "cbits/audio/syn_audio.h"
        headers = vendor : filter ((== ".h") . takeExtension) native
    forM_ (filter ((== ".c") . takeExtension) native) $ \source ->
        forM_ ["o", "dyn_o", "p_o", "p_dyn_o"] $ \suffix ->
            invalidate headers (output </> replaceExtension source suffix)
    forM_ ["o", "dyn_o", "p_o", "p_dyn_o"] $ \suffix ->
        invalidate [vendor] (output </> "cbits/vendor/miniaudio/miniaudio." ++ suffix)
    -- hsc2hs is also timestamp-based. Recreate its generated .hs when the public
    -- ABI changes, before Cabal's ordinary preprocessing phase runs.
    forM_ (filter ((== ".hsc") . takeExtension) bindings) $ \source ->
        invalidate [public] (output </> replaceExtension (makeRelative "src" source) "hs")

invalidate :: [FilePath] -> FilePath -> IO ()
invalidate dependencies target = do
    exists <- doesFileExist target
    when exists $ do
        built <- getModificationTime target
        inputs <- filterM doesFileExist dependencies
        changed <- or <$> mapM (fmap (> built) . getModificationTime) inputs
        when changed (removeFile target)

filesUnder :: FilePath -> IO [FilePath]
filesUnder directory = do
    exists <- doesDirectoryExist directory
    if not exists then pure [] else do
        names <- listDirectory directory
        concat <$> mapM (\name -> do
            let path = directory </> name
            nested <- doesDirectoryExist path
            if nested then filesUnder path else pure [path]) names
