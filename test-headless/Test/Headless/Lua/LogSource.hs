{-# LANGUAGE OverloadedStrings, TypeApplications #-}
-- | The source field of a Lua log line's @[source:line]@ prefix (#1960).
--
--   All four @engine.log*@ functions build that prefix from the chunk
--   the caller lives in. The shortener they used to share was written
--   for repository-relative paths and treated EVERY source as one: a
--   source with no @\/@ was consumed down to @\"\"@, and a source whose
--   @\/@ was not a path separator was replaced by whatever followed it.
--   Both are reachable, because two engine surfaces feed the same
--   logging path a chunk source that is not a path at all — the debug
--   console names its chunk @=@ plus the entered code
--   ('Engine.Scripting.Lua.Thread.Console.executeDebugLua'), and the
--   in-game shell compiles with 'HsLua.Core.loadstring', whose label is
--   @[string \"...\"]@. So @engine.logInfo(\"x\")@ typed at the console
--   logged @[:1]@, and @local a=8\/2; engine.logInfo(\"x\")@ logged the
--   tail of the operator's own command as the source.
--
--   Two halves, and the second is why the first can be trusted:
--
--   1. The pure transformation, pinned input-by-input. Path-backed
--      sources are a REGRESSION guard — today's output is correct and
--      must not move.
--   2. Real Lua metadata. @short_src@ alone cannot tell a path from a
--      label (a file chunk and a @=@-named one can produce identical
--      text), so the classification reads the source-kind byte
--      @cbits\/lua_debug.c@ now reports. These examples compile chunks
--      the three ways the engine actually does — the console's
--      'HsLua.Core.loadbuffer' name, 'HsLua.Core.loadstring', and a real
--      file — and check the kind that arrives, which synthetic inputs
--      cannot.
--
--   The probe is pushed with 'HsLua.Core.pushHaskellFunction', the same
--   primitive @registerLuaFunction@ gives every @engine.log*@ binding,
--   and reads stack level 2 for the same reason production does: level 0
--   is the userdata call, level 1 hslua's wrapping C closure, level 2
--   the Lua caller.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match \"Lua log source\"'@.
module Test.Headless.Lua.LogSource (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified HsLua as Lua
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))

import Engine.Scripting.Lua.API.Log (logSourceField, shortenChunkPath)
import Engine.Scripting.Lua.Debug
    (ChunkSourceInfo(..), ChunkKind(..), getChunkSourceInfo)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

spec ∷ Spec
spec = do

    -- * Path-backed sources: today's output, guarded

    describe "a file-backed chunk names a path and is shortened" $ do
        let shortens input expected =
                it (show input <> " -> " <> show expected) $ do
                    shortenChunkPath input `shouldBe` expected
                    logSourceField (Just (fileChunk input)) `shouldBe` expected

        -- The two the issue pins verbatim, from the live engine.
        shortens "./scripts/unit_ai.lua" "unit_ai.lua"
        shortens "./scripts/ui/panel.lua" "ui/panel.lua"

        -- The nested segment is retained deliberately; it is the useful
        -- half of a 'scripts/ui/...' path.
        shortens "./scripts/foo.lua" "foo.lua"
        shortens "./scripts/ui/foo.lua" "ui/foo.lua"

        -- Engine.Scripting.Lua.Script hands Lua.dofileTrace the relative
        -- form with no './' prefix, so this shape is live too.
        shortens "scripts/foo.lua" "foo.lua"

        -- Nothing to drop: returned whole rather than consumed to "".
        shortens "foo.lua" "foo.lua"

        -- An absolute path loses only its root.
        shortens "/abs/foo.lua" "abs/foo.lua"

        -- Empty in, empty out — there is nothing to name.
        shortens "" ""

    -- * Everything else is a label, and survives intact

    describe "a chunk that is not a file reaches the log line unchanged" $ do
        let keeps kind input =
                it (show kind <> " " <> show input) $
                    logSourceField (Just (chunkOf kind input))
                        `shouldBe` input

        -- The debug console's own convention: '=' plus the entered code,
        -- so short_src IS the command. Without a '/' it used to vanish;
        -- with one, its tail became the source field.
        keeps ChunkNamed "engine.logInfo(\"PLAIN\")"
        keeps ChunkNamed "local a=8/2; engine.logInfo(\"SLASH\")"

        -- The in-game shell's loadstring label.
        keeps ChunkString "[string \"engine.logInfo(\\\"SHELL\\\")\"]"

        -- Lua's own placeholder for a chunk it cannot name.
        keeps ChunkNamed "?"

        -- A label may be empty; it is still not a path to shorten.
        keeps ChunkString ""

    describe "a frame Lua reports nothing for" $
        it "is named <unknown> rather than erased" $ do
            logSourceField Nothing `shouldBe` "<unknown>"
            -- The same text arriving AS a source is equally untouched:
            -- before #1960 the shortener consumed it to "".
            logSourceField (Just (chunkOf ChunkUnknownKind "<unknown>"))
                `shouldBe` "<unknown>"

    -- * Real Lua, real chunk metadata

    describe "real chunk metadata" $ do

        it "classifies the debug console's own loadbuffer name as a \
           \label, not a path" $ do
            -- Byte-for-byte executeDebugLua's chunkName convention.
            let code = "local a=8/2; probe()"
            r ← probeChunkSource
                    (Lua.loadbuffer code (Lua.Name ("=" <> code)))
            withChunk r $ \csi → do
                csiKind csi `shouldBe` ChunkNamed
                csiSource csi `shouldBe` BSC.unpack code
                csiCurrentLine csi `shouldBe` 1
                -- The whole command, not the tail after its '/'.
                logSourceField (Just csi) `shouldBe` BSC.unpack code

        it "classifies loadstring's chunk as a string label" $ do
            -- shellTryLoadAndRun's primitive.
            r ← probeChunkSource (Lua.loadstring "probe()")
            withChunk r $ \csi → do
                csiKind csi `shouldBe` ChunkString
                csiSource csi `shouldBe` "[string \"probe()\"]"
                logSourceField (Just csi) `shouldBe` "[string \"probe()\"]"

        it "classifies a real file-backed chunk as a path and shortens it" $
            withTempTree "log-source" $ \root → do
                createDirectoryIfMissing True (root </> "scripts" </> "ui")
                writeFile (root </> "scripts" </> "ui" </> "probe.lua")
                          "probe()\n"
                r ← withCurrentDirectory root $ probeChunkSource
                        (Lua.loadfile (Just "./scripts/ui/probe.lua"))
                withChunk r $ \csi → do
                    csiKind csi `shouldBe` ChunkFile
                    csiSource csi `shouldBe` "./scripts/ui/probe.lua"
                    csiCurrentLine csi `shouldBe` 1
                    logSourceField (Just csi) `shouldBe` "ui/probe.lua"

        it "reports no frame at all when nothing Lua is calling" $ do
            -- The '<unknown>' fallback's own precondition: level 2 with
            -- no Lua caller under the probe.
            info ← Lua.run @Lua.Exception (getChunkSourceInfo 2)
            info `shouldBe` Nothing
            logSourceField info `shouldBe` "<unknown>"

-- * Helpers

-- | A synthetic file-backed chunk carrying the given source text.
fileChunk ∷ String → ChunkSourceInfo
fileChunk = chunkOf ChunkFile

chunkOf ∷ ChunkKind → String → ChunkSourceInfo
chunkOf kind src = ChunkSourceInfo { csiSource      = src
                                   , csiCurrentLine = 1
                                   , csiKind        = kind }

-- | Compile a chunk the given way, call a Haskell probe from inside it,
--   and report the metadata the log functions would see for that frame.
probeChunkSource ∷ Lua.LuaE Lua.Exception Lua.Status
                 → IO (Either String ChunkSourceInfo)
probeChunkSource load = do
    seen ← newIORef (Left "the probe function was never called")
    Lua.run @Lua.Exception $ do
        Lua.pushHaskellFunction $ do
            info ← getChunkSourceInfo 2
            Lua.liftIO . writeIORef seen $
                maybe (Left "Lua reported no frame at level 2") Right info
            pure 0
        Lua.setglobal "probe"
        let failWith what = do
                mErr ← Lua.tostring (-1)
                Lua.liftIO . writeIORef seen . Left $
                    what <> maybe "" ((": " <>) . BSC.unpack) mErr
        status ← load
        if status ≢ Lua.OK
            then failWith ("chunk did not compile (" <> show status <> ")")
            else do
                called ← Lua.pcall 0 0 Nothing
                when (called ≢ Lua.OK) $
                    failWith ("chunk did not run (" <> show called <> ")")
    readIORef seen

withChunk ∷ Either String ChunkSourceInfo
          → (ChunkSourceInfo → Expectation) → Expectation
withChunk (Left err)  _ = expectationFailure err
withChunk (Right csi) k = k csi

-- | A private scratch tree, removed however the action ends.
withTempTree ∷ String → (FilePath → IO α) → IO α
withTempTree label = withExclusiveTempDirectory ("synarchy-" <> label)
