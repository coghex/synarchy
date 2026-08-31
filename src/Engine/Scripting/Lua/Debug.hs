{-# LANGUAGE ForeignFunctionInterface #-}
module Engine.Scripting.Lua.Debug
    ( getSourceInfo
    , SourceInfo(..)
    , getChunkSourceInfo
    , ChunkSourceInfo(..)
    , ChunkKind(..)
    ) where

import UPrelude
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString)
import qualified HsLua.Core as Lua
import HsLua.Core.Types (LuaE, State(..))

-- | Source information from Lua debug API
data SourceInfo = SourceInfo
    { siSource      ∷ String
    , siCurrentLine ∷ Int
    } deriving (Show, Eq)

-- | What KIND of chunk a stack frame belongs to.
--
--   Lua's @short_src@ is a display string, so it cannot answer this on
--   its own: a file-backed chunk and a caller-named one can produce
--   byte-identical text. The distinction lives in the first byte of
--   @lua_Debug.source@ (@\@@ for a file, @=@ for a literal name, neither
--   for a string chunk), which @cbits\/lua_debug.c@ classifies before the
--   prefix is discarded.
--
--   Constructor order mirrors the @LUA_SOURCE_*@ macros in
--   @cbits\/lua_debug.h@ (0..3); the two lists are kept in step by hand.
data ChunkKind
    = ChunkUnknownKind
      -- ^ Lua reported no source for the frame.
    | ChunkFile
      -- ^ Loaded from a file (@\@path@). 'csiSource' names a path.
    | ChunkNamed
      -- ^ A caller-supplied literal label (@=label@). 'csiSource' is
      --   that label verbatim — the debug console's entered code, for
      --   instance — and is NOT a path.
    | ChunkString
      -- ^ A string chunk, as 'HsLua.Core.loadstring' compiles.
      --   'csiSource' is Lua's own @[string \"...\"]@ rendering.
    deriving (Show, Eq)

-- | Source information from the Lua debug API, with the chunk kind
--   @short_src@ alone cannot carry.
data ChunkSourceInfo = ChunkSourceInfo
    { csiSource      ∷ String
    , csiCurrentLine ∷ Int
    , csiKind        ∷ ChunkKind
    } deriving (Show, Eq)

foreign import ccall unsafe "get_lua_caller_info"
    c_get_lua_caller_info ∷ Ptr () → CInt → CString → CInt → Ptr CInt
                          → Ptr CInt → IO CInt

-- | Size of the caller-supplied buffer for the short_src string
-- (lua_Debug.short_src is char[LUA_IDSIZE], LUA_IDSIZE is typically 60).
sourceBufSize ∷ Int
sourceBufSize = 256

-- | Decode the @LUA_SOURCE_*@ code @cbits\/lua_debug.c@ writes.
--   An unrecognised code is treated as unknown rather than guessed at.
chunkKindOfCode ∷ CInt → ChunkKind
chunkKindOfCode 1 = ChunkFile
chunkKindOfCode 2 = ChunkNamed
chunkKindOfCode 3 = ChunkString
chunkKindOfCode _ = ChunkUnknownKind

-- | Get source info for a given stack level, including the chunk kind.
-- Level 0 = current function, 1 = caller, 2 = caller's caller, etc.
getChunkSourceInfo ∷ Int → LuaE e (Maybe ChunkSourceInfo)
getChunkSourceInfo level = do
    State lPtr ← Lua.state
    Lua.liftIO $ allocaArray sourceBufSize $ \sourceBuf →
        alloca $ \linePtr →
        alloca $ \kindPtr → do
            result ← c_get_lua_caller_info lPtr (fromIntegral level) sourceBuf
                        (fromIntegral sourceBufSize) linePtr kindPtr
            if result ≡ 0
                then return Nothing
                else do
                    source ← peekCString sourceBuf
                    line ← peek linePtr
                    kind ← peek kindPtr
                    return $ Just ChunkSourceInfo
                        { csiSource = source
                        , csiCurrentLine = fromIntegral line
                        , csiKind = chunkKindOfCode kind
                        }

-- | Get source info for a given stack level.
-- Level 0 = current function, 1 = caller, 2 = caller's caller, etc.
--
-- Kind-blind view of 'getChunkSourceInfo', kept for callers that only
-- want the display text.
getSourceInfo ∷ Int → LuaE e (Maybe SourceInfo)
getSourceInfo level = fmap (fmap narrow) (getChunkSourceInfo level)
  where
    narrow csi = SourceInfo { siSource      = csiSource csi
                            , siCurrentLine = csiCurrentLine csi }
