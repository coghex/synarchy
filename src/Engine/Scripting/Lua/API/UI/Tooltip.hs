{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for the tooltip subsystem. Four entry points:
--
--   * 'UI.setTooltip(handle, text)' — attach a plain-text tooltip.
--   * 'UI.setTooltipRich(handle, contentTable)' — attach a tooltip
--     with optional text + a sprite row (static or animated).
--   * 'UI.clearTooltip(handle)' — remove any tooltip on the element.
--   * 'UI.setTooltipStyle(styleTable)' — configure global look (font,
--     box-texture set, colors, padding, dwell delay, mouse offset).
module Engine.Scripting.Lua.API.UI.Tooltip
  ( uiSetTooltipFn
  , uiSetTooltipRichFn
  , uiClearTooltipFn
  , uiSetTooltipStyleFn
  , uiLockTooltipFn
  , uiUnlockTooltipFn
  , uiToggleTooltipLockFn
  , uiIsTooltipLockedFn
  ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Asset.Handle (FontHandle(..), TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import UI.Types
import UI.Manager (setElementTooltip, clearElementTooltip)
import UI.Tooltip (setTooltipStyle, lockActiveTooltip, clearTooltipLock
                  , toggleTooltipLock, isTooltipLocked)

------------------------------------------------------------
-- UI.setTooltip(handle, text)
------------------------------------------------------------

uiSetTooltipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTooltipFn env = do
    elemArg ← Lua.tointeger 1
    textArg ← Lua.tostring  2
    case (elemArg, textArg) of
        (Just e, Just txtBS) → do
            let elemH  = ElementHandle (fromIntegral e)
                txt    = TE.decodeUtf8Lenient txtBS
                content = TooltipContent
                  { ttText     = Just txt
                  , ttHint     = Nothing
                  , ttSprites  = []
                  , ttMaxWidth = Nothing
                  }
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementTooltip elemH content mgr, ())
        _ → pure ()
    return 0

------------------------------------------------------------
-- UI.clearTooltip(handle)
------------------------------------------------------------

uiClearTooltipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiClearTooltipFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e →
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (clearElementTooltip (ElementHandle (fromIntegral e)) mgr, ())
        Nothing → pure ()
    return 0

------------------------------------------------------------
-- UI.setTooltipRich(handle, contentTable)
--
-- contentTable schema:
--   { text     = "optional string",
--     maxWidth = optional number,
--     sprites  = {
--       { texture = N,                  w = N, h = N },           -- static
--       { frames  = {T1, T2, ...},  frameMs = N, w = N, h = N },  -- animated
--     }
--   }
--
-- A sprite with both 'texture' and 'frames' uses 'frames'.
------------------------------------------------------------

uiSetTooltipRichFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTooltipRichFn env = do
    elemArg ← Lua.tointeger 1
    isTab   ← Lua.istable 2
    case (elemArg, isTab) of
        (Just e, True) → do
            -- 'Lua.nth N' counts from the TOP (StackIndex = -N), so with
            -- two args on the stack 'Lua.nth 1' is the top — i.e. the
            -- second (content) argument. Using positive index 2 directly
            -- (counting from the bottom) would also work; 'Lua.nth 1'
            -- mirrors how the rest of this codebase addresses the most-
            -- recently-pushed value.
            let contentIdx = Lua.nth 1
            mText    ← getOptString contentIdx "text"
            mHint    ← getOptString contentIdx "hint"
            mMaxW    ← getOptNumber contentIdx "maxWidth"
            sprites  ← readSprites contentIdx
            let elemH = ElementHandle (fromIntegral e)
                content = TooltipContent
                  { ttText     = mText
                  , ttHint     = mHint
                  , ttSprites  = sprites
                  , ttMaxWidth = realToFrac <$> mMaxW
                  }
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementTooltip elemH content mgr, ())
        _ → pure ()
    return 0

readSprites ∷ Lua.StackIndex → Lua.LuaE Lua.Exception [TooltipSprite]
readSprites contentIdx = do
    _ ← Lua.getfield contentIdx "sprites"
    isT ← Lua.istable Lua.top
    if not isT
      then do
          Lua.pop 1
          return []
      else do
          n ← Lua.rawlen Lua.top
          let go i acc
                | i > fromIntegral n = return (reverse acc)
                | otherwise = do
                    _ ← Lua.rawgeti Lua.top (fromIntegral i)
                    isS ← Lua.istable Lua.top
                    s ← if not isS then return Nothing else readSprite Lua.top
                    Lua.pop 1
                    case s of
                      Just x  → go (i + 1) (x : acc)
                      Nothing → go (i + 1) acc
          xs ← go (1 ∷ Int) []
          Lua.pop 1
          return xs

readSprite ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe TooltipSprite)
readSprite idx = do
    mTex     ← getOptInt    idx "texture"
    mFrames  ← readFrameList idx "frames"
    mFrameMs ← getOptInt    idx "frameMs"
    mW       ← getOptNumber idx "w"
    mH       ← getOptNumber idx "h"
    let frames = case mFrames of
            Just fs | not (V.null fs) → fs
            _ → case mTex of
                    Just t  → V.singleton (TextureHandle t)
                    Nothing → V.empty
        w = maybe 32 realToFrac mW
        h = maybe 32 realToFrac mH
        durMs = max 1 (fromIntegral (maybe 100 id mFrameMs))
    if V.null frames
      then return Nothing
      else return $ Just TooltipSprite
             { tsFrames     = frames
             , tsFrameDurMs = durMs
             , tsSize       = (w, h)
             }

readFrameList ∷ Lua.StackIndex → Lua.Name
              → Lua.LuaE Lua.Exception (Maybe (V.Vector TextureHandle))
readFrameList tbl name = do
    _ ← Lua.getfield tbl name
    isT ← Lua.istable Lua.top
    if not isT
      then do
          Lua.pop 1
          return Nothing
      else do
          n ← Lua.rawlen Lua.top
          let go i acc
                | i > fromIntegral n = return (reverse acc)
                | otherwise = do
                    _ ← Lua.rawgeti Lua.top (fromIntegral i)
                    mInt ← Lua.tointeger Lua.top
                    Lua.pop 1
                    case mInt of
                      Just k  → go (i + 1)
                                  (TextureHandle (fromIntegral k) : acc)
                      Nothing → go (i + 1) acc
          xs ← go (1 ∷ Int) []
          Lua.pop 1
          return (Just (V.fromList xs))

------------------------------------------------------------
-- UI.setTooltipStyle(styleTable)
--
-- All fields are optional; omitted fields keep their previous value.
------------------------------------------------------------

uiSetTooltipStyleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTooltipStyleFn env = do
    isTab ← Lua.istable 1
    if not isTab
      then return 0
      else do
          let styleIdx = Lua.nth 1
          mFont      ← getOptInt    styleIdx "font"
          mFontSize  ← getOptNumber styleIdx "fontSize"
          mPadding   ← getOptNumber styleIdx "padding"
          mBoxTex    ← getOptInt    styleIdx "boxTextures"
          mBoxTile   ← getOptNumber styleIdx "boxTileSize"
          mOffX      ← getOptNumber styleIdx "mouseOffsetX"
          mOffY      ← getOptNumber styleIdx "mouseOffsetY"
          mDwellMs   ← getOptNumber styleIdx "dwellMs"
          mHintDelay ← getOptNumber styleIdx "hintDelayMs"
          mSpriteGap ← getOptNumber styleIdx "spriteGap"
          mTextCol   ← readColor styleIdx "textColor"
          mBgCol     ← readColor styleIdx "bgColor"
          mHintSize  ← getOptNumber styleIdx "hintFontSize"
          mHintCol   ← readColor    styleIdx "hintColor"
          mSepCol    ← readColor    styleIdx "separatorColor"
          mSepThick  ← getOptNumber styleIdx "separatorThickness"
          mSepTex    ← getOptInt    styleIdx "separatorTexture"
          Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
              let cur = ttsStyle (upmTooltip mgr)
                  new = cur
                    { tsFont        = maybe (tsFont cur)
                                          (FontHandle . fromIntegral) mFont
                    , tsFontSize    = maybe (tsFontSize cur) realToFrac mFontSize
                    , tsPadding     = maybe (tsPadding cur) realToFrac mPadding
                    , tsBoxTextures = maybe (tsBoxTextures cur)
                                          (BoxTextureHandle . fromIntegral) mBoxTex
                    , tsBoxTileSize = maybe (tsBoxTileSize cur) realToFrac mBoxTile
                    , tsMouseOffsetX = maybe (tsMouseOffsetX cur) realToFrac mOffX
                    , tsMouseOffsetY = maybe (tsMouseOffsetY cur) realToFrac mOffY
                    , tsDwellMs     = maybe (tsDwellMs cur) realToFrac mDwellMs
                    , tsHintDelayMs = maybe (tsHintDelayMs cur) realToFrac mHintDelay
                    , tsSpriteGap   = maybe (tsSpriteGap cur) realToFrac mSpriteGap
                    , tsTextColor   = maybe (tsTextColor cur) id mTextCol
                    , tsBgColor     = maybe (tsBgColor cur) id mBgCol
                    , tsHintFontSize = maybe (tsHintFontSize cur) realToFrac mHintSize
                    , tsHintColor    = maybe (tsHintColor cur) id mHintCol
                    , tsSeparatorColor = maybe (tsSeparatorColor cur) id mSepCol
                    , tsSeparatorThickness = maybe (tsSeparatorThickness cur)
                                              realToFrac mSepThick
                    , tsSeparatorTexture = maybe (tsSeparatorTexture cur)
                                            (TextureHandle . fromIntegral) mSepTex
                    }
              in (setTooltipStyle new mgr, ())
          return 0

readColor ∷ Lua.StackIndex → Lua.Name
          → Lua.LuaE Lua.Exception (Maybe (Float, Float, Float, Float))
readColor tbl name = do
    _ ← Lua.getfield tbl name
    isT ← Lua.istable Lua.top
    if not isT
      then do
          Lua.pop 1
          return Nothing
      else do
          let readN i = do
                _ ← Lua.rawgeti Lua.top (fromIntegral (i ∷ Int))
                mn ← Lua.tonumber Lua.top
                Lua.pop 1
                return $ case mn of
                  Just (Lua.Number n) → realToFrac n
                  Nothing → 1.0 ∷ Float
          r ← readN 1
          g ← readN 2
          b ← readN 3
          a ← readN 4
          Lua.pop 1
          return (Just (r, g, b, a))

------------------------------------------------------------
-- Generic table-field helpers
------------------------------------------------------------

getOptInt ∷ Lua.StackIndex → Lua.Name → Lua.LuaE Lua.Exception (Maybe Int)
getOptInt tbl name = do
    _ ← Lua.getfield tbl name
    mi ← Lua.tointeger Lua.top
    Lua.pop 1
    return (fromIntegral <$> mi)

getOptNumber ∷ Lua.StackIndex → Lua.Name → Lua.LuaE Lua.Exception (Maybe Double)
getOptNumber tbl name = do
    _ ← Lua.getfield tbl name
    mn ← Lua.tonumber Lua.top
    Lua.pop 1
    return $ case mn of
        Just (Lua.Number n) → Just n
        Nothing → Nothing

getOptString ∷ Lua.StackIndex → Lua.Name → Lua.LuaE Lua.Exception (Maybe Text)
getOptString tbl name = do
    _ ← Lua.getfield tbl name
    ms ← Lua.tostring Lua.top
    Lua.pop 1
    return (TE.decodeUtf8Lenient <$> ms)

------------------------------------------------------------
-- Lock control
------------------------------------------------------------

-- | UI.lockTooltip() — freeze the currently-shown tooltip in place.
--   No-op if no tooltip is visible.
uiLockTooltipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiLockTooltipFn env = do
    Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
        (lockActiveTooltip mgr, ())
    return 0

-- | UI.unlockTooltip() — release the lock and hide the tooltip.
uiUnlockTooltipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiUnlockTooltipFn env = do
    Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
        (clearTooltipLock mgr, ())
    return 0

-- | UI.toggleTooltipLock() — lock if showing+unlocked, unlock+hide
--   otherwise.
uiToggleTooltipLockFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiToggleTooltipLockFn env = do
    Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
        (toggleTooltipLock mgr, ())
    return 0

-- | UI.isTooltipLocked() -> boolean
uiIsTooltipLockedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiIsTooltipLockedFn env = do
    mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
    Lua.pushboolean (isTooltipLocked mgr)
    return 1
