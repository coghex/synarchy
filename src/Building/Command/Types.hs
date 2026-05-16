{-# LANGUAGE Strict, UnicodeSyntax #-}
module Building.Command.Types
    ( BuildingCommand(..)
    ) where

import UPrelude
import Building.Types (BuildingId(..))

data BuildingCommand
    = BuildingSpawn !BuildingId !Text !Int !Int !Int
        -- ^ pre-allocated id, defName, anchor gx, gy, gz.
        --   Placement validation is the caller's responsibility — the
        --   handler trusts these coords. (We do this in the Lua API:
        --   spawn checks canPlaceAt before enqueuing.)
    | BuildingDestroy !BuildingId
    deriving (Show)
