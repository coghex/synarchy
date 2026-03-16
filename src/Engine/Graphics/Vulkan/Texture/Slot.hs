-- | Slot management for bindless texture array
-- Handles allocation, deallocation, and generation tracking
module Engine.Graphics.Vulkan.Texture.Slot
  ( TextureSlot(..)
  , SlotGeneration
  , TextureSlotAllocator(..)
  , createSlotAllocator
  , allocateSlot
  , freeSlot
  , isValidSlot
  , undefinedSlot
  ) where

import UPrelude
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap

-- | Generation counter to detect stale handles
type SlotGeneration = Word32

-- | A texture slot in the bindless array
data TextureSlot = TextureSlot
  { tsIndex      ∷ !Word32        -- ^ Index in the descriptor array
  , tsGeneration ∷ !SlotGeneration -- ^ Generation (increments on reuse)
  } deriving (Show, Eq, Ord)

-- | The undefined texture always lives at slot 0, generation 0
undefinedSlot ∷ TextureSlot
undefinedSlot = TextureSlot 0 0

-- | Manages texture slot allocation
data TextureSlotAllocator = TextureSlotAllocator
  { tsaMaxSlots    ∷ !Word32                    -- ^ Maximum slots available
  , tsaFreeSlots   ∷ !IntSet.IntSet             -- ^ Set of free slot indices
  , tsaGenerations ∷ !(IntMap.IntMap SlotGeneration) -- ^ Current generation per slot
  , tsaNextSlot    ∷ !Word32                    -- ^ Next slot to allocate if no free slots
  } deriving (Show)

-- | Create a new slot allocator
-- Slot 0 is reserved for the undefined texture
createSlotAllocator ∷ Word32 → TextureSlotAllocator
createSlotAllocator maxSlots = TextureSlotAllocator
  { tsaMaxSlots    = maxSlots
  , tsaFreeSlots   = IntSet.empty  -- No free slots initially (none allocated yet)
  , tsaGenerations = IntMap.singleton 0 0  -- Slot 0 starts at generation 0
  , tsaNextSlot    = 1  -- Start allocating from slot 1 (0 is undefined)
  }

-- | Allocate a new texture slot
-- Returns Nothing if no slots available
allocateSlot ∷ TextureSlotAllocator → Maybe (TextureSlot, TextureSlotAllocator)
allocateSlot alloc
  | not (IntSet.null (tsaFreeSlots alloc)) =
      let slotIdx = IntSet.findMin (tsaFreeSlots alloc)
          newFreeSlots = IntSet.delete slotIdx (tsaFreeSlots alloc)
          currentGen = IntMap.findWithDefault 0 slotIdx (tsaGenerations alloc)
          newGen = currentGen + 1
          newGenerations = IntMap.insert slotIdx newGen (tsaGenerations alloc)
          slot = TextureSlot (fromIntegral slotIdx) newGen
          newAlloc = alloc
            { tsaFreeSlots   = newFreeSlots
            , tsaGenerations = newGenerations
            }
      in Just (slot, newAlloc)
  
  | tsaNextSlot alloc < tsaMaxSlots alloc =
      let slotIdx = tsaNextSlot alloc
          newGenerations = IntMap.insert (fromIntegral slotIdx) 0 (tsaGenerations alloc)
          slot = TextureSlot slotIdx 0
          newAlloc = alloc
            { tsaNextSlot    = slotIdx + 1
            , tsaGenerations = newGenerations
            }
      in Just (slot, newAlloc)
  
  | otherwise = Nothing

-- | Free a texture slot for reuse
-- The slot can be reallocated later with an incremented generation
freeSlot ∷ TextureSlot → TextureSlotAllocator → TextureSlotAllocator
freeSlot slot alloc
  | tsIndex slot ≡ 0 = alloc              -- slot 0 is reserved for undefined texture
  | not (isValidSlot slot alloc) = alloc  -- stale handle (generation mismatch)
  | otherwise = alloc
      { tsaFreeSlots = IntSet.insert (fromIntegral $ tsIndex slot) (tsaFreeSlots alloc)
      }

-- | Check if a slot handle is still valid (generation matches)
isValidSlot ∷ TextureSlot → TextureSlotAllocator → Bool
isValidSlot slot alloc =
  case IntMap.lookup (fromIntegral $ tsIndex slot) (tsaGenerations alloc) of
    Just gen → gen ≡ tsGeneration slot
    Nothing  → False
