{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The canonical generated atlas-index DOCUMENT, built field by field
--   (#1259, TEX-3).
--
--   Shared because two owners read it: the index/schema owner corrupts
--   ONE field at a time to assert a specific rejection, and the
--   freshness owner parses the same canonical document to derive the
--   YAML facts its mode-selection cases perturb. One builder is what
--   keeps those two views of "the document the compiler emits" the same
--   document.
--
--   A support leaf: it imports production modules only, never a spec
--   owner.
module Test.Headless.Unit.Atlas.Document
    ( Field
    , obj
    , str
    , arr
    , directionEntry
    , idleFields
    , swingFields
    , indexWith
    , setField
    , dropField
    , indexWithout
    , goodIndex
    , parse
    ) where

import UPrelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Unit.Atlas.Index (AtlasLoadError, parseAtlasIndex)
import Unit.Atlas.Types (AtlasAnimation)

-- * The canonical index document
--
--   A two-animation index in exactly the shape @tools/pack_atlas.py@'s
--   `build_index_document` emits, built field by field so a test can
--   corrupt ONE thing and assert the specific rejection.

type Field = (Text, Text)

obj ∷ [Field] → Text
obj fs = "{" <> T.intercalate "," [ "\"" <> k <> "\":" <> v | (k, v) ← fs ] <> "}"

str ∷ Text → Text
str t = "\"" <> t <> "\""

arr ∷ [Text] → Text
arr xs = "[" <> T.intercalate "," xs <> "]"

directionEntry ∷ Text → Int → Int → Text
directionEntry d row n = obj
    [ ("direction", str d), ("row", tshow row), ("frame_count", tshow n) ]

-- | The @idle@ animation: five authored directions, mirroring on,
--   32x48 cells, four frames each.
idleFields ∷ [Field]
idleFields =
    [ ("name", str "idle")
    , ("storage_format", str "png")
    , ("atlas_path", str "assets/textures/units/acolyte/atlas/idle.png")
    -- Padded strides (#2076): 4 columns of (32+2) and 5 rows of (48+2).
    , ("atlas_width", "136"), ("atlas_height", "250")
    , ("cell_width", "32"), ("cell_height", "48"), ("cell_padding", "1")
    , ("columns", "4"), ("rows", "5")
    , ("flip", "true"), ("fps", "8"), ("loop", "true")
    , ("directions", arr
        [ directionEntry "south" 0 4
        , directionEntry "north-west" 1 4
        , directionEntry "north" 2 4
        , directionEntry "north-east" 3 4
        , directionEntry "east" 4 4 ])
    , ("source_digest", str "aaaa"), ("atlas_digest", str "bbbb")
    ]

-- | The @swing@ animation: eight directions, unequal counts, no mirror.
swingFields ∷ [Field]
swingFields =
    [ ("name", str "swing")
    , ("storage_format", str "png")
    , ("atlas_path", str "assets/textures/units/acolyte/atlas/swing.png")
    , ("atlas_width", "204"), ("atlas_height", "400")
    , ("cell_width", "32"), ("cell_height", "48"), ("cell_padding", "1")
    , ("columns", "6"), ("rows", "8")
    , ("flip", "false"), ("fps", "12"), ("loop", "false")
    , ("directions", arr
        [ directionEntry "south" 0 6
        , directionEntry "south-west" 1 2
        , directionEntry "west" 2 5
        , directionEntry "north-west" 3 1
        , directionEntry "north" 4 6
        , directionEntry "north-east" 5 3
        , directionEntry "east" 6 4
        , directionEntry "south-east" 7 2 ])
    , ("source_digest", str "cccc"), ("atlas_digest", str "dddd")
    ]

indexWith ∷ [Field] → [[Field]] → BL.ByteString
indexWith top anims = BLC.pack ∘ T.unpack ∘ obj $
    ([ ("schema_version", "2")
     , ("generator", str "tools/pack_atlas.py")
     , ("tool_version", "2")
     , ("digest_algorithm", str "sha256")
     , ("unit", str "acolyte")
     , ("direction_order", arr (map str
         [ "south", "south-west", "west", "north-west"
         , "north", "north-east", "east", "south-east" ]))
     ] `override` top)
    <> [("animations", arr (map obj anims))]

-- | Replace matching keys, keeping order — so a test can override one
--   top-level field without restating the document.
override ∷ [Field] → [Field] → [Field]
override base new =
    [ (k, maybe v id (lookup k new)) | (k, v) ← base ]
    <> [ f | f@(k, _) ← new, isNothing (lookup k base) ]

-- | Replace one animation field.
setField ∷ Text → Text → [Field] → [Field]
setField k v = map (\(k', v') → if k' ≡ k then (k', v) else (k', v'))

dropField ∷ Text → [Field] → [Field]
dropField k = filter ((≢ k) ∘ fst)

-- | The canonical document with one top-level field removed.
indexWithout ∷ Text → BL.ByteString
indexWithout field =
    BLC.pack ∘ T.unpack ∘ obj ∘ dropField field $
        [ ("schema_version", "2")
        , ("generator", str "tools/pack_atlas.py")
        , ("tool_version", "2")
        , ("digest_algorithm", str "sha256")
        , ("unit", str "acolyte")
        , ("direction_order", arr (map str
            [ "south", "south-west", "west", "north-west"
            , "north", "north-east", "east", "south-east" ]))
        , ("animations", arr [obj idleFields])
        ]

goodIndex ∷ BL.ByteString
goodIndex = indexWith [] [idleFields, swingFields]

parse ∷ BL.ByteString → Either AtlasLoadError [AtlasAnimation]
parse = parseAtlasIndex "acolyte" "assets/textures/units/acolyte/atlas/index.json"
