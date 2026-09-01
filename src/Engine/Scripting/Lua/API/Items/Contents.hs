{-# LANGUAGE Strict #-}
-- | Nested item-container CONTENTS, shared by the two verbs the
--   container-window stack (#1238, epic #1013 phase 1) renders an
--   item-container level from: @unit.getItemContents@ (LIVE, a unit's
--   own carried/equipped/worn container) and
--   @building.getRememberedItemContents@ (REMEMBERED, a container the
--   player last saw inside a storage building).
--
--   The two answer about different worlds — one reads a live
--   'Unit.Types.UnitInstance', the other a frozen
--   'Building.Knowledge.ContainerRecord' — but they must PRESENT
--   identically, because one Lua level renderer draws both. That is
--   what this module is: the row shape and the descent rule, written
--   once.
--
--   __Rows are grouped by def name, and deliberately coarsely.__ A kit
--   holds a handful of types, so ten bandages read better as one row of
--   ten than as ten rows; the finer stack key
--   @scripts\/ui\/item_list.lua@ uses for a unit inventory or a cargo
--   hold would re-split them by quality/condition/fill and is
--   explicitly not applied here (the Lua host passes @preGrouped@).
--   'cgInstanceId' is the group's REPRESENTATIVE instance — the same
--   #67 convention the shared widget documents for its own row actions
--   — which is what lets \"open this row's contents\" name an exact
--   container.
--
--   __The descent is by exact instance identity, never by def name.__
--   'resolveContainedItem' walks a path of 'iiInstanceId's from a
--   starting item list down through 'iiContents', so two same-def kits
--   nested inside one toolbox can never show each other's contents,
--   and a path that no longer resolves answers 'Nothing' rather than
--   silently retargeting a sibling.
module Engine.Scripting.Lua.API.Items.Contents
    ( resolveContainedItem
    , pushGroupedContents
    , readInstanceIdPath
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Util (isDenseArray)
import Item.Types
    ( ItemDef(..), ItemInstance(..), ItemManager, lookupItemDef
    , itemTotalWeight )

-- | Walk a path of instance ids from @items@ down through nested
--   'iiContents'. The first id selects among @items@ themselves; each
--   later id selects among the previous item's contents.
--
--   An EMPTY path answers 'Nothing' — there is no item to name — which
--   is what keeps \"no path given\" a caller-side decision rather than
--   an accidental identity for the container itself.
resolveContainedItem ∷ [Word64] → [ItemInstance] → Maybe ItemInstance
resolveContainedItem [] _ = Nothing
resolveContainedItem (iid : rest) items =
    case [ i | i ← items, iiInstanceId i ≡ iid ] of
        []      → Nothing
        (it : _) → case rest of
            [] → Just it
            _  → resolveContainedItem rest (iiContents it)

-- | One rendered row: a def name plus the aggregate of every instance
--   of that def in the container, and the representative instance the
--   row's own actions target.
data ContentGroup = ContentGroup
    { cgCount      ∷ !Int
    , cgFill       ∷ !Float
    , cgCondition  ∷ !Float
    , cgWeight     ∷ !Float
    , cgInstanceId ∷ !Word64
    }

-- | Push a fresh table of grouped rows for @contents@ onto the stack.
--
--   Grouping is by def name only. The per-item values kept are the
--   REPRESENTATIVE's, not a sum: items sharing a def in a kit are
--   interchangeable for everything a row presents (consumables carry no
--   fill or condition spread, and a bottle is count 1), so
--   @weight × count@ is the exact total. @weight@ is the recursive
--   'itemTotalWeight' — empty case plus fill plus nested contents — so
--   a stocked kit inside a kit reads its real mass rather than its def
--   weight.
pushGroupedContents ∷ ItemManager → [ItemInstance] → Lua.LuaE Lua.Exception ()
pushGroupedContents itemMgr contents = do
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] grouped) $ \(idx, (dname, g)) → do
        let mDef = lookupItemDef dname itemMgr
            disp = maybe dname idDisplayName mDef
            cat  = maybe "Misc" idCategory mDef
            knd  = maybe "" idKind mDef
            -- The UI-policy handle (#2075): a container row's icon.
            tex  = maybe (-1) (\d → let TextureHandle t = idIconTexture d in t) mDef
        Lua.newtable
        pushText "defName" dname
        pushText "displayName" disp
        pushText "category" cat
        -- The one signal the UI branches on to offer a nested level:
        -- the same @kind == "container"@ test
        -- @scripts/unit_info_v2_context_menu.lua@ already applies to a
        -- unit's own inventory rows, so one rule decides everywhere.
        pushText "kind" knd
        pushInt "count" (fromIntegral (cgCount g))
        pushNum "weight" (realToFrac (cgWeight g))
        pushInt "iconTex" (fromIntegral tex)
        pushNum "fill" (realToFrac (cgFill g))
        pushNum "condition" (realToFrac (cgCondition g))
        -- The group's representative (#67). A row action — here, only
        -- "open this container's contents" — targets THIS instance,
        -- never the first item that happens to share the def name.
        pushInt "instanceId" (fromIntegral (cgInstanceId g))
        Lua.rawseti (-2) (fromIntegral idx)
  where
    grouped = HM.toList $ HM.fromListWith merge
        [ ( iiDefName i
          , ContentGroup
              { cgCount      = 1
              , cgFill       = iiCurrentFill i
              , cgCondition  = iiCondition i
              , cgWeight     = itemTotalWeight itemMgr i
              , cgInstanceId = iiInstanceId i
              } )
        | i ← contents ]
    -- 'HM.fromListWith' applies @f new old@, so the representative is
    -- the LAST member enumerated — count is the only field that
    -- accumulates, exactly as this grouping has always behaved.
    merge new old = new { cgCount = cgCount new + cgCount old }

pushText ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
pushText key val = Lua.pushstring (TE.encodeUtf8 val) >> Lua.setfield (-2) key

pushInt ∷ Lua.Name → Lua.Integer → Lua.LuaE Lua.Exception ()
pushInt key val = Lua.pushinteger val >> Lua.setfield (-2) key

pushNum ∷ Lua.Name → Double → Lua.LuaE Lua.Exception ()
pushNum key val = Lua.pushnumber (Lua.Number val) >> Lua.setfield (-2) key

-- | Read an optional descent path at the given stack index.
--
--   @Just []@ = the argument is absent or nil (\"no descent\"); a
--   'Just' of ids = a dense one-based array of non-negative integers;
--   'Nothing' = a MALFORMED argument, which every caller answers nil to
--   rather than descending a prefix of it. That distinction is
--   load-bearing: silently truncating a path would open a DIFFERENT
--   container's window, which is exactly the confusion the exact-
--   instance descent exists to prevent. 'isDenseArray' rejects the
--   holes 'Lua.rawlen''s border can otherwise hide.
readInstanceIdPath ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe [Word64])
readInstanceIdPath idx = do
    ty ← Lua.ltype idx
    case ty of
        Lua.TypeNil  → pure (Just [])
        Lua.TypeNone → pure (Just [])
        Lua.TypeTable → do
            dense ← isDenseArray idx
            if not dense then pure Nothing else do
                n ← Lua.rawlen idx
                let go i acc
                      | i > fromIntegral n = pure (Just (reverse acc))
                      | otherwise = do
                          entryTy ← Lua.rawgeti idx i
                          mInt ← if entryTy ≢ Lua.TypeNumber
                                   then pure Nothing
                                   else Lua.tointeger (-1)
                          Lua.pop 1
                          case mInt of
                              Just v | v ≥ 0 → go (i + 1) (fromIntegral v : acc)
                              _              → pure Nothing
                go 1 []
        _ → pure Nothing
