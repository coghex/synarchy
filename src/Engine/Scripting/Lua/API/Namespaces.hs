-- | The engine API namespaces "Engine.Scripting.Lua.API.registerLuaAPI"
--   installs as Lua globals, and which of them the in-game debug console
--   can reach (#1958).
--
--   Before this module existed, @setupShellSandbox@ carried its own
--   hand-maintained copy of the list and a comment asking the next author
--   to keep the two in sync. They drifted by sixteen namespaces, and the
--   console offered completions for every one of them while refusing to
--   execute them. The single declaration here is what the sandbox is
--   built from, so a namespace can no longer be exposed to completion
--   without being executable.
--
--   The declaration is still a list a human writes, so it can disagree
--   with the @setglobal@ calls it describes. That disagreement is what
--   @Test.Headless.Lua.ShellInput@'s \"engine API namespace
--   synchronisation\" group fails on: it derives the registered set
--   /live/ by diffing @_G@ across a real 'registerLuaAPI' call and
--   compares it against 'engineApiNamespaces' in both directions.
module Engine.Scripting.Lua.API.Namespaces
  ( engineApiNamespaces
  , consoleWithheldNamespaces
  , consoleExposedNamespaces
  ) where

import UPrelude
import qualified Data.ByteString.Char8 as BS

-- | Every Lua global that 'registerLuaAPI' installs or augments as an
--   engine API namespace, grouped by the @Register@ submodule that owns
--   it. Adding a @setglobal@ to one of those modules without adding its
--   name here fails the synchronisation spec.
--
--   @debug@ is the one entry that is /augmented/ rather than installed:
--   @Lua.openlibs@ has already created the stdlib table, so
--   "Engine.Scripting.Lua.API.Register.Debug" adds its verbs to the
--   existing one. It is a registered namespace all the same, and it is
--   the sole withheld one — see 'consoleWithheldNamespaces'.
engineApiNamespaces ∷ [BS.ByteString]
engineApiNamespaces =
    [ "engine"                                    -- Register.Engine
    , "debug"                                     -- Register.Debug
    , "input"                                     -- Register.Input
    , "UI"                                        -- Register.UI
    , "unit"                                      -- Register.Unit
    , "faction"                                   -- Register.Faction
    , "building"                                  -- Register.Building
    , "structure", "construction", "chop"         -- Register.Designation
    , "till", "plant"
    , "equipment", "substance", "infection"       -- Register.Equipment
    , "craft", "power", "repair"                  -- Register.Craft
    , "blood", "loot", "item"                     -- Register.Item
    , "world", "flora"                            -- Register.World
    , "camera", "combat", "injury", "thought"     -- Register.Camera
    ]

-- | Engine API namespaces deliberately kept out of the console sandbox,
--   each beside the reason it is withheld.
--
--   @debug@ is not the engine's own table: @Lua.openlibs@ created Lua's
--   stdlib @debug@ and "Engine.Scripting.Lua.API.Register.Debug" adds
--   engine verbs to it. Copying it into the sandbox would hand the
--   console @debug.setupvalue@, @debug.getupvalue@ and
--   @debug.setmetatable@, which reach straight past every restriction
--   the sandbox exists to impose — including the chunk's own @_ENV@
--   upvalue, which is the sandbox. The engine debug verbs remain
--   available to scripts and to the unsandboxed TCP console.
consoleWithheldNamespaces ∷ [(BS.ByteString, Text)]
consoleWithheldNamespaces =
    [ ( "debug"
      , "shares Lua's stdlib debug table, whose upvalue and metatable \
        \primitives would let console code escape the sandbox" )
    ]

-- | The engine API namespaces the in-game console can both complete and
--   execute: every registered namespace that is not withheld. This is
--   what @setupShellSandbox@ copies, so the exposed set and the
--   executable set are one derivation rather than two lists.
consoleExposedNamespaces ∷ [BS.ByteString]
consoleExposedNamespaces =
    filter (\n → n `notElem` map fst consoleWithheldNamespaces)
           engineApiNamespaces
