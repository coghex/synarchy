-- One authored `construction:` declaration -> the frame list
-- `structure.registerPackArt` takes (#2488).
--
-- Shared by scripts/structures.lua and scripts/wire.lua because the two
-- pack schemas differ everywhere EXCEPT here: a sequence is an ordered
-- list of image paths whatever block it was declared in, and two copies
-- of the rules below would be two chances to disagree about a malformed
-- one.
--
-- The rule is: SHAPE IS PRESERVED, never normalised. The engine owns
-- every judgement about a declaration — empty, sparse, escaping,
-- unloadable, too long, too short — and this module's job is to hand it
-- what the pack actually said, so its refusal names the real fault.

local M = {}

-- Turn a declared sequence into `{ {texture=, texHandle=}, ... }`.
--
-- nil in  -> nil out: the appearance declares no sequence, which is a
-- supported state and every shipped appearance's.
--
-- Anything that is NOT a table goes over unchanged, so the engine's own
-- reader refuses it as "not an array" rather than this module inventing
-- a shape and swapping that message for a vaguer one.
--
-- A table is rebuilt BY KEY, holes included. `engine.loadYaml` turns a
-- YAML null into a Lua nil, so
--
--     construction:
--       - a.png
--       -            # null
--       - c.png
--
-- arrives as a table with a HOLE at index 2 — and `ipairs` stops there,
-- which would hand the engine a dense ONE-frame list indistinguishable
-- from an authored one, silently dropping every later stage. Copying the
-- gap through is what lets the engine's density check see it and refuse
-- the pack.
--
-- An entry that is present but not a string becomes an empty frame
-- table, which the engine refuses by index rather than by silence.
function M.load(paths)
    if paths == nil then return nil end
    if type(paths) ~= "table" then return paths end
    -- A non-integer key means this is not a list at all; hand the table
    -- over as it is and let the engine say so.
    local maxKey = 0
    for k, _ in pairs(paths) do
        if type(k) ~= "number" or k < 1 or k % 1 ~= 0 then
            return paths
        end
        if k > maxKey then maxKey = k end
    end
    local frames = {}
    for i = 1, maxKey do
        local path = paths[i]
        if type(path) ~= "string" then
            -- Absent: leave the hole, so the engine sees the gap.
            -- Present but not a path: an entry it refuses by index.
            if path ~= nil then frames[i] = {} end
        elseif structure.isSafeArtPath
               and not structure.isSafeArtPath(path) then
            -- ASK BEFORE LOADING. `structure.isSafeArtPath` is the
            -- engine's own declaration rule, not a copy of it, and a
            -- path that fails it must not be queued for load at all --
            -- the declaration still goes over with no handle, and the
            -- catalogue refuses the pack naming the escape (its path
            -- check runs before its handle one).
            frames[i] = { texture = path }
        else
            frames[i] = { texture = path, texHandle = engine.loadTexture(path) }
        end
    end
    return frames
end

-- Which construction declaration an appearance uses. A VARIANT reads its
-- own override's and nothing else: inheriting the default's would build a
-- damaged wall out of the intact wall's frames, which requirement 1
-- forbids outright. Written as an explicit branch rather than the
-- `variant and over.construction or base.construction` idiom, which
-- silently falls back to the default whenever the override declares none
-- — exactly the inheritance being ruled out.
function M.declaredBy(variant, over, base)
    if variant then return over.construction end
    return base.construction
end

return M
