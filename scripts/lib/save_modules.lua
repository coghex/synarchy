-- Lua-side save module registry for save/load v2 Phase 5.
--
-- Modules with persistent state register themselves at init time:
--   saveModules.register("unit_ai", serializeFn, deserializeFn)
--
-- The Haskell save handler calls saveModules.serializeAll() to collect
-- a `{ name → blob }` table that gets stored in the SaveData. On load,
-- saveModules.deserializeAll(blobs) walks the registry and invokes
-- each module's deserializer with the matching blob.
--
-- Modules without persistent state don't need to register. Modules
-- registered without a blob in the saved data are skipped silently
-- (fresh state). Blobs whose name no longer matches a registered
-- module are skipped with a warn (def was removed between sessions).
--
-- Singleton via package.loaded so script reloads + multiple require()s
-- share the same registry.

local saveModules = package.loaded["scripts.lib.save_modules"] or {}
package.loaded["scripts.lib.save_modules"] = saveModules

saveModules.registry = saveModules.registry or {}

function saveModules.register(name, serializeFn, deserializeFn)
    if type(name) ~= "string" then
        error("saveModules.register: name must be a string")
    end
    saveModules.registry[name] = {
        serialize   = serializeFn,
        deserialize = deserializeFn,
    }
end

-- Called from Haskell at save time. Returns a table mapping module
-- name to its serialized blob (string). Serializers may return nil
-- or empty strings; both round-trip as empty entries.
function saveModules.serializeAll()
    local blobs = {}
    for name, fns in pairs(saveModules.registry) do
        if fns.serialize then
            local ok, blob = pcall(fns.serialize)
            if ok then
                blobs[name] = blob or ""
            else
                engine.logWarn("saveModules: " .. name
                    .. " serializer crashed: " .. tostring(blob))
                blobs[name] = ""
            end
        end
    end
    return blobs
end

-- Called from Haskell at load time. blobs is a table { name → string }
-- pulled out of the save file. Each registered module gets its blob
-- (or nil if absent). Unknown blob names are skipped with a warn.
function saveModules.deserializeAll(blobs)
    if not blobs then return end
    for name, blob in pairs(blobs) do
        local fns = saveModules.registry[name]
        if not fns then
            engine.logWarn("saveModules: no registered module named '"
                .. name .. "', blob skipped")
        elseif fns.deserialize then
            local ok, err = pcall(fns.deserialize, blob)
            if not ok then
                engine.logWarn("saveModules: " .. name
                    .. " deserializer crashed: " .. tostring(err))
            end
        end
    end
end

return saveModules
