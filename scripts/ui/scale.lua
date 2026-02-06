-- UI Scaling utility
local scale = {}

-- Get current UI scale from engine
function scale.get()
    return engine.getUIScale()
end

-- Apply scale to a single value
function scale.apply(baseValue)
    return math.floor(baseValue * engine.getUIScale())
end

-- Apply scale to multiple values, returns a table with same keys
function scale.applyAll(baseValues)
    local uiscale = engine.getUIScale()
    local result = {}
    for k, v in pairs(baseValues) do
        result[k] = math.floor(v * uiscale)
    end
    return result
end

-- Apply scale with a specific scale value (not from engine)
function scale.applyWith(baseValue, uiscale)
    return math.floor(baseValue * uiscale)
end

-- Apply scale to multiple values with a specific scale value
function scale.applyAllWith(baseValues, uiscale)
    local result = {}
    for k, v in pairs(baseValues) do
        result[k] = math.floor(v * uiscale)
    end
    return result
end

return scale
