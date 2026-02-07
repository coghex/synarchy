-- Checkbox UI component
local checkbox = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local CHECKBOX_CALLBACK = "onCheckboxClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local checkboxes = {}
local nextId = 1

local texChecked = nil
local texUnchecked = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function checkbox.init()
    if assetsLoaded then return end
    
    texChecked = engine.loadTexture("assets/textures/ui/checkboxchecked.png")
    texUnchecked = engine.loadTexture("assets/textures/ui/checkboxunchecked.png")
    
    assetsLoaded = true
    engine.logDebug("Checkbox module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function checkbox.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or 1.0
    local size = math.floor((params.size or 48) * uiscale)
    
    local cb = {
        id = id,
        name = params.name or ("checkbox_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        size = size,
        page = params.page,
        parent = params.parent,
        spriteId = nil,
        checked = params.default or false,
        onChange = params.onChange,  -- optional callback function(checked, id, name)
    }
    
    local tex = cb.checked and texChecked or texUnchecked
    cb.spriteId = UI.newSprite(
        cb.name .. "_sprite",
        cb.size,
        cb.size,
        tex,
        1.0, 1.0, 1.0, 1.0,
        cb.page
    )
    
    if cb.parent then
        UI.addChild(cb.parent, cb.spriteId, cb.x, cb.y)
    elseif cb.page then
        UI.addToPage(cb.page, cb.spriteId, cb.x, cb.y)
    end
    
    UI.setClickable(cb.spriteId, true)
    UI.setOnClick(cb.spriteId, CHECKBOX_CALLBACK)
    
    checkboxes[id] = cb
    
    engine.logDebug("Checkbox created: " .. cb.name .. " (checked=" .. tostring(cb.checked) .. ")")
    
    return id
end

function checkbox.destroy(id)
    local cb = checkboxes[id]
    if not cb then return end
    
    checkboxes[id] = nil
    engine.logInfo("Checkbox destroyed: " .. cb.name)
end

function checkbox.destroyAll()
    for id, cb in pairs(checkboxes) do
        engine.logDebug("Checkbox destroyed: " .. cb.name)
    end
    checkboxes = {}
    nextId = 1
end

-----------------------------------------------------------
-- Value Access
-----------------------------------------------------------

function checkbox.isChecked(id)
    local cb = checkboxes[id]
    if not cb then return false end
    return cb.checked
end

function checkbox.setChecked(id, checked)
    local cb = checkboxes[id]
    if not cb then return end
    
    cb.checked = checked
    
    local tex = cb.checked and texChecked or texUnchecked
    UI.setSpriteTexture(cb.spriteId, tex)
end

function checkbox.setVisible(id, visible)
    local cb = checkboxes[id]
    if not cb then return end
    
    if cb.spriteId then UI.setVisible(cb.spriteId, visible) end
end

function checkbox.toggle(id)
    local cb = checkboxes[id]
    if not cb then return false end
    
    cb.checked = not cb.checked
    
    local tex = cb.checked and texChecked or texUnchecked
    UI.setSpriteTexture(cb.spriteId, tex)
    
    engine.logDebug("Checkbox toggled: " .. cb.name .. " = " .. tostring(cb.checked))
    
    -- Fire optional callback
    if cb.onChange then
        cb.onChange(cb.checked, id, cb.name)
    end
    
    return cb.checked
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function checkbox.findByElementHandle(elemHandle)
    for id, cb in pairs(checkboxes) do
        if cb.spriteId == elemHandle then
            return id
        end
    end
    return nil
end

function checkbox.handleClickByElement(elemHandle)
    local id = checkbox.findByElementHandle(elemHandle)
    if id then
        checkbox.toggle(id)
        return true
    end
    return false
end

function checkbox.isCheckboxElement(elemHandle)
    return checkbox.findByElementHandle(elemHandle) ~= nil
end

function checkbox.handleCallback(callbackName, elemHandle)
    if callbackName == CHECKBOX_CALLBACK and elemHandle then
        return checkbox.handleClickByElement(elemHandle)
    end
    return false
end

function checkbox.isCheckboxCallback(callbackName)
    if not callbackName then return false end
    return callbackName == CHECKBOX_CALLBACK
end

function checkbox.onHoverEnter(id)
end
function checkbox.onHoverLeave(id)
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function checkbox.getElementHandle(id)
    local cb = checkboxes[id]
    if not cb then return nil end
    return cb.spriteId
end

function checkbox.getName(id)
    local cb = checkboxes[id]
    if not cb then return nil end
    return cb.name
end

function checkbox.getSize(id)
    local cb = checkboxes[id]
    if not cb then return 0, 0 end
    return cb.size, cb.size
end

return checkbox
