-- Shell module for debug console
local shell = {}

-- State
local visible = false
local focusId = nil
local inputBuffer = ""

-- Textures (loaded on init)
local texBox = nil
local texBoxN = nil
local texBoxS = nil
local texBoxE = nil
local texBoxW = nil
local texBoxNE = nil
local texBoxNW = nil
local texBoxSE = nil
local texBoxSW = nil

-- UI object IDs for the 9-box
local objBox = nil
local objBoxN = nil
local objBoxS = nil
local objBoxE = nil
local objBoxW = nil
local objBoxNE = nil
local objBoxNW = nil
local objBoxSE = nil
local objBoxSW = nil

-- Font
local fontHandle = nil

-- Configuration
local tileSize = 64        -- tile size in pixels
local middleWidth = 400    -- middle section width in pixels
local shellLayer = 10

-- Margins from edges (in pixels)
local marginLeft = 40
local marginBottom = 40

function shell.init(font)
    fontHandle = font
    
    -- Load all 9 box textures
    texBox = engine.loadTexture("assets/textures/box/box.png")
    texBoxN = engine.loadTexture("assets/textures/box/boxn.png")
    texBoxS = engine.loadTexture("assets/textures/box/boxs.png")
    texBoxE = engine.loadTexture("assets/textures/box/boxe.png")
    texBoxW = engine.loadTexture("assets/textures/box/boxw.png")
    texBoxNE = engine.loadTexture("assets/textures/box/boxne.png")
    texBoxNW = engine.loadTexture("assets/textures/box/boxnw.png")
    texBoxSE = engine.loadTexture("assets/textures/box/boxse.png")
    texBoxSW = engine.loadTexture("assets/textures/box/boxsw.png")
    
    -- Register as focusable (accepts text, tab index 0)
    focusId = engine.registerFocusable(true, 0)
end

function shell.toggle()
    if visible then
        shell.hide()
    else
        shell.show()
    end
end

function shell.show()
    visible = true
    engine.requestFocus(focusId)
    
    -- Use framebuffer size for pixel-accurate positioning
    local w, h = engine.getFramebufferSize()
    
    -- Calculate total box dimensions
    local totalHeight = tileSize * 3
    
    -- Position at bottom-left of screen
    local baseX = marginLeft
    local baseY = h - marginBottom - totalHeight
    
    -- Spawn tiles if they don't exist
    -- Note: spawnSprite expects CENTER position, so we add half the size
    if not objBoxNW then
        -- Top row
        local row0Y = baseY + tileSize / 2
        objBoxNW = engine.spawnSprite(baseX + tileSize / 2, row0Y, tileSize, tileSize, texBoxNW, shellLayer)
        objBoxN  = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row0Y, middleWidth, tileSize, texBoxN, shellLayer)
        objBoxNE = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row0Y, tileSize, tileSize, texBoxNE, shellLayer)
        
        -- Middle row
        local row1Y = baseY + tileSize + tileSize / 2
        objBoxW  = engine.spawnSprite(baseX + tileSize / 2, row1Y, tileSize, tileSize, texBoxW, shellLayer)
        objBox   = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row1Y, middleWidth, tileSize, texBox, shellLayer)
        objBoxE  = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row1Y, tileSize, tileSize, texBoxE, shellLayer)
        
        -- Bottom row
        local row2Y = baseY + tileSize * 2 + tileSize / 2
        objBoxSW = engine.spawnSprite(baseX + tileSize / 2, row2Y, tileSize, tileSize, texBoxSW, shellLayer)
        objBoxS  = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row2Y, middleWidth, tileSize, texBoxS, shellLayer)
        objBoxSE = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row2Y, tileSize, tileSize, texBoxSE, shellLayer)
    else
        -- Show existing tiles
        engine.setSpriteVisible(objBoxNW, true)
        engine.setSpriteVisible(objBoxN, true)
        engine.setSpriteVisible(objBoxNE, true)
        engine.setSpriteVisible(objBoxW, true)
        engine.setSpriteVisible(objBox, true)
        engine.setSpriteVisible(objBoxE, true)
        engine.setSpriteVisible(objBoxSW, true)
        engine.setSpriteVisible(objBoxS, true)
        engine.setSpriteVisible(objBoxSE, true)
    end
end

function shell.hide()
    visible = false
    engine.releaseFocus()
    
    if objBoxNW then engine.setSpriteVisible(objBoxNW, false) end
    if objBoxN then engine.setSpriteVisible(objBoxN, false) end
    if objBoxNE then engine.setSpriteVisible(objBoxNE, false) end
    if objBoxW then engine.setSpriteVisible(objBoxW, false) end
    if objBox then engine.setSpriteVisible(objBox, false) end
    if objBoxE then engine.setSpriteVisible(objBoxE, false) end
    if objBoxSW then engine.setSpriteVisible(objBoxSW, false) end
    if objBoxS then engine.setSpriteVisible(objBoxS, false) end
    if objBoxSE then engine.setSpriteVisible(objBoxSE, false) end
end

function shell.updateDisplay()
    if not visible then return end
    -- TODO: Update text display with inputBuffer
end

function shell.isVisible()
    return visible
end

function shell.getFocusId()
    return focusId
end

function shell.onChar(char)
    inputBuffer = inputBuffer .. char
    shell.updateDisplay()
end

function shell.onBackspace()
    if #inputBuffer > 0 then
        inputBuffer = string.sub(inputBuffer, 1, -2)
        shell.updateDisplay()
    end
end

function shell.onSubmit()
    -- TODO: Execute command from inputBuffer
    inputBuffer = ""
    shell.updateDisplay()
end

return shell
