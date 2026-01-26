-- Shell module for in-game Lua console
local shell = {}

-- State
local visible = false
local initialized = false
local textures = {}
local sprites = {}

-- Configuration
local config = {
    width = 600,
    height = 300,
    borderSize = 32,
    baseX = -400,  -- Adjust based on your coordinate system
    baseY = -300,  -- Bottom of screen
    layer = 1000,  -- High layer to render on top
}

-- History
local history = {}
local historyIndex = 0
local maxHistory = 100

-- Initialize shell (call once)
function shell.init()
    if initialized then return end
    
    engine.logInfo("Shell: Initializing...")
    
    -- Load 9-patch textures
    textures.nw = engine.loadTexture("assets/textures/box/boxnw.png")
    textures.n  = engine.loadTexture("assets/textures/box/boxn.png")
    textures.ne = engine.loadTexture("assets/textures/box/boxne.png")
    textures.w  = engine.loadTexture("assets/textures/box/boxw.png")
    textures.c  = engine.loadTexture("assets/textures/box/box.png")
    textures.e  = engine.loadTexture("assets/textures/box/boxe.png")
    textures.sw = engine.loadTexture("assets/textures/box/boxsw.png")
    textures.s  = engine.loadTexture("assets/textures/box/boxs.png")
    textures.se = engine.loadTexture("assets/textures/box/boxse.png")
    
    -- Create the 9-patch sprites
    --shell.createBox()
    
    -- Start hidden
    shell.setVisible(false)
    
    initialized = true
    engine.logInfo("Shell: Initialized")
end

-- Create the 9-patch box sprites
function shell.createBox()
    local w = config.width
    local h = config.height
    local b = config.borderSize
    local x = config.baseX
    local y = config.baseY
    local layer = config.layer
    
    -- Calculate sizes
    local edgeW = w - (2 * b)  -- Horizontal edge width
    local edgeH = h - (2 * b)  -- Vertical edge height
    
    -- Bottom row (SW, S, SE)
    sprites.sw = engine.spawnSprite(x, y, b, b, textures.sw, layer)
    sprites.s  = engine.spawnSprite(x + b, y, edgeW, b, textures.s, layer)
    sprites.se = engine.spawnSprite(x + w - b, y, b, b, textures.se, layer)
    
    -- Middle row (W, C, E)
    sprites.w = engine.spawnSprite(x, y + b, b, edgeH, textures.w, layer)
    sprites.c = engine.spawnSprite(x + b, y + b, edgeW, edgeH, textures.c, layer)
    sprites.e = engine.spawnSprite(x + w - b, y + b, b, edgeH, textures.e, layer)
    
    -- Top row (NW, N, NE)
    sprites.nw = engine.spawnSprite(x, y + h - b, b, b, textures.nw, layer)
    sprites.n  = engine.spawnSprite(x + b, y + h - b, edgeW, b, textures.n, layer)
    sprites.ne = engine.spawnSprite(x + w - b, y + h - b, b, b, textures.ne, layer)
    
    engine.logInfo("Shell: Box created with 9 sprites")
end

-- Set visibility of all shell sprites
function shell.setVisible(vis)
    visible = vis
    if vis and sprites.c == nil then
        shell.createBox()
    end
    for name, id in pairs(sprites) do
        engine.setSpriteVisible(id, vis)
    end
    engine.logInfo("Shell: visibility = " .. tostring(vis))
end

-- Toggle shell visibility
function shell.toggle()
    -- Initialize on first toggle if needed
    if not initialized then
        shell.init()
    end
    
    shell.setVisible(not visible)
end

-- Execute a command
function shell.execute(command)
    if command == nil or command == "" then
        return
    end
    
    engine.logInfo("Shell: Executing: " .. command)
    
    -- Try to evaluate as expression first (for things like "1+1")
    local fn, err = load("return " .. command)
    if not fn then
        -- If that fails, try as statement
        fn, err = load(command)
    end
    
    if fn then
        local ok, result = pcall(fn)
        if ok then
            local output = tostring(result)
            engine.logInfo("Shell: Result: " .. output)
            shell.addHistory(command, output)
        else
            engine.logInfo("Shell: Error: " .. tostring(result))
            shell.addHistory(command, "Error: " .. tostring(result))
        end
    else
        engine.logInfo("Shell: Parse error: " .. tostring(err))
        shell.addHistory(command, "Parse error: " .. tostring(err))
    end
end

-- Add to history
function shell.addHistory(command, result)
    table.insert(history, {
        command = command,
        result = result,
        time = os.time()
    })
    
    -- Trim history if too long
    while #history > maxHistory do
        table.remove(history, 1)
    end
    
    -- Reset history navigation
    historyIndex = #history + 1
end

-- Navigate history up (older)
function shell.historyUp()
    if historyIndex > 1 then
        historyIndex = historyIndex - 1
        local entry = history[historyIndex]
        if entry then
            engine.logInfo("Shell: History[" .. historyIndex .. "]: " .. entry.command)
            -- TODO: Set input buffer to entry.command
        end
    end
end

-- Navigate history down (newer)
function shell.historyDown()
    if historyIndex <= #history then
        historyIndex = historyIndex + 1
        if historyIndex <= #history then
            local entry = history[historyIndex]
            engine.logInfo("Shell: History[" .. historyIndex .. "]: " .. entry.command)
            -- TODO: Set input buffer to entry.command
        else
            engine.logInfo("Shell: History: (current)")
            -- TODO: Clear input buffer
        end
    end
end

-- Return the module
return shell
