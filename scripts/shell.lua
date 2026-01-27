-- Shell module for debug console
local shell = {}

-- State
local visible = false
local focusId = nil
local inputBuffer = ""
local history = {}
local historyTextObjects = {}
local maxHistoryVisible = {}
local lineHeight = 40
local historyPadding = 10
local marginLeft = 40
local marginBottom = 40
local marginTop = 40

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

-- Text object for prompt
local objPrompt = nil
local objBufferText = nil

-- Font
local fontHandle = nil
local shellFont = nil

-- Configuration
local tileSize = 64        -- tile size in pixels
local middleWidth = 1200    -- middle section width in pixels
local shellLayer = 10

-- Margins from edges (in pixels)
local marginLeft = 40
local marginBottom = 40

function shell.init(font)
    fontHandle = font
    
    -- Load shell-specific font
    shellFont = engine.loadFont("assets/fonts/Cabal.ttf", 32)
    if shellFont then
        engine.logInfo("Shell font loaded: Cabal.ttf")
    else
        engine.logError("Failed to load shell font")
        shellFont = font  -- Fall back to passed font
    end
    
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
    
    engine.logInfo("Shell textures loaded")
    
    -- Register as focusable (accepts text, tab index 0)
    focusId = engine.registerFocusable(true, 0)
    engine.logInfo("Shell initialized with focusId: " .. tostring(focusId))
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
    
    shell.rebuildBox()
    shell.rebuildHistoryDisplay()
end

function shell.hide()
    visible = false
    engine.releaseFocus()
    
    if objBoxNW then engine.setVisible(objBoxNW, false) end
    if objBoxN then engine.setVisible(objBoxN, false) end
    if objBoxNE then engine.setVisible(objBoxNE, false) end
    if objBoxW then engine.setVisible(objBoxW, false) end
    if objBox then engine.setVisible(objBox, false) end
    if objBoxE then engine.setVisible(objBoxE, false) end
    if objBoxSW then engine.setVisible(objBoxSW, false) end
    if objBoxS then engine.setVisible(objBoxS, false) end
    if objBoxSE then engine.setVisible(objBoxSE, false) end
    if objPrompt then engine.setVisible(objPrompt, false) end
    if objBufferText then engine.setVisible(objBufferText, false) end
    for _, obj in ipairs(historyTextObjects) do
        engine.setVisible(obj, false)
    end
end

function shell.updateDisplay()
    if not visible then return end
    if objBufferText then
        engine.setText(objBufferText, inputBuffer)
    end
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
    if inputBuffer == "" then return end
    local cmd = string.lower(string.match(inputBuffer, "^%s*(%S+)") or "")
    if cmd == "help" then
        shell.addHistory(inputBuffer, shell.cmdHelp(), false)
    elseif cmd == "clear" then
        shell.addHistory(inputBuffer, "OK", false)
        shell.cmdClear()
    elseif cmd == "exit" or cmd == "quit" then
        shell.addHistory(inputBuffer, "OK", false)
        shell.cmdQuit()
    else
        local result, isError = engine.shellExecute(inputBuffer)
        
        -- Bare identifier returning nil is probably an undefined variable
        local trimmed = inputBuffer:match("^%s*(.-)%s*$")
        if result == "nil" and trimmed:match("^[%a_][%w_]*$") then
            result = "undefined: " .. trimmed
            isError = true
        end
        
        if result == nil or result == "" or result == "nil" then
            if isError then
                result = "ERROR"
            else
                result = "OK"
            end
        end
        shell.addHistory(inputBuffer, result, isError)
    end
    inputBuffer = ""
    shell.updateDisplay()
end

function shell.addHistory(command, result, isError)
    table.insert(history, {
        command = command,
        result = result,
        isError = isError
    })
    if visible then
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
    end
end

function shell.cmdHelp()
    return "Commands: help, clear, quit/exit\nOr enter Lua code to execute"
end

function shell.cmdClear()
    history = {}
    if visible then
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
    end
end

function shell.cmdQuit()
    -- TODO: Signal engine to quit
    engine.logInfo("Quit requested from shell")
end

function shell.rebuildHistoryDisplay()
    -- Destroy old text objects
    for _, obj in ipairs(historyTextObjects) do
        engine.setVisible(obj, false)
        -- TODO: engine.destroyText(obj) when available
    end
    historyTextObjects = {}
    
    if not visible then return end
    if #history == 0 then return end
    
    -- Get current layout info
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local baseX = marginLeft
    local textX = baseX + tileSize + historyPadding
    
    -- Calculate how many lines we can show
    local promptY = getPromptY()
    local availableHeight = promptY - marginTop - tileSize - historyPadding
    local maxLines = math.floor(availableHeight / lineHeight)
    
    -- Get the lines to display (most recent at bottom)
    local startIdx = math.max(1, #history - maxLines + 1)
    
    local y = promptY - lineHeight  -- Start just above prompt
    for i = #history, startIdx, -1 do
        local entry = history[i]
        
        -- Result line first (closer to prompt, lower on screen)
        if entry.result and entry.result ~= "" and entry.result ~= "nil" then
            local resultObj = engine.spawnText(textX + 20, y, shellFont, entry.result,"white", shellLayer)
            table.insert(historyTextObjects, resultObj)
            y = y - lineHeight
        end
        
        -- Command line (above result, higher on screen)
        local cmdText = "$> " .. entry.command
        local cmdObj = engine.spawnText(textX, y, shellFont, cmdText, "white", shellLayer)
        table.insert(historyTextObjects, cmdObj)
        y = y - lineHeight
        
        -- Stop if we've run out of space
        if y < marginTop + tileSize then
            break
        end
    end
end

-- Add this new function
function shell.rebuildBox()
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = calculateBoxHeight()
    
    -- Calculate middle section height (total - top edge - bottom edge)
    local middleHeight = boxHeight - tileSize * 2
    
    -- Position at bottom-left of screen
    local baseX = marginLeft
    local baseY = fbHeight - marginBottom - boxHeight
    
    -- Destroy old sprites if they exist
    if objBoxNW then
        -- For now, just hide them - we'll respawn
        -- TODO: Add engine.destroySprite() for proper cleanup
        engine.setVisible(objBoxNW, false)
        engine.setVisible(objBoxN, false)
        engine.setVisible(objBoxNE, false)
        engine.setVisible(objBoxW, false)
        engine.setVisible(objBox, false)
        engine.setVisible(objBoxE, false)
        engine.setVisible(objBoxSW, false)
        engine.setVisible(objBoxS, false)
        engine.setVisible(objBoxSE, false)
        engine.setVisible(objPrompt, false)
        engine.setVisible(objBufferText, false)
    end
    
    -- Top row (at the top of the box)
    local row0Y = baseY + tileSize / 2
    objBoxNW = engine.spawnSprite(baseX + tileSize / 2, row0Y, tileSize, tileSize, texBoxNW, shellLayer)
    objBoxN  = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row0Y, middleWidth, tileSize, texBoxN, shellLayer)
    objBoxNE = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row0Y, tileSize, tileSize, texBoxNE, shellLayer)
    
    -- Middle section (stretched to fill)
    local middleY = baseY + tileSize + middleHeight / 2
    objBoxW  = engine.spawnSprite(baseX + tileSize / 2, middleY, tileSize, middleHeight, texBoxW, shellLayer)
    objBox   = engine.spawnSprite(baseX + tileSize + middleWidth / 2, middleY, middleWidth, middleHeight, texBox, shellLayer)
    objBoxE  = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, middleY, tileSize, middleHeight, texBoxE, shellLayer)
    
    -- Bottom row
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    objBoxSW = engine.spawnSprite(baseX + tileSize / 2, row2Y, tileSize, tileSize, texBoxSW, shellLayer)
    objBoxS  = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row2Y, middleWidth, tileSize, texBoxS, shellLayer)
    objBoxSE = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row2Y, tileSize, tileSize, texBoxSE, shellLayer)
    
    -- Prompt text at bottom
    local promptX = baseX + tileSize + 10
    local promptY = row2Y - 32
    objPrompt = engine.spawnText(promptX, promptY, shellFont, "$>", "white", shellLayer)
    local bufferX = promptX + 60
    objBufferText = engine.spawnText(bufferX, promptY, shellFont, inputBuffer, "white", shellLayer)
end

function shell.addHistory(command, result, isError)
    engine.logInfo("addHistory called: " .. command .. " -> " .. tostring(result))
    table.insert(history, {
        command = command,
        result = result,
        isError = isError
    })
    engine.logInfo("History size now: " .. #history)
    if visible then
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
    end
end

-- Helper to get prompt Y position
function getPromptY()
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = calculateBoxHeight()
    local baseY = fbHeight - marginBottom - boxHeight
    local middleHeight = boxHeight - tileSize * 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    return row2Y - 32  -- Adjust based on font size
end

-- Calculate required box height based on history
function calculateBoxHeight()
    local promptPadding = 20  -- Extra space around prompt
    local baseHeight = tileSize * 2 + lineHeight - promptPadding
    
    if #history == 0 then
        return baseHeight
    end
    
    -- Count lines needed for history
    local historyLines = 0
    for _, entry in ipairs(history) do
        historyLines = historyLines + 1
        if entry.result and entry.result ~= "" and entry.result ~= "nil" then
            historyLines = historyLines + 1
        end
    end
    
    local historyHeight = historyLines * lineHeight
    local neededHeight = baseHeight + historyHeight
    
    -- Clamp to screen bounds
    local _, fbHeight = engine.getFramebufferSize()
    local maxHeight = fbHeight - marginTop - marginBottom
    
    return math.min(neededHeight, maxHeight)
end

return shell
