-- Shell module for debug console
local shell = {}

-- Script ID (passed from engine)
local myScriptId = nil

-- State
local visible = false
local focusId = nil
local inputBuffer = ""
local history = {}
local historyTextObjects = {}
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

-- UI object IDs for the 9-box (persistent - spawned once)
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

-- Track if box sprites have been created
local boxSpawned = false

-- Font
local fontHandle = nil
local shellFont = nil

-- Cursor
local objCursor = nil
local cursorVisible = true
local cursorBlinkTime = 0
local cursorBlinkRate = 0.5 -- seconds

-- Configuration
local tileSize = 64
local middleWidth = 1200
local shellLayer = 10
local fontSize = 32

function shell.init(scriptId)
    myScriptId = scriptId
    engine.logInfo("Shell module initialized with scriptId: " .. tostring(scriptId))
    
    shellFont = engine.loadFont("assets/fonts/Cabal.ttf", fontSize)
    if shellFont then
        engine.logInfo("Shell font loaded: Cabal.ttf")
    else
        engine.logError("Failed to load shell font")
    end
    
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
    
    focusId = engine.registerFocusable(true, 0)
    engine.logInfo("Shell initialized with focusId: " .. tostring(focusId))
end

function shell.update(dt)
    if not visible then return end
    
    -- dt will be ~0.5, so just toggle every call
    cursorVisible = not cursorVisible
    if objCursor then
        engine.setVisible(objCursor, cursorVisible)
    end
end

function shell.shutdown()
    engine.logInfo("Shell module shutting down")
    -- Cleanup all objects
    if objBoxNW then engine.destroy(objBoxNW) end
    if objBoxN then engine.destroy(objBoxN) end
    if objBoxNE then engine.destroy(objBoxNE) end
    if objBoxW then engine.destroy(objBoxW) end
    if objBox then engine.destroy(objBox) end
    if objBoxE then engine.destroy(objBoxE) end
    if objBoxSW then engine.destroy(objBoxSW) end
    if objBoxS then engine.destroy(objBoxS) end
    if objBoxSE then engine.destroy(objBoxSE) end
    if objPrompt then engine.destroy(objPrompt) end
    if objBufferText then engine.destroy(objBufferText) end
    if objCursor then engine.destroy(objCursor) end
    for _, obj in ipairs(historyTextObjects) do
        engine.destroy(obj)
    end
end

function shell.onShellToggle()
    shell.toggle()
end

function shell.onCharInput(fid, char)
    if fid == focusId then
        shell.onChar(char)
    end
end

function shell.onTextBackspace(fid)
    if fid == focusId then
        shell.onBackspace()
    end
end

function shell.onTextSubmit(fid)
    if fid == focusId then
        shell.onSubmit()
    end
end

function shell.onFocusLost(fid)
    if fid == focusId and visible then
        shell.hide()
    end
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
    cursorVisible = true
    cursorBlinkTime = 0
    engine.requestFocus(focusId)
    
    shell.rebuildBox()
    shell.rebuildHistoryDisplay()
end

function shell.hide()
    visible = false
    engine.releaseFocus()
    
    -- Just hide, don't destroy
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
    if objCursor then engine.setVisible(objCursor, false) end
    for _, obj in ipairs(historyTextObjects) do
        engine.setVisible(obj, false)
    end
end

function shell.updateDisplay()
    if not visible then return end
    if objBufferText then
        engine.setText(objBufferText, inputBuffer)
    end
    shell.updateCursorPos()
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

function shell.cmdHelp()
    return "Commands: help, clear, quit/exit\nOr enter Lua code to execute"
end

function shell.cmdClear()
    history = {}
    -- Destroy history text objects
    for _, obj in ipairs(historyTextObjects) do
        engine.destroy(obj)
    end
    historyTextObjects = {}
    if visible then
        shell.rebuildBox()
    end
end

function shell.cmdQuit()
    engine.logInfo("Quit requested from shell")
end

function shell.rebuildHistoryDisplay()
    -- Destroy old text objects
    for _, obj in ipairs(historyTextObjects) do
        engine.destroy(obj)
    end
    historyTextObjects = {}
    
    if not visible then return end
    if #history == 0 then return end
    
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local baseX = marginLeft
    local textX = baseX + tileSize + historyPadding
    
    local promptY = getPromptY()
    local availableHeight = promptY - marginTop - tileSize - historyPadding
    local maxLines = math.floor(availableHeight / lineHeight)
    
    local startIdx = math.max(1, #history - maxLines + 1)
    
    local y = promptY - lineHeight
    for i = #history, startIdx, -1 do
        local entry = history[i]
        
        local resultColor = "white"
        if entry.result == "OK" then
            resultColor = "green"
        elseif entry.result and entry.result:match("^undefined:") then
            resultColor = "orange"
        elseif entry.isError then
            resultColor = "red"
        end

        if entry.result and entry.result ~= "" and entry.result ~= "nil" then
            local resultObj = engine.spawnText(textX + 20, y, shellFont, entry.result, resultColor, shellLayer)
            table.insert(historyTextObjects, resultObj)
            y = y - lineHeight
        end
        
        local cmdText = "$> " .. entry.command
        local cmdObj = engine.spawnText(textX, y, shellFont, cmdText, "white", shellLayer)
        table.insert(historyTextObjects, cmdObj)
        y = y - lineHeight
        
        if y < marginTop + tileSize then
            break
        end
    end
end

function shell.rebuildBox()
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = calculateBoxHeight()
    local middleHeight = boxHeight - tileSize * 2
    local baseX = marginLeft
    local baseY = fbHeight - marginBottom - boxHeight
    
    -- Calculate positions
    local row0Y = baseY + tileSize / 2
    local middleY = baseY + tileSize + middleHeight / 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    local promptX = baseX + tileSize + 10
    local promptY = row2Y - fontSize
    local bufferX = promptX + fontSize
    
    if not boxSpawned then
        -- First time: spawn all sprites
        objBoxNW = engine.spawnSprite(baseX + tileSize / 2, row0Y, tileSize, tileSize, texBoxNW, shellLayer)
        objBoxN  = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row0Y, middleWidth, tileSize, texBoxN, shellLayer)
        objBoxNE = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row0Y, tileSize, tileSize, texBoxNE, shellLayer)
        
        objBoxW  = engine.spawnSprite(baseX + tileSize / 2, middleY, tileSize, middleHeight, texBoxW, shellLayer)
        objBox   = engine.spawnSprite(baseX + tileSize + middleWidth / 2, middleY, middleWidth, middleHeight, texBox, shellLayer)
        objBoxE  = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, middleY, tileSize, middleHeight, texBoxE, shellLayer)
        
        objBoxSW = engine.spawnSprite(baseX + tileSize / 2, row2Y, tileSize, tileSize, texBoxSW, shellLayer)
        objBoxS  = engine.spawnSprite(baseX + tileSize + middleWidth / 2, row2Y, middleWidth, tileSize, texBoxS, shellLayer)
        objBoxSE = engine.spawnSprite(baseX + tileSize + middleWidth + tileSize / 2, row2Y, tileSize, tileSize, texBoxSE, shellLayer)
        
        objPrompt = engine.spawnText(promptX, promptY, shellFont, "$>", "white", shellLayer)
        objBufferText = engine.spawnText(bufferX, promptY, shellFont, inputBuffer, "white", shellLayer)
        objCursor = engine.spawnText(bufferX, promptY, shellFont, "|", "white", shellLayer)
        
        boxSpawned = true
    else
        -- Reposition and resize existing sprites
        engine.setVisible(objBoxNW, true)
        engine.setVisible(objBoxN, true)
        engine.setVisible(objBoxNE, true)
        engine.setVisible(objBoxW, true)
        engine.setVisible(objBox, true)
        engine.setVisible(objBoxE, true)
        engine.setVisible(objBoxSW, true)
        engine.setVisible(objBoxS, true)
        engine.setVisible(objBoxSE, true)
        engine.setVisible(objPrompt, true)
        engine.setVisible(objBufferText, true)
        
        -- Top row
        engine.setPos(objBoxNW, baseX + tileSize / 2, row0Y)
        engine.setPos(objBoxN, baseX + tileSize + middleWidth / 2, row0Y)
        engine.setPos(objBoxNE, baseX + tileSize + middleWidth + tileSize / 2, row0Y)
        
        -- Middle row - reposition and resize
        engine.setPos(objBoxW, baseX + tileSize / 2, middleY)
        engine.setSize(objBoxW, tileSize, middleHeight)
        engine.setPos(objBox, baseX + tileSize + middleWidth / 2, middleY)
        engine.setSize(objBox, middleWidth, middleHeight)
        engine.setPos(objBoxE, baseX + tileSize + middleWidth + tileSize / 2, middleY)
        engine.setSize(objBoxE, tileSize, middleHeight)
        
        -- Bottom row
        engine.setPos(objBoxSW, baseX + tileSize / 2, row2Y)
        engine.setPos(objBoxS, baseX + tileSize + middleWidth / 2, row2Y)
        engine.setPos(objBoxSE, baseX + tileSize + middleWidth + tileSize / 2, row2Y)
        
        -- Prompt
        engine.setPos(objPrompt, promptX, promptY)
        engine.setPos(objBufferText, bufferX, promptY)
        engine.setVisible(objCursor, cursorVisible)
        shell.updateCursorPos()
    end
end

-- Update cursor position based on text width
function shell.updateCursorPos()
    if not objCursor then return end
    if not visible then return end
    
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = calculateBoxHeight()
    local baseX = marginLeft
    local baseY = fbHeight - marginBottom - boxHeight
    local middleHeight = boxHeight - tileSize * 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    local promptX = baseX + tileSize + 10
    local promptY = row2Y - fontSize
    local bufferX = promptX + fontSize
    
    local textWidth = engine.getTextWidth(shellFont, inputBuffer)
    local cursorX = bufferX + textWidth
    
    engine.setPos(objCursor, cursorX, promptY)
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

function getPromptY()
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = calculateBoxHeight()
    local baseY = fbHeight - marginBottom - boxHeight
    local middleHeight = boxHeight - tileSize * 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    return row2Y - fontSize
end

function calculateBoxHeight()
    local promptPadding = 20
    local baseHeight = tileSize * 2 + lineHeight - promptPadding
    
    if #history == 0 then
        return baseHeight
    end
    
    local historyLines = 0
    for _, entry in ipairs(history) do
        historyLines = historyLines + 1
        if entry.result and entry.result ~= "" and entry.result ~= "nil" then
            historyLines = historyLines + 1
        end
    end
    
    local historyHeight = historyLines * lineHeight
    local neededHeight = baseHeight + historyHeight
    
    local _, fbHeight = engine.getFramebufferSize()
    local maxHeight = fbHeight - marginTop - marginBottom
    
    return math.min(neededHeight, maxHeight)
end

return shell
