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
local historyIndex = 0
local savedInputBuffer = ""
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
local cursorPos = 0
local inputScrollOffset = 0

-- Configuration
local tileSize = 64
local middleWidth = 1200
local shellLayer = 10
local fontSize = 32
local maxInputWidth = 0

-- System configuration
local uiscale = 1.0

-- completion state
local currentCompletions = {}
local ghostText = nil

function shell.init(scriptId)
    myScriptId = scriptId
    engine.logInfo("Shell initializing...")

    uiscale = engine.getUIScale()
    tileSize = math.floor(tileSize * uiscale)
    middleWidth = math.floor(middleWidth * uiscale)
    fontSize = math.floor(fontSize * uiscale)
    marginLeft = math.floor(marginLeft * uiscale)
    marginBottom = math.floor(marginBottom * uiscale)
    marginTop = math.floor(marginTop * uiscale)
    lineHeight = math.floor(lineHeight * uiscale)

    shellFont = engine.loadFont("assets/fonts/shell.ttf", fontSize)
    
    texBox = engine.loadTexture("assets/textures/box/box.png")
    texBoxN = engine.loadTexture("assets/textures/box/boxn.png")
    texBoxS = engine.loadTexture("assets/textures/box/boxs.png")
    texBoxE = engine.loadTexture("assets/textures/box/boxe.png")
    texBoxW = engine.loadTexture("assets/textures/box/boxw.png")
    texBoxNE = engine.loadTexture("assets/textures/box/boxne.png")
    texBoxNW = engine.loadTexture("assets/textures/box/boxnw.png")
    texBoxSE = engine.loadTexture("assets/textures/box/boxse.png")
    texBoxSW = engine.loadTexture("assets/textures/box/boxsw.png")
    
    engine.logDebug("Shell textures loaded")
    
    focusId = engine.registerFocusable(true, 0)
    engine.logDebug("Shell initialized with focusId: " .. tostring(focusId))
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
    engine.logDebug("Shell module shutting down")
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
    if ghostText then engine.destroy(ghostText) end
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
    if ghostText then engine.setVisible(ghostText, false) end
    for _, obj in ipairs(historyTextObjects) do
        engine.setVisible(obj, false)
    end
end

function shell.updateDisplay()
    if not visible then return end
    updateInputScroll()
    if objBufferText then
        engine.setText(objBufferText, getVisibleInput())
    end
    shell.updateCursorPos()
    shell.updateGhostText()
end

function shell.isVisible()
    return visible
end

function shell.getFocusId()
    return focusId
end

function shell.onChar(char)
    historyIndex = 0
    savedInputBuffer = ""
    -- Insert at cursor position
    local before = inputBuffer:sub(1, cursorPos)
    local after = inputBuffer:sub(cursorPos + 1)
    inputBuffer = before .. char .. after
    cursorPos = cursorPos + 1
    shell.updateDisplay()
end

function shell.onBackspace()
    if cursorPos > 0 then
        local before = inputBuffer:sub(1, cursorPos - 1)
        local after = inputBuffer:sub(cursorPos + 1)
        inputBuffer = before .. after
        cursorPos = cursorPos - 1
        shell.updateDisplay()
    end
end

function shell.onSubmit()
    if inputBuffer == "" then return end
    historyIndex = 0
    savedInputBuffer = ""
    inputScrollOffset = 0
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
    cursorPos = 0
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
    engine.logDebug("Quit requested from shell")
end

function shell.rebuildHistoryDisplay()
    for _, obj in ipairs(historyTextObjects) do
        engine.destroy(obj)
    end
    historyTextObjects = {}
    
    if not visible then return end
    if #history == 0 then return end
    
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local baseX = marginLeft
    local textX = baseX + tileSize + historyPadding
    local maxTextWidth = middleWidth - historyPadding * 2
    
    local promptY = getPromptY()
    local y = promptY - lineHeight
    
    for i = #history, 1, -1 do
        local entry = history[i]
        
        -- Result (potentially multi-line)
        if entry.result and entry.result ~= "" and entry.result ~= "nil" then
            local resultColor = "white"
            if entry.result == "OK" then
                resultColor = "green"
            elseif entry.result:match("^undefined:") then
                resultColor = "orange"
            elseif entry.isError then
                resultColor = "red"
            end
            
            local resultLines = wrapText(entry.result, maxTextWidth - 20, shellFont)
            for j = #resultLines, 1, -1 do
                if y < marginTop + tileSize then break end
                local resultObj = engine.spawnText(textX + 20, y, shellFont, resultLines[j], resultColor, shellLayer, fontSize)
                table.insert(historyTextObjects, resultObj)
                y = y - lineHeight
            end
        end
        
        -- Command (potentially multi-line)
        local cmdText = "$> " .. entry.command
        local cmdLines = wrapText(cmdText, maxTextWidth, shellFont)
        for j = #cmdLines, 1, -1 do
            if y < marginTop + tileSize then break end
            local cmdObj = engine.spawnText(textX, y, shellFont, cmdLines[j], "white", shellLayer, fontSize)
            table.insert(historyTextObjects, cmdObj)
            y = y - lineHeight
        end
        
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
    local promptWidth = engine.getTextWidth(shellFont, "$>", fontSize)
    local bufferX = promptX + promptWidth + 10
    
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
        
        objPrompt = engine.spawnText(promptX, promptY, shellFont, "$>", "white", shellLayer, fontSize)
        objBufferText = engine.spawnText(bufferX, promptY, shellFont, inputBuffer, "white", shellLayer, fontSize)
        objCursor = engine.spawnText(bufferX, promptY, shellFont, "|", "white", shellLayer, fontSize)
        
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
    local promptWidth = engine.getTextWidth(shellFont, "$>", fontSize)
    local bufferX = promptX + promptWidth + 10
    
    -- Only measure text up to cursor position
    local textBeforeCursor = inputBuffer:sub(inputScrollOffset + 1, cursorPos)
    local textWidth = engine.getTextWidth(shellFont, textBeforeCursor, fontSize)
    local cursorWidth = engine.getTextWidth(shellFont, "|", fontSize)
    local cursorX = bufferX + textWidth - cursorWidth / 2
    
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

function getMaxInputWidth()
    if maxInputWidth > 0 then return maxInputWidth end
    maxInputWidth = middleWidth - 100
    return maxInputWidth
end

-- Update scroll position based on cursor
function updateInputScroll()
    local maxWidth = getMaxInputWidth()
    
    -- If cursor moved left of scroll offset, scroll left
    if cursorPos < inputScrollOffset then
        inputScrollOffset = cursorPos
        return
    end
    
    -- If cursor is at start, reset scroll
    if cursorPos == 0 then
        inputScrollOffset = 0
        return
    end
    
    -- Check if cursor is past right edge
    local widthFromScrollToCursor = engine.getTextWidth(shellFont, inputBuffer:sub(inputScrollOffset + 1, cursorPos), fontSize)
    
    -- If cursor goes past right edge, scroll right
    while widthFromScrollToCursor > maxWidth do
        inputScrollOffset = inputScrollOffset + 1
        widthFromScrollToCursor = engine.getTextWidth(shellFont, inputBuffer:sub(inputScrollOffset + 1, cursorPos), fontSize)
    end
end

-- Get the visible portion of input buffer
function getVisibleInput()
    local maxWidth = getMaxInputWidth()
    local visible = inputBuffer:sub(inputScrollOffset + 1)
    
    -- Trim to fit width
    for i = #visible, 1, -1 do
        local test = visible:sub(1, i)
        if engine.getTextWidth(shellFont, test, fontSize) <= maxWidth then
            return test
        end
    end
    return ""
end

function calculateBoxHeight()
    local promptPadding = 20
    local baseHeight = tileSize * 2 + lineHeight - promptPadding
    
    if #history == 0 then
        return baseHeight
    end
    
    local maxTextWidth = middleWidth - historyPadding * 2
    local historyLines = 0
    for _, entry in ipairs(history) do
        historyLines = historyLines + countLinesForEntry(entry, maxTextWidth, shellFont)
    end
    
    local historyHeight = historyLines * lineHeight
    local neededHeight = baseHeight + historyHeight
    
    local _, fbHeight = engine.getFramebufferSize()
    local maxHeight = fbHeight - marginTop - marginBottom
    
    return math.min(neededHeight, maxHeight)
end

local function longestCommonPrefix(strings)
    if #strings == 0 then return "" end
    if #strings == 1 then return strings[1] end
    
    local prefix = strings[1]
    for i = 2, #strings do
        local s = strings[i]
        local j = 1
        while j <= #prefix and j <= #s and prefix:sub(j, j) == s:sub(j, j) do
            j = j + 1
        end
        prefix = prefix:sub(1, j - 1)
        if prefix == "" then return "" end
    end
    return prefix
end

-- Get the current word being typed (handles engine.xxx)
function getCurrentWord()
    return inputBuffer:match("[%w_%.]+$") or ""
end

-- Wrap text into multiple lines that fit within maxWidth (by character)
function wrapText(text, maxWidth, font)
    local lines = {}
    local currentLine = ""
    
    for i = 1, #text do
        local char = text:sub(i, i)
        local testLine = currentLine .. char
        local width = engine.getTextWidth(font, testLine, fontSize)
        
        if width > maxWidth and currentLine ~= "" then
            table.insert(lines, currentLine)
            currentLine = char
        else
            currentLine = testLine
        end
    end
    
    if currentLine ~= "" then
        table.insert(lines, currentLine)
    end
    
    if #lines == 0 then
        table.insert(lines, text)
    end
    
    return lines
end

-- Wrap text into multiple lines that fit within maxWidth
function wrapTextByWord(text, maxWidth, font)
    local lines = {}
    local currentLine = ""
    
    for word in text:gmatch("%S+") do
        local testLine = currentLine == "" and word or (currentLine .. " " .. word)
        local width = engine.getTextWidth(font, testLine, fontSize)
        
        if width > maxWidth and currentLine ~= "" then
            table.insert(lines, currentLine)
            currentLine = word
        else
            currentLine = testLine
        end
    end
    
    if currentLine ~= "" then
        table.insert(lines, currentLine)
    end
    
    -- Handle empty input
    if #lines == 0 then
        table.insert(lines, text)
    end
    
    return lines
end

-- Count lines needed for a history entry
function countLinesForEntry(entry, maxWidth, font)
    local lines = 0
    
    local cmdText = "$> " .. entry.command
    lines = lines + #wrapText(cmdText, maxWidth, font)
    
    if entry.result and entry.result ~= "" and entry.result ~= "nil" then
        lines = lines + #wrapText(entry.result, maxWidth - 20, font)
    end
    
    return lines
end

-- More general table member completion
function getTableCompletions(tableName, memberPrefix)
    local results = {}
    local tbl = _G[tableName] or (shellSandbox and shellSandbox[tableName])
    if type(tbl) == "table" then
        for name, _ in pairs(tbl) do
            if type(name) == "string" and name:sub(1, #memberPrefix) == memberPrefix then
                table.insert(results, tableName .. "." .. name)
            end
        end
    end
    return results
end

function shell.getCompletions(prefix)
    local results = {}
    local seen = {}
    
    local function addUnique(str)
        if not seen[str] then
            seen[str] = true
            table.insert(results, str)
        end
    end
    
    -- Check for table.member pattern
    local tableName, memberPrefix = prefix:match("^([%w_]+)%.(.*)$")
    if tableName then
        for _, completion in ipairs(getTableCompletions(tableName, memberPrefix)) do
            addUnique(completion)
        end
    else
        -- Lua keywords
        local keywords = {
            "and", "break", "do", "else", "elseif", "end",
            "false", "for", "function", "if", "in", "local",
            "nil", "not", "or", "repeat", "return", "then",
            "true", "until", "while"
        }
        for _, kw in ipairs(keywords) do
            if kw:sub(1, #prefix) == prefix then
                addUnique(kw)
            end
        end
        
        -- Globals
        for name, _ in pairs(_G) do
            if type(name) == "string" and name:sub(1, #prefix) == prefix then
                addUnique(name)
            end
        end
        
        -- Sandbox globals
        if shellSandbox then
            for name, _ in pairs(shellSandbox) do
                if type(name) == "string" and name:sub(1, #prefix) == prefix then
                    addUnique(name)
                end
            end
        end
        
        -- Command history
        for _, entry in ipairs(history) do
            if entry.command:sub(1, #prefix) == prefix then
                addUnique(entry.command)
            end
        end
    end
    
    table.sort(results)
    return results
end

-- Update ghost text showing completion hint
function shell.updateGhostText()
    if not visible then return end
    if cursorPos ~= #inputBuffer then
        if ghostText then
            engine.setVisible(ghostText, false)
        end
        currentCompletions = {}
        return
    end
    
    local prefix = getCurrentWord()
    
    if #prefix == 0 then
        if ghostText then
            engine.setVisible(ghostText, false)
        end
        currentCompletions = {}
        return
    end
    
    currentCompletions = shell.getCompletions(prefix)
    
    if #currentCompletions > 0 then
        local commonPrefix = longestCommonPrefix(currentCompletions)
        local ghostPart = commonPrefix:sub(#prefix + 1)
        local maxWidth = getMaxInputWidth()
        local currentWidth = engine.getTextWidth(shellFont, inputBuffer:sub(inputScrollOffset + 1), fontSize)
        local ghostWidth = engine.getTextWidth(shellFont, ghostPart, fontSize)

        if #ghostPart > 0 and (currentWidth + ghostWidth) <= maxWidth then
            if not ghostText then
                ghostText = engine.spawnText(0, 0, shellFont, ghostPart, "ghost", shellLayer, fontSize)
            else
                engine.setText(ghostText, ghostPart)
                engine.setVisible(ghostText, true)
            end
            
            -- Position after cursor
            local fbWidth, fbHeight = engine.getFramebufferSize()
            local boxHeight = calculateBoxHeight()
            local baseX = marginLeft
            local baseY = fbHeight - marginBottom - boxHeight
            local middleHeight = boxHeight - tileSize * 2
            local row2Y = baseY + tileSize + middleHeight + tileSize / 2
            local promptX = baseX + tileSize + 10
            local promptY = row2Y - fontSize
            local promptWidth = engine.getTextWidth(shellFont, "$>", fontSize)
            local bufferX = promptX + promptWidth + 10
            local textWidth = engine.getTextWidth(shellFont, inputBuffer:sub(inputScrollOffset + 1), fontSize)
            local cursorWidth = engine.getTextWidth(shellFont, "|", fontSize)
            local cursorX = bufferX + textWidth - cursorWidth / 2
            
            engine.setPos(ghostText, cursorX, promptY)
        else
            if ghostText then
                engine.setVisible(ghostText, false)
            end
        end
    else
        if ghostText then
            engine.setVisible(ghostText, false)
        end
    end
end

-- Tab completion - complete to common prefix
function shell.onTab()
    local prefix = getCurrentWord()
    if #prefix == 0 then return end
    
    local completions = shell.getCompletions(prefix)
    if #completions == 0 then return end
    
    local commonPrefix = longestCommonPrefix(completions)
    local addition = commonPrefix:sub(#prefix + 1)
    
    if #addition > 0 then
        local before = inputBuffer:sub(1, cursorPos)
        local after = inputBuffer:sub(cursorPos + 1)
        inputBuffer = before .. addition .. after
        cursorPos = cursorPos + #addition
        shell.updateDisplay()
    end
end

-- Handle tab key event
function shell.onTabPressed(fid)
    if fid == focusId then
        shell.onTab()
    end
end

function shell.onCursorUp(fid)
    if fid ~= focusId then return end
    if #history == 0 then return end
    
    -- Save current input when starting to browse
    if historyIndex == 0 then
        savedInputBuffer = inputBuffer
    end
    
    -- Move up in history (towards older commands)
    if historyIndex < #history then
        historyIndex = historyIndex + 1
        local entry = history[#history - historyIndex + 1]
        inputBuffer = entry.command
        cursorPos = #inputBuffer
        shell.updateDisplay()
    end
end

function shell.onCursorDown(fid)
    if fid ~= focusId then return end
    
    if historyIndex > 1 then
        -- Move down in history (towards newer commands)
        historyIndex = historyIndex - 1
        local entry = history[#history - historyIndex + 1]
        inputBuffer = entry.command
        cursorPos = #inputBuffer
        shell.updateDisplay()
    elseif historyIndex == 1 then
        -- Back to saved input
        historyIndex = 0
        inputBuffer = savedInputBuffer
        cursorPos = #inputBuffer
        shell.updateDisplay()
    end
end

function shell.onCursorLeft(fid)
    if fid == focusId and cursorPos > 0 then
        cursorPos = cursorPos - 1
        shell.updateCursorPos()
        if cursorPos < inputScrollOffset then
            inputScrollOffset = cursorPos
            shell.updateDisplay()
        end
    end
end

function shell.onCursorRight(fid)
    if fid == focusId and cursorPos < #inputBuffer then
        cursorPos = cursorPos + 1
        shell.updateCursorPos()
        shell.updateDisplay()
    end
end

function shell.onCursorHome(fid)
    if fid == focusId then
        cursorPos = 0
        inputScrollOffset = 0
        shell.updateCursorPos()
        shell.updateDisplay()
    end
end

function shell.onCursorEnd(fid)
    if fid == focusId then
        cursorPos = #inputBuffer
        shell.updateCursorPos()
        shell.updateDisplay()
    end
end

function shell.onDelete(fid)
    if fid == focusId and cursorPos < #inputBuffer then
        local before = inputBuffer:sub(1, cursorPos)
        local after = inputBuffer:sub(cursorPos + 2)
        inputBuffer = before .. after
        shell.updateDisplay()
    end
end

function shell.onInterrupt(fid)
    if fid ~= focusId then return end
    historyIndex = 0
    savedInputBuffer = ""
    inputScrollOffset = 0
    inputBuffer = ""
    cursorPos = 0
    shell.updateDisplay()
end

function shell.onFramebufferResize(width, height)
    -- Reset max input width cache so it gets recalculated
    maxInputWidth = 0
    
    -- If visible, rebuild everything with new dimensions
    if visible then
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
        shell.updateCursorPos()
        if ghostText then
            shell.updateGhostText()
        end
    end
end

return shell
