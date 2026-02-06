-- Shell module for debug console
local boxTextures = require("scripts.ui.box_textures")
local shell = {}

-- Script ID (passed from engine)
local myScriptId = nil

-- State
local shellvisible = false
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

-- UI page
local shellPage = nil

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

-- UI element IDs for the 9-box
local objBox = nil
local objBoxN = nil
local objBoxS = nil
local objBoxE = nil
local objBoxW = nil
local objBoxNE = nil
local objBoxNW = nil
local objBoxSE = nil
local objBoxSW = nil

-- Text element IDs
local objPrompt = nil
local objBufferText = nil

-- Track if box elements have been created
local boxSpawned = false

-- Font
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
    engine.logDebug("Shell font loaded with handle: " .. tostring(shellFont))
    engine.logDebug("shell font size: " .. tostring(fontSize))
    
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
    
    shellPage = UI.newPage("shell", "debug")
    engine.logDebug("Shell page created: " .. tostring(shellPage))
    
    focusId = engine.registerFocusable(true, 0)
    engine.logDebug("Shell initialized with focusId: " .. tostring(focusId))
end

function shell.update(dt)
    if not shellvisible then return end
    
    -- dt will be ~0.5, so just toggle every call
    cursorVisible = not cursorVisible
    if objCursor then
        UI.setVisible(objCursor, cursorVisible)
    end
end

function shell.shutdown()
    engine.logDebug("Shell module shutting down")
    shell.destroyAllElements()
    if shellPage then
        UI.hidePage(shellPage)
        UI.deletePage(shellPage)
        shellPage = nil
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
    if fid == focusId and shellvisible then
        shell.hide()
    end
end

function shell.toggle()
    if shellvisible then
        shell.hide()
    else
        shell.show()
    end
end

function shell.show()
    shellvisible = true
    cursorVisible = true
    cursorBlinkTime = 0
    engine.requestFocus(focusId)
    local scaleChanged = shell.rescale()
    if scaleChanged and boxSpawned then
        shell.destroyAllElements()
        boxSpawned = false
    end
    
    shell.rebuildBox()
    shell.rebuildHistoryDisplay()
    UI.showPage(shellPage)
end

function shell.hide()
    shellvisible = false
    engine.releaseFocus()
    UI.hidePage(shellPage)
end

function shell.updateDisplay()
    if not shellvisible then return end
    shell.updateInputScroll()
    if objBufferText then
        UI.setText(objBufferText, shell.getVisibleInput())
    end
    shell.updateCursorPos()
    shell.updateGhostText()
end

function shell.isVisible()
    return shellvisible
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
        UI.deleteElement(obj)
    end
    historyTextObjects = {}
    if shellvisible then
        shell.rebuildBox()
    end
end

function shell.cmdQuit()
    engine.logDebug("Quit requested from shell")
end

function shell.rebuildHistoryDisplay()
    for _, obj in ipairs(historyTextObjects) do
        UI.deleteElement(obj)
    end
    historyTextObjects = {}
    
    if not shellvisible then return end
    if #history == 0 then return end
    
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local baseX = marginLeft
    local textX = baseX + tileSize + historyPadding
    local maxTextWidth = middleWidth - historyPadding * 2
    
    local promptY = shell.getPromptY()
    local y = promptY - lineHeight
    
    for i = #history, 1, -1 do
        local entry = history[i]
        
        -- Result (potentially multi-line)
        if entry.result and entry.result ~= "" and entry.result ~= "nil" then
            local resultColor = {1.0, 1.0, 1.0, 1.0}
            if entry.result == "OK" then
                resultColor = {0.0, 1.0, 0.0, 1.0}
            elseif entry.result:match("^undefined:") then
                resultColor = {1.0, 0.65, 0.0, 1.0}
            elseif entry.isError then
                resultColor = {1.0, 0.0, 0.0, 1.0}
            end
            
            local resultLines = shell.wrapText(entry.result, maxTextWidth - 20, shellFont)
            for j = #resultLines, 1, -1 do
                if y < marginTop + tileSize then break end
                local resultObj = UI.newText(
                    "shell_result_" .. i .. "_" .. j,
                    resultLines[j],
                    shellFont,
                    fontSize,
                    resultColor[1], resultColor[2], resultColor[3], resultColor[4],
                    shellPage
                )
                UI.addToPage(shellPage, resultObj, textX + 20, y)
                table.insert(historyTextObjects, resultObj)
                y = y - lineHeight
            end
        end
        
        -- Command (potentially multi-line)
        local cmdText = "$> " .. entry.command
        local cmdLines = shell.wrapText(cmdText, maxTextWidth, shellFont)
        for j = #cmdLines, 1, -1 do
            if y < marginTop + tileSize then break end
            local cmdObj = UI.newText(
                "shell_cmd_" .. i .. "_" .. j,
                cmdLines[j],
                shellFont,
                fontSize,
                1.0, 1.0, 1.0, 1.0,
                shellPage
            )
            UI.addToPage(shellPage, cmdObj, textX, y)
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
    local boxHeight = shell.calculateBoxHeight()
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
        -- First time: create all UI elements
        objBoxNW = UI.newSprite("shell_nw", tileSize, tileSize, texBoxNW, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxN  = UI.newSprite("shell_n", middleWidth, tileSize, texBoxN, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxNE = UI.newSprite("shell_ne", tileSize, tileSize, texBoxNE, 1.0, 1.0, 1.0, 1.0, shellPage)
        
        objBoxW  = UI.newSprite("shell_w", tileSize, middleHeight, texBoxW, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBox   = UI.newSprite("shell_c", middleWidth, middleHeight, texBox, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxE  = UI.newSprite("shell_e", tileSize, middleHeight, texBoxE, 1.0, 1.0, 1.0, 1.0, shellPage)
        
        objBoxSW = UI.newSprite("shell_sw", tileSize, tileSize, texBoxSW, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxS  = UI.newSprite("shell_s", middleWidth, tileSize, texBoxS, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxSE = UI.newSprite("shell_se", tileSize, tileSize, texBoxSE, 1.0, 1.0, 1.0, 1.0, shellPage)
        
        -- ... rest of addToPage calls unchanged ...
        
        UI.addToPage(shellPage, objBoxNW, baseX, row0Y - tileSize / 2)
        UI.addToPage(shellPage, objBoxN,  baseX + tileSize, row0Y - tileSize / 2)
        UI.addToPage(shellPage, objBoxNE, baseX + tileSize + middleWidth, row0Y - tileSize / 2)
        
        UI.addToPage(shellPage, objBoxW,  baseX, row0Y + tileSize / 2)
        UI.addToPage(shellPage, objBox,   baseX + tileSize, row0Y + tileSize / 2)
        UI.addToPage(shellPage, objBoxE,  baseX + tileSize + middleWidth, row0Y + tileSize / 2)
        
        UI.addToPage(shellPage, objBoxSW, baseX, row0Y + tileSize / 2 + middleHeight)
        UI.addToPage(shellPage, objBoxS,  baseX + tileSize, row0Y + tileSize / 2 + middleHeight)
        UI.addToPage(shellPage, objBoxSE, baseX + tileSize + middleWidth, row0Y + tileSize / 2 + middleHeight)
        
        objPrompt = UI.newText("shell_prompt", "$>", shellFont, fontSize, 1.0, 1.0, 1.0, 1.0, shellPage)
        UI.addToPage(shellPage, objPrompt, promptX, promptY)
        
        objBufferText = UI.newText("shell_buffer", inputBuffer, shellFont, fontSize, 1.0, 1.0, 1.0, 1.0, shellPage)
        UI.addToPage(shellPage, objBufferText, bufferX, promptY)
        
        objCursor = UI.newText("shell_cursor", "|", shellFont, fontSize, 1.0, 1.0, 1.0, 1.0, shellPage)
        UI.addToPage(shellPage, objCursor, bufferX, promptY)
        
        boxSpawned = true
    else
        -- Reposition and resize existing elements
        
        -- Top row
        UI.setPosition(objBoxNW, baseX, row0Y - tileSize / 2)
        UI.setPosition(objBoxN,  baseX + tileSize, row0Y - tileSize / 2)
        UI.setSize(objBoxN, middleWidth, tileSize)
        UI.setPosition(objBoxNE, baseX + tileSize + middleWidth, row0Y - tileSize / 2)
        
        -- Middle row - reposition and resize
        UI.setPosition(objBoxW,  baseX, row0Y + tileSize / 2)
        UI.setSize(objBoxW, tileSize, middleHeight)
        UI.setPosition(objBox,   baseX + tileSize, row0Y + tileSize / 2)
        UI.setSize(objBox, middleWidth, middleHeight)
        UI.setPosition(objBoxE,  baseX + tileSize + middleWidth, row0Y + tileSize / 2)
        UI.setSize(objBoxE, tileSize, middleHeight)
        
        -- Bottom row
        UI.setPosition(objBoxSW, baseX, row0Y + tileSize / 2 + middleHeight)
        UI.setPosition(objBoxS,  baseX + tileSize, row0Y + tileSize / 2 + middleHeight)
        UI.setSize(objBoxS, middleWidth, tileSize)
        UI.setPosition(objBoxSE, baseX + tileSize + middleWidth, row0Y + tileSize / 2 + middleHeight)
        
        -- Prompt
        UI.setPosition(objPrompt, promptX, promptY)
        UI.setPosition(objBufferText, bufferX, promptY)
        UI.setVisible(objCursor, cursorVisible)
        shell.updateCursorPos()
    end
end

function shell.updateCursorPos()
    if not objCursor then return end
    if not shellvisible then return end
    
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = shell.calculateBoxHeight()
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
    
    UI.setPosition(objCursor, cursorX, promptY)
end

function shell.addHistory(command, result, isError)
    table.insert(history, {
        command = command,
        result = result,
        isError = isError
    })
    if shellvisible then
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
    end
end

function shell.getPromptY()
    local fbWidth, fbHeight = engine.getFramebufferSize()
    local boxHeight = shell.calculateBoxHeight()
    local baseY = fbHeight - marginBottom - boxHeight
    local middleHeight = boxHeight - tileSize * 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    return row2Y - fontSize
end

function shell.getMaxInputWidth()
    if maxInputWidth > 0 then return maxInputWidth end
    maxInputWidth = middleWidth - 100
    return maxInputWidth
end

-- Update scroll position based on cursor
function shell.updateInputScroll()
    local maxWidth = shell.getMaxInputWidth()
    
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
function shell.getVisibleInput()
    local maxWidth = shell.getMaxInputWidth()
    local visibleText = inputBuffer:sub(inputScrollOffset + 1)
    
    -- Trim to fit width
    for i = #visibleText, 1, -1 do
        local test = visibleText:sub(1, i)
        if engine.getTextWidth(shellFont, test, fontSize) <= maxWidth then
            return test
        end
    end
    return ""
end

function shell.calculateBoxHeight()
    local promptPadding = 20
    local baseHeight = tileSize * 2 + lineHeight - promptPadding
    
    if #history == 0 then
        return baseHeight
    end
    
    local maxTextWidth = middleWidth - historyPadding * 2
    local historyLines = 0
    for _, entry in ipairs(history) do
        historyLines = historyLines + shell.countLinesForEntry(entry, maxTextWidth, shellFont)
    end
    
    local historyHeight = historyLines * lineHeight
    local neededHeight = baseHeight + historyHeight
    
    local _, fbHeight = engine.getFramebufferSize()
    local maxHeight = fbHeight - marginTop - marginBottom
    
    return math.min(neededHeight, maxHeight)
end

function shell.longestCommonPrefix(strings)
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
function shell.getCurrentWord()
    return inputBuffer:match("[%w_%.]+$") or ""
end

-- Wrap text into multiple lines that fit within maxWidth (by character)
function shell.wrapText(text, maxWidth, font)
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
function shell.wrapTextByWord(text, maxWidth, font)
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
function shell.countLinesForEntry(entry, maxWidth, font)
    local lines = 0
    
    local cmdText = "$> " .. entry.command
    lines = lines + #shell.wrapText(cmdText, maxWidth, font)
    
    if entry.result and entry.result ~= "" and entry.result ~= "nil" then
        lines = lines + #shell.wrapText(entry.result, maxWidth - 20, font)
    end
    
    return lines
end

-- More general table member completion
function shell.getTableCompletions(tableName, memberPrefix)
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
    shell._completionResults = {}
    shell._completionSeen = {}
    
    -- Check for table.member pattern
    local tableName, memberPrefix = prefix:match("^([%w_]+)%.(.*)$")
    if tableName then
        for _, completion in ipairs(shell.getTableCompletions(tableName, memberPrefix)) do
            if not shell._completionSeen[completion] then
                shell._completionSeen[completion] = true
                table.insert(shell._completionResults, completion)
            end
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
                if not shell._completionSeen[kw] then
                    shell._completionSeen[kw] = true
                    table.insert(shell._completionResults, kw)
                end
            end
        end
        
        -- Globals
        for name, _ in pairs(_G) do
            if type(name) == "string" and name:sub(1, #prefix) == prefix then
                if not shell._completionSeen[name] then
                    shell._completionSeen[name] = true
                    table.insert(shell._completionResults, name)
                end
            end
        end
        
        -- Sandbox globals
        if shellSandbox then
            for name, _ in pairs(shellSandbox) do
                if type(name) == "string" and name:sub(1, #prefix) == prefix then
                    if not shell._completionSeen[name] then
                        shell._completionSeen[name] = true
                        table.insert(shell._completionResults, name)
                    end
                end
            end
        end
        
        -- Command history
        for _, entry in ipairs(history) do
            if entry.command:sub(1, #prefix) == prefix then
                if not shell._completionSeen[entry.command] then
                    shell._completionSeen[entry.command] = true
                    table.insert(shell._completionResults, entry.command)
                end
            end
        end
    end
    
    table.sort(shell._completionResults)
    return shell._completionResults
end

-- Update ghost text showing completion hint
function shell.updateGhostText()
    if not shellvisible then return end
    if cursorPos ~= #inputBuffer then
        if ghostText then
            UI.setVisible(ghostText, false)
        end
        currentCompletions = {}
        return
    end
    
    local prefix = shell.getCurrentWord()
    
    if #prefix == 0 then
        if ghostText then
            UI.setVisible(ghostText, false)
        end
        currentCompletions = {}
        return
    end
    
    currentCompletions = shell.getCompletions(prefix)
    
    if #currentCompletions > 0 then
        local commonPrefix = shell.longestCommonPrefix(currentCompletions)
        local ghostPart = commonPrefix:sub(#prefix + 1)
        local maxWidth = shell.getMaxInputWidth()
        local currentWidth = engine.getTextWidth(shellFont, inputBuffer:sub(inputScrollOffset + 1), fontSize)
        local ghostWidth = engine.getTextWidth(shellFont, ghostPart, fontSize)

        if #ghostPart > 0 and (currentWidth + ghostWidth) <= maxWidth then
            if not ghostText then
                ghostText = UI.newText("shell_ghost", ghostPart, shellFont, fontSize, 0.5, 0.5, 0.5, 0.5, shellPage)
                UI.addToPage(shellPage, ghostText, 0, 0)
            else
                UI.setText(ghostText, ghostPart)
                UI.setVisible(ghostText, true)
            end
            
            -- Position after cursor
            local fbWidth, fbHeight = engine.getFramebufferSize()
            local boxHeight = shell.calculateBoxHeight()
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
            
            UI.setPosition(ghostText, cursorX, promptY)
        else
            if ghostText then
                UI.setVisible(ghostText, false)
            end
        end
    else
        if ghostText then
            UI.setVisible(ghostText, false)
        end
    end
end

-- Tab completion - complete to common prefix
function shell.onTab()
    local prefix = shell.getCurrentWord()
    if #prefix == 0 then return end
    
    local completions = shell.getCompletions(prefix)
    if #completions == 0 then return end
    
    local commonPrefix = shell.longestCommonPrefix(completions)
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
    if shellvisible then
        shell.destroyAllElements()
        boxSpawned = false
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
    end
end

-- Recalculate all scaled values from current UI scale
function shell.rescale()
    local newScale = engine.getUIScale()
    
    -- Only rebuild if scale actually changed
    if newScale == uiscale and boxSpawned then
        return false
    end
    
    uiscale = newScale
    
    -- Recalculate all scaled values from base values
    local baseTileSize = 64
    local baseMiddleWidth = 1200
    local baseFontSize = 32
    local baseMarginLeft = 40
    local baseMarginBottom = 40
    local baseMarginTop = 40
    local baseLineHeight = 40
    
    tileSize = math.floor(baseTileSize * uiscale)
    middleWidth = math.floor(baseMiddleWidth * uiscale)
    fontSize = math.floor(baseFontSize * uiscale)
    marginLeft = math.floor(baseMarginLeft * uiscale)
    marginBottom = math.floor(baseMarginBottom * uiscale)
    marginTop = math.floor(baseMarginTop * uiscale)
    lineHeight = math.floor(baseLineHeight * uiscale)
    
    -- Reset max input width cache
    maxInputWidth = 0
    
    engine.logDebug("Shell rescaled to: " .. tostring(uiscale))
    return true
end

-- Destroy all shell UI elements (for rescale rebuild)
function shell.destroyAllElements()
    if objBoxNW then UI.deleteElement(objBoxNW); objBoxNW = nil end
    if objBoxN then UI.deleteElement(objBoxN); objBoxN = nil end
    if objBoxNE then UI.deleteElement(objBoxNE); objBoxNE = nil end
    if objBoxW then UI.deleteElement(objBoxW); objBoxW = nil end
    if objBox then UI.deleteElement(objBox); objBox = nil end
    if objBoxE then UI.deleteElement(objBoxE); objBoxE = nil end
    if objBoxSW then UI.deleteElement(objBoxSW); objBoxSW = nil end
    if objBoxS then UI.deleteElement(objBoxS); objBoxS = nil end
    if objBoxSE then UI.deleteElement(objBoxSE); objBoxSE = nil end
    if objPrompt then UI.deleteElement(objPrompt); objPrompt = nil end
    if objBufferText then UI.deleteElement(objBufferText); objBufferText = nil end
    if objCursor then UI.deleteElement(objCursor); objCursor = nil end
    if ghostText then UI.deleteElement(ghostText); ghostText = nil end
    
    -- Destroy history text objects
    for _, obj in ipairs(historyTextObjects) do
        UI.deleteElement(obj)
    end
    historyTextObjects = {}
    
    boxSpawned = false
end

return shell
