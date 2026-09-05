-- Shell module for debug console
local boxTextures = require("scripts.ui.box_textures")
local textWrap = require("scripts.ui.text_wrap")
-- #1187: cursorPos/inputScrollOffset below are zero-based Unicode CODE-POINT
-- offsets into inputBuffer, the same contract UI.TextBuffer and the editable
-- widgets use (#746). Lua's # and string.sub count BYTES, so every slice,
-- measurement and cursor step goes through this helper -- a byte-stepping
-- cursor leaves a stray fragment of a multi-byte character in the buffer.
local utf8Safe = require("scripts.ui.utf8_safe")

-- #1325: ONE module identity per Lua state. Production loads this file
-- through engine.loadScript (scripts/init_loader.lua), whose
-- loadModuleRef runs dofile and deliberately does NOT populate
-- package.loaded (src/Engine/Scripting/Lua/Script.hs) -- while
-- scripts/settings_menu.lua `require`s "scripts.shell". Without the
-- self-registration below those two paths built SEPARATE tables with
-- their own private upvalues: the engine broadcast LuaFramebufferResize
-- to the first, and Settings Defaults/Apply/Save/Back drove the second,
-- whose `shellvisible` is permanently false -- so a scale-only change
-- reached nothing visible and the live console kept stale geometry until
-- the next real resize or shell.show(). Reuse whatever is already
-- cached, otherwise register ourselves, so a later require resolves to
-- this exact table without re-executing this file (same convention as
-- scripts/ui_manager.lua, scripts/unit_ai.lua and scripts/debug.lua --
-- see gotcha_dofile_module_state). The shell deliberately stays OUT of
-- responsive.register/notifyResize (#748 round 7): the engine already
-- broadcasts a real resize straight here, and the shared fan-out would
-- double-fire it.
local shell = package.loaded["scripts.shell"] or {}
package.loaded["scripts.shell"] = shell

-- Script ID (passed from engine)
local myScriptId = nil

-- State
local shellvisible = false
local focusId = nil
local inputBuffer = ""
-- `history` is the visible scrollback: per-entry {command, result,
-- isError}, rebuilt as 9-box-anchored text elements by
-- rebuildHistoryDisplay. Cleared by the `clear` command. NOT
-- persisted — fresh on each session.
local history = {}
-- #1956: cap on the RETAINED scrollback. rebuildHistoryDisplay renders
-- newest-first and stops at the viewport, and no control exposes anything
-- older, so entries past the cap are unreachable memory that every
-- measurement and completion scan still walks. 1000 mirrors the
-- historyMaxLines bound the navigation history already carries -- but
-- deliberately as a SEPARATE constant, because the two lists are bounded
-- for different reasons and either may move without the other. One consequence
-- is intended: shell.getCompletions sources past commands from this list,
-- so an evicted entry also stops being a completion candidate.
local scrollbackMaxEntries = 1000
local historyTextObjects = {}
local lineHeight = 40
local historyPadding = 10
local historyIndex = 0
local savedInputBuffer = ""

-- `arrowHistory` is the up/down navigation list of command strings.
-- Persisted to config/shell_history.txt across sessions (one
-- command per line, oldest first). Dedup'd on push so repeating a
-- command doesn't clutter the buffer — earlier occurrences are
-- pruned and the most recent stays in position. Survives the
-- `clear` command; only `clear-history` wipes it (and the file).
local arrowHistory = {}
local historyFilePath = "config/shell_history.txt"
local historyMaxLines = 1000
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
-- Both are zero-based CODE-POINT offsets into inputBuffer -- see the
-- utf8_safe note at the top of this file.
local cursorPos = 0
local inputScrollOffset = 0

-- Length of the input buffer in code points.
local function bufferLength()
    return utf8Safe.codepointLength(inputBuffer)
end

-- Configuration
local tileSize = 64
-- The PREFERRED center-section width. #1959: this is what the console is
-- drawn at wherever the framebuffer can hold the whole nine-box; where it
-- cannot, shell.getContentWidth() below fits the box instead. Both are the
-- base constant scaled by UI scale (shell.init / shell.rescale).
local middleWidth = 1200
local fontSize = 32

-- System configuration
local uiscale = 1.0

-----------------------------------------------------------
-- Horizontal fit (#1959)
-----------------------------------------------------------

-- The box is laid out as marginLeft + tileSize + center + tileSize, so the
-- widest one that fits a framebuffer of fbWidth has a center of
-- fbWidth - marginLeft - 2 * tileSize. Nothing consulted fbWidth before
-- #1959: at 1x the right edge landed at 40 + 64 + 1200 + 64 = 1368px, and
-- every responsive band's maximum scale multiplies that (up to 5472px at
-- 4x), so the console ran off the side of every display its own envelope
-- declares supported.
--
-- A framebuffer below responsive.MIN_WIDTH (or a scale far past its band)
-- can drive that subtraction negative. Out-of-envelope sizes degrade
-- best-effort, which means a tiny box -- never a negative sprite size or a
-- non-positive text budget, so every fitted width clamps here.
local minCenterWidth = 1

-- The glyphs the input row is laid out around, and the gap left of the
-- prompt and between prompt and input line. Naming the label keeps the
-- element and the width it is measured against from drifting apart.
local promptLabel = "$>"
local inputGap = 10

-- The extra inset a history RESULT line carries over the command line
-- above it. Predates #1959; named because the fitted width clamps against
-- it.
local resultIndent = 20

-- completion state
local currentCompletions = {}
local ghostText = nil

-----------------------------------------------------------
-- Arrow-key history persistence
-----------------------------------------------------------

-- Read the on-disk command history into `arrowHistory`. Silent on
-- missing file (first run). Trims to historyMaxLines just in case
-- the file was hand-edited above the cap.
local function loadArrowHistoryFromDisk()
    local f = io.open(historyFilePath, "r")
    if not f then return end
    arrowHistory = {}
    local dropped = 0
    for line in f:lines() do
        if line ~= "" then
            -- #1187: arrow-up drops a stored line straight into
            -- inputBuffer, whose editing contract is code-point based.
            -- This file is plain text a user can hand-edit or truncate,
            -- and it is the ONE ingress that isn't engine-delivered
            -- text, so a line that isn't valid UTF-8 is refused here
            -- rather than left to blow up mid-edit.
            if utf8.len(line) then
                table.insert(arrowHistory, line)
            else
                dropped = dropped + 1
            end
        end
    end
    f:close()
    if dropped > 0 then
        engine.logWarn("Shell history: skipped " .. tostring(dropped)
            .. " line(s) of invalid UTF-8 in " .. historyFilePath)
    end
    while #arrowHistory > historyMaxLines do
        table.remove(arrowHistory, 1)
    end
end

-- Append a command to the navigation history. Dedup'd: any prior
-- occurrence is removed so the same command never appears twice in
-- arrow-up. Caps at historyMaxLines (oldest dropped). Rewrites the
-- whole file each call — cheap at this scale (~50KB max).
local function pushArrowHistory(cmd)
    if not cmd or cmd == "" then return end
    for i = #arrowHistory, 1, -1 do
        if arrowHistory[i] == cmd then
            table.remove(arrowHistory, i)
        end
    end
    table.insert(arrowHistory, cmd)
    while #arrowHistory > historyMaxLines do
        table.remove(arrowHistory, 1)
    end
    local f = io.open(historyFilePath, "w")
    if f then
        for _, c in ipairs(arrowHistory) do
            f:write(c)
            f:write("\n")
        end
        f:close()
    end
    -- If the open failed (e.g. config/ missing or no write perm)
    -- we silently skip persistence — the in-memory list still
    -- works for the current session.
end

-- Wipe the arrow-key navigation history AND the on-disk file.
-- Does not touch the visible scrollback — that's what `clear`
-- handles. After this, arrow-up yields nothing until the user
-- enters a new command.
local function clearArrowHistory()
    arrowHistory = {}
    os.remove(historyFilePath)
    historyIndex = 0
    savedInputBuffer = ""
end

function shell.init(scriptId)
    myScriptId = scriptId
    engine.logDebug("Shell initializing...")

    loadArrowHistoryFromDisk()
    if #arrowHistory > 0 then
        engine.logDebug("Shell history loaded: "
            .. tostring(#arrowHistory) .. " entries from "
            .. historyFilePath)
    end

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
    
    texBox = engine.loadTexture("assets/textures/ui/box/box.png", "ui")
    texBoxN = engine.loadTexture("assets/textures/ui/box/boxn.png", "ui")
    texBoxS = engine.loadTexture("assets/textures/ui/box/boxs.png", "ui")
    texBoxE = engine.loadTexture("assets/textures/ui/box/boxe.png", "ui")
    texBoxW = engine.loadTexture("assets/textures/ui/box/boxw.png", "ui")
    texBoxNE = engine.loadTexture("assets/textures/ui/box/boxne.png", "ui")
    texBoxNW = engine.loadTexture("assets/textures/ui/box/boxnw.png", "ui")
    texBoxSE = engine.loadTexture("assets/textures/ui/box/boxse.png", "ui")
    texBoxSW = engine.loadTexture("assets/textures/ui/box/boxsw.png", "ui")
    
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
    -- Same reason as shell.onFramebufferResize (#1959): opening after a
    -- resize taken while the console was hidden -- or after a scale change,
    -- which destroys the elements above -- reaches the same rebuild with a
    -- retained input buffer, so the input line, cursor and ghost have to be
    -- re-derived from the width the box was just drawn at.
    shell.updateDisplay()
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

-- Read-only introspection of the input line: the raw buffer text plus the
-- cursor and scroll offsets, both zero-based CODE-POINT offsets into it
-- (#1187). Exists so the editing contract can be asserted from the debug
-- console and from headless tests without reaching into these upvalues.
function shell.getInputState()
    return inputBuffer, cursorPos, inputScrollOffset
end

function shell.onChar(char)
    historyIndex = 0
    savedInputBuffer = ""
    -- Insert at cursor position. `char` arrives from LuaCharInput as one
    -- Haskell Char (Engine.Scripting.Lua.Thread.Dispatch), so it is one
    -- code point of one to four bytes -- advance by code points, not by 1
    -- byte, or the next edit cuts the character in half.
    local before = utf8Safe.prefix(inputBuffer, cursorPos)
    local after = utf8Safe.suffix(inputBuffer, cursorPos)
    inputBuffer = before .. char .. after
    cursorPos = cursorPos + utf8Safe.codepointLength(char)
    shell.updateDisplay()
end

function shell.onBackspace()
    if cursorPos > 0 then
        local before = utf8Safe.prefix(inputBuffer, cursorPos - 1)
        local after = utf8Safe.suffix(inputBuffer, cursorPos)
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
    -- Push onto the navigation history before dispatching the
    -- command — even commands that error should appear in arrow-up
    -- so the user can edit and re-run.
    pushArrowHistory(inputBuffer)
    local cmd = string.lower(string.match(inputBuffer, "^%s*(%S+)") or "")
    if cmd == "help" then
        shell.addHistory(inputBuffer, shell.cmdHelp(), false)
    elseif cmd == "clear" then
        shell.addHistory(inputBuffer, "OK", false)
        shell.cmdClear()
    elseif cmd == "clear-history" then
        shell.cmdClearHistory()
        shell.addHistory(inputBuffer, "OK", false)
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
    return "Commands: help, clear, clear-history, quit/exit\n"
        .. "Or enter Lua code to execute"
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

-- Wipe the arrow-key navigation history AND the persisted file.
-- Doesn't touch the visible scrollback (use `clear` for that).
-- Useful when accumulated history gets noisy or contains sensitive
-- state you don't want to keep across sessions.
function shell.cmdClearHistory()
    clearArrowHistory()
    engine.logInfo("Shell history cleared")
end

function shell.cmdQuit()
    engine.logDebug("Quit requested from shell")
    engine.quit()
end

function shell.rebuildHistoryDisplay()
    for _, obj in ipairs(historyTextObjects) do
        UI.deleteElement(obj)
    end
    historyTextObjects = {}
    
    if not shellvisible then return end
    if #history == 0 then return end
    
    local baseX = marginLeft
    local textX = baseX + tileSize + historyPadding
    -- #1959: resolve the fitted budgets ONCE, before any wrap below, so the
    -- rendered history measures against the center the box is drawn at.
    local maxTextWidth = shell.getHistoryTextWidth()
    local maxResultWidth = shell.getResultTextWidth()
    
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
            
            local resultLines = shell.wrapText(entry.result, maxResultWidth, shellFont)
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
    local _, fbHeight = engine.getFramebufferSize()
    local boxHeight = shell.calculateBoxHeight()
    -- #1959: one fitted center width for the whole rebuild -- every sprite
    -- size and every edge position below reads this, never the preferred
    -- (unfitted) width the base constant scales to.
    local centerWidth = shell.getContentWidth()
    local middleHeight = boxHeight - tileSize * 2
    local baseX = marginLeft
    local baseY = fbHeight - marginBottom - boxHeight
    
    -- Calculate positions
    local row0Y = baseY + tileSize / 2
    local middleY = baseY + tileSize + middleHeight / 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    local promptY = row2Y - fontSize
    local promptX, bufferX, _, promptVisible = shell.getInputLayout()
    
    if not boxSpawned then
        -- First time: create all UI elements
        objBoxNW = UI.newSprite("shell_nw", tileSize, tileSize, texBoxNW, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxN  = UI.newSprite("shell_n", centerWidth, tileSize, texBoxN, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxNE = UI.newSprite("shell_ne", tileSize, tileSize, texBoxNE, 1.0, 1.0, 1.0, 1.0, shellPage)
        
        objBoxW  = UI.newSprite("shell_w", tileSize, middleHeight, texBoxW, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBox   = UI.newSprite("shell_c", centerWidth, middleHeight, texBox, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxE  = UI.newSprite("shell_e", tileSize, middleHeight, texBoxE, 1.0, 1.0, 1.0, 1.0, shellPage)
        
        objBoxSW = UI.newSprite("shell_sw", tileSize, tileSize, texBoxSW, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxS  = UI.newSprite("shell_s", centerWidth, tileSize, texBoxS, 1.0, 1.0, 1.0, 1.0, shellPage)
        objBoxSE = UI.newSprite("shell_se", tileSize, tileSize, texBoxSE, 1.0, 1.0, 1.0, 1.0, shellPage)
        
        -- ... rest of addToPage calls unchanged ...
        
        UI.addToPage(shellPage, objBoxNW, baseX, row0Y - tileSize / 2)
        UI.addToPage(shellPage, objBoxN,  baseX + tileSize, row0Y - tileSize / 2)
        UI.addToPage(shellPage, objBoxNE, baseX + tileSize + centerWidth, row0Y - tileSize / 2)
        
        UI.addToPage(shellPage, objBoxW,  baseX, row0Y + tileSize / 2)
        UI.addToPage(shellPage, objBox,   baseX + tileSize, row0Y + tileSize / 2)
        UI.addToPage(shellPage, objBoxE,  baseX + tileSize + centerWidth, row0Y + tileSize / 2)
        
        UI.addToPage(shellPage, objBoxSW, baseX, row0Y + tileSize / 2 + middleHeight)
        UI.addToPage(shellPage, objBoxS,  baseX + tileSize, row0Y + tileSize / 2 + middleHeight)
        UI.addToPage(shellPage, objBoxSE, baseX + tileSize + centerWidth, row0Y + tileSize / 2 + middleHeight)
        
        objPrompt = UI.newText("shell_prompt", promptLabel, shellFont, fontSize, 1.0, 1.0, 1.0, 1.0, shellPage)
        UI.addToPage(shellPage, objPrompt, promptX, promptY)
        UI.setVisible(objPrompt, promptVisible)
        
        objBufferText = UI.newText("shell_buffer", inputBuffer, shellFont, fontSize, 1.0, 1.0, 1.0, 1.0, shellPage)
        UI.addToPage(shellPage, objBufferText, bufferX, promptY)
        
        objCursor = UI.newText("shell_cursor", "|", shellFont, fontSize, 1.0, 1.0, 1.0, 1.0, shellPage)
        UI.addToPage(shellPage, objCursor, bufferX, promptY)
        
        boxSpawned = true
    else
        -- Reposition and resize existing elements.
        --
        -- The four CORNER sprites and every text element are created at the
        -- then-current tileSize/fontSize and never resized here, which is
        -- sound only because neither can have changed since: every
        -- shell.rescale() that moves them is followed by a
        -- destroyAllElements() -- in shell.show() when the scale changed,
        -- and in shell.onFramebufferResize() on both its branches (#1959).
        
        -- Top row
        UI.setPosition(objBoxNW, baseX, row0Y - tileSize / 2)
        UI.setPosition(objBoxN,  baseX + tileSize, row0Y - tileSize / 2)
        UI.setSize(objBoxN, centerWidth, tileSize)
        UI.setPosition(objBoxNE, baseX + tileSize + centerWidth, row0Y - tileSize / 2)
        
        -- Middle row - reposition and resize
        UI.setPosition(objBoxW,  baseX, row0Y + tileSize / 2)
        UI.setSize(objBoxW, tileSize, middleHeight)
        UI.setPosition(objBox,   baseX + tileSize, row0Y + tileSize / 2)
        UI.setSize(objBox, centerWidth, middleHeight)
        UI.setPosition(objBoxE,  baseX + tileSize + centerWidth, row0Y + tileSize / 2)
        UI.setSize(objBoxE, tileSize, middleHeight)
        
        -- Bottom row
        UI.setPosition(objBoxSW, baseX, row0Y + tileSize / 2 + middleHeight)
        UI.setPosition(objBoxS,  baseX + tileSize, row0Y + tileSize / 2 + middleHeight)
        UI.setSize(objBoxS, centerWidth, tileSize)
        UI.setPosition(objBoxSE, baseX + tileSize + centerWidth, row0Y + tileSize / 2 + middleHeight)
        
        -- Prompt
        UI.setPosition(objPrompt, promptX, promptY)
        UI.setVisible(objPrompt, promptVisible)
        UI.setPosition(objBufferText, bufferX, promptY)
        UI.setVisible(objCursor, cursorVisible)
        shell.updateCursorPos()
    end
end

function shell.updateCursorPos()
    if not objCursor then return end
    if not shellvisible then return end
    
    local _, fbHeight = engine.getFramebufferSize()
    local boxHeight = shell.calculateBoxHeight()
    local baseY = fbHeight - marginBottom - boxHeight
    local middleHeight = boxHeight - tileSize * 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    local promptY = row2Y - fontSize
    local _, bufferX = shell.getInputLayout()
    
    -- Only measure text up to cursor position, as whole code points --
    -- a byte slice here would hand engine.getTextWidth half a character.
    local textBeforeCursor = utf8Safe.slice(inputBuffer, inputScrollOffset, cursorPos)
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
    -- #1956: drop from the OLD end, so the entry just appended -- the one
    -- the user is about to read -- is always the one that survives.
    while #history > scrollbackMaxEntries do
        table.remove(history, 1)
    end
    if shellvisible then
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
    end
end

function shell.getPromptY()
    local _, fbHeight = engine.getFramebufferSize()
    local boxHeight = shell.calculateBoxHeight()
    local baseY = fbHeight - marginBottom - boxHeight
    local middleHeight = boxHeight - tileSize * 2
    local row2Y = baseY + tileSize + middleHeight + tileSize / 2
    return row2Y - fontSize
end

-- The center-section width the console is actually DRAWN at: the preferred
-- `middleWidth` wherever marginLeft + 2 * tileSize + middleWidth fits the
-- framebuffer, and the widest fitting center otherwise (#1959 requirement
-- 4 -- nothing narrows on a display that fits the preferred width today).
--
-- Resolved from the LIVE framebuffer on every call rather than memoized:
-- with no retained width there is nothing for a framebuffer-width change
-- to leave stale, which is what the old `maxInputWidth` memo got wrong --
-- it was reset only from shell.rescale and shell.onFramebufferResize, so a
-- width change observed through any other path still measured against the
-- previous framebuffer.
function shell.getContentWidth()
    local fbWidth = engine.getFramebufferSize()
    local fitted = (fbWidth or 0) - marginLeft - tileSize * 2
    if fitted > middleWidth then fitted = middleWidth end
    if fitted < minCenterWidth then fitted = minCenterWidth end
    return math.floor(fitted)
end

-- Horizontal budget for one wrapped history COMMAND line: the fitted
-- center less its padding on both sides.
function shell.getHistoryTextWidth()
    return math.max(minCenterWidth,
                    shell.getContentWidth() - historyPadding * 2)
end

-- Same, for a history RESULT line, which is indented one `resultIndent`
-- further than the command above it.
function shell.getResultTextWidth()
    return math.max(minCenterWidth,
                    shell.getHistoryTextWidth() - resultIndent)
end

-- Where the prompt and the input line sit inside the fitted center, and
-- how much horizontal budget the input line has there. ONE derivation,
-- shared by shell.rebuildBox (which PLACES prompt, input and cursor),
-- shell.updateCursorPos, shell.updateGhostText and shell.getMaxInputWidth
-- (which MEASURE against them). Three of those recomputed this arithmetic
-- privately before #1959, and the budget was a flat 100px inset with no
-- relation to where the input actually starts -- so at a narrow fitted
-- center the reported budget described a row the box was not drawing.
--
-- Returns promptX, bufferX, the input's width budget, and whether the
-- prompt fits at all. The ladder degrades rather than overrunning the
-- right edge tile: full spacing, then no spacing, then -- when the prompt
-- alone is as wide as the whole fitted center, which 800x1601 at 4x really
-- is -- no prompt at all, and the input takes the entire interior.
function shell.getInputLayout()
    local left = marginLeft + tileSize
    local right = left + shell.getContentWidth()
    local promptWidth =
        engine.getTextWidth(shellFont, promptLabel, fontSize) or 0

    local promptX = left + inputGap
    local bufferX = promptX + promptWidth + inputGap
    if bufferX + minCenterWidth <= right then
        return promptX, bufferX, math.max(minCenterWidth, right - bufferX), true
    end

    promptX = left
    bufferX = left + promptWidth
    if bufferX + minCenterWidth <= right then
        return promptX, bufferX, math.max(minCenterWidth, right - bufferX), true
    end

    return left, left, math.max(minCenterWidth, right - left), false
end

-- Horizontal budget for the input line -- and, through
-- shell.updateGhostText, for the completion hint that trails it.
function shell.getMaxInputWidth()
    local _, _, width = shell.getInputLayout()
    return width
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
    
    -- Check if cursor is past right edge. The scroll offset advances one
    -- CODE POINT at a time so every measured slice is whole characters.
    local widthFromScrollToCursor = engine.getTextWidth(shellFont,
        utf8Safe.slice(inputBuffer, inputScrollOffset, cursorPos), fontSize)

    -- If cursor goes past right edge, scroll right. Bounded by the cursor:
    -- once the slice is empty there is nothing left to scroll away, and a
    -- degenerate (non-positive) maxWidth must not spin forever.
    while widthFromScrollToCursor > maxWidth and inputScrollOffset < cursorPos do
        inputScrollOffset = inputScrollOffset + 1
        widthFromScrollToCursor = engine.getTextWidth(shellFont,
            utf8Safe.slice(inputBuffer, inputScrollOffset, cursorPos), fontSize)
    end
end

-- Get the visible portion of input buffer
function shell.getVisibleInput()
    local maxWidth = shell.getMaxInputWidth()
    local visibleText = utf8Safe.suffix(inputBuffer, inputScrollOffset)

    -- Trim to fit width, testing whole-code-point prefixes only: a
    -- byte-length prefix walk hands partial sequences to getTextWidth and
    -- can return a fragment for the buffer text element to render.
    for i = utf8Safe.codepointLength(visibleText), 1, -1 do
        local test = utf8Safe.prefix(visibleText, i)
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
    
    local _, fbHeight = engine.getFramebufferSize()
    local maxHeight = fbHeight - marginTop - marginBottom

    -- #1956: walk NEWEST-first -- the order rebuildHistoryDisplay renders
    -- in -- and stop the moment the running total has reached the height
    -- this is clamped to anyway. Measuring one entry costs one
    -- engine.getTextWidth call per code point, so summing the whole
    -- retained history put every keystroke's cost in proportion to the
    -- session's total output. The answer is unchanged either way: a
    -- history that fits is still summed in full, and one that overflows
    -- still returns maxHeight, whichever entries carried it there.
    local maxTextWidth = shell.getHistoryTextWidth()
    local historyLines = 0
    for i = #history, 1, -1 do
        historyLines = historyLines
            + shell.countLinesForEntry(history[i], maxTextWidth, shellFont)
        if baseHeight + historyLines * lineHeight >= maxHeight then
            return maxHeight
        end
    end

    return baseHeight + historyLines * lineHeight
end

-- Longest common prefix of the completion candidates, cut at a code-point
-- boundary. Two candidates can agree on part of a multi-byte character --
-- `a🙂x` and `a🙃y` share three of the emoji's four bytes -- so the raw
-- byte-wise agreement point is snapped back to the last whole character
-- before it becomes ghost text, a Tab insertion, or a measured string.
-- The candidates come from sandbox key names and past commands, so
-- snapping (which never raises) is used rather than utf8_safe's asserting
-- code-point walk.
function shell.longestCommonPrefix(strings)
    if #strings == 0 then return "" end
    if #strings == 1 then return strings[1] end

    local base = strings[1]
    local n = #base
    for i = 2, #strings do
        local s = strings[i]
        local j = 0
        while j < n and j < #s and base:byte(j + 1) == s:byte(j + 1) do
            j = j + 1
        end
        n = j
        if n == 0 then return "" end
    end
    return base:sub(1, utf8Safe.snapToCharBoundary(base, n))
end

-- The part of `completion` past the already-typed `prefix`, which
-- getCurrentWord only ever produces as an ASCII run. longestCommonPrefix
-- has already cut at a character boundary; refusing a fragment that isn't
-- valid UTF-8 on its own additionally keeps a hand-installed non-UTF-8
-- global name out of the input buffer and away from engine.getTextWidth.
local function completionSuffix(completion, prefix)
    local addition = completion:sub(#prefix + 1)
    if addition == "" or utf8.len(addition) == nil then return "" end
    return addition
end

-- Get the current word being typed (handles engine.xxx)
function shell.getCurrentWord()
    return inputBuffer:match("[%w_%.]+$") or ""
end

-- Wrap text into multiple lines that fit within maxWidth, breaking between
-- Unicode CODE POINTS -- console output is arbitrary text (Lua values, error
-- messages), so a byte-wise break would split multi-byte characters. The
-- shared implementation lives in scripts/ui/text_wrap.lua; the word-wrapping
-- variant beside it is what the log panels use.
function shell.wrapText(text, maxWidth, font)
    return textWrap.byCharacter(text, maxWidth, font, fontSize)
end

-- Count lines needed for a history entry
function shell.countLinesForEntry(entry, maxWidth, font)
    local lines = 0
    
    local cmdText = "$> " .. entry.command
    lines = lines + #shell.wrapText(cmdText, maxWidth, font)
    
    if entry.result and entry.result ~= "" and entry.result ~= "nil" then
        lines = lines + #shell.wrapText(entry.result,
            math.max(minCenterWidth, maxWidth - resultIndent), font)
    end
    
    return lines
end

-- Member completion for a `table.` prefix.
--
-- #1958: the member set comes from shellSandbox's own copy of the table
-- and from nowhere else. setupShellSandbox shallow-copies each engine API
-- table before scripts/init.lua runs, so _G.engine and
-- shellSandbox.engine are DISTINCT tables whose contents can diverge --
-- resolving _G first (as this did) could offer a member the console's
-- `_ENV` does not have. shellSandbox is also what the console assigns
-- into, so a member added from the console completes here too.
function shell.getTableCompletions(tableName, memberPrefix)
    local results = {}
    local tbl = shellSandbox and shellSandbox[tableName]
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
        
        -- Names visible to the console at execution time. This is
        -- shellSandbox and NOT _G (#1958): engine.shellExecute installs
        -- shellSandbox as the chunk's `_ENV`, so a _G-only name -- `io`,
        -- `require`, or any of the sixteen engine API tables the sandbox
        -- used to omit -- is a suggestion that cannot run. It is also the
        -- table console assignments land in, so a name defined from the
        -- console is completable even though _G never sees it.
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
    if cursorPos ~= bufferLength() then
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
        local ghostPart = completionSuffix(commonPrefix, prefix)
        local maxWidth = shell.getMaxInputWidth()
        local currentWidth = engine.getTextWidth(shellFont,
            utf8Safe.suffix(inputBuffer, inputScrollOffset), fontSize)
        local ghostWidth = engine.getTextWidth(shellFont, ghostPart, fontSize)

        if ghostPart ~= "" and (currentWidth + ghostWidth) <= maxWidth then
            if not ghostText then
                ghostText = UI.newText("shell_ghost", ghostPart, shellFont, fontSize, 0.5, 0.5, 0.5, 0.5, shellPage)
                UI.addToPage(shellPage, ghostText, 0, 0)
            else
                UI.setText(ghostText, ghostPart)
                UI.setVisible(ghostText, true)
            end
            
            -- Position after cursor
            local _, fbHeight = engine.getFramebufferSize()
            local boxHeight = shell.calculateBoxHeight()
            local baseY = fbHeight - marginBottom - boxHeight
            local middleHeight = boxHeight - tileSize * 2
            local row2Y = baseY + tileSize + middleHeight + tileSize / 2
            local promptY = row2Y - fontSize
            local _, bufferX = shell.getInputLayout()
            local textWidth = engine.getTextWidth(shellFont,
                utf8Safe.suffix(inputBuffer, inputScrollOffset), fontSize)
            local cursorWidth = engine.getTextWidth(shellFont, "|", fontSize)
            local cursorX = bufferX + textWidth
            
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
    local addition = completionSuffix(commonPrefix, prefix)

    if addition ~= "" then
        local before = utf8Safe.prefix(inputBuffer, cursorPos)
        local after = utf8Safe.suffix(inputBuffer, cursorPos)
        inputBuffer = before .. addition .. after
        -- Code points, not #addition: a completion can carry multi-byte
        -- characters (a past command, a non-ASCII table key).
        cursorPos = cursorPos + utf8.len(addition)
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
    if #arrowHistory == 0 then return end

    -- Save current input when starting to browse
    if historyIndex == 0 then
        savedInputBuffer = inputBuffer
    end

    -- Move up in history (towards older commands)
    if historyIndex < #arrowHistory then
        historyIndex = historyIndex + 1
        inputBuffer = arrowHistory[#arrowHistory - historyIndex + 1]
        cursorPos = bufferLength()
        shell.updateDisplay()
    end
end

function shell.onCursorDown(fid)
    if fid ~= focusId then return end

    if historyIndex > 1 then
        -- Move down in history (towards newer commands)
        historyIndex = historyIndex - 1
        inputBuffer = arrowHistory[#arrowHistory - historyIndex + 1]
        cursorPos = bufferLength()
        shell.updateDisplay()
    elseif historyIndex == 1 then
        -- Back to saved input
        historyIndex = 0
        inputBuffer = savedInputBuffer
        cursorPos = bufferLength()
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
    if fid == focusId and cursorPos < bufferLength() then
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
        cursorPos = bufferLength()
        shell.updateCursorPos()
        shell.updateDisplay()
    end
end

function shell.onDelete(fid)
    if fid == focusId and cursorPos < bufferLength() then
        local before = utf8Safe.prefix(inputBuffer, cursorPos)
        local after = utf8Safe.suffix(inputBuffer, cursorPos + 1)
        inputBuffer = before .. after
        shell.updateDisplay()
    end
end

-- The Delete key's production entry point: Engine.Scripting.Lua.Thread.Dispatch
-- broadcasts LuaTextDelete as "onTextDelete", matching the onTextBackspace /
-- onCharInput naming above. onDelete stays the implementation (and keeps its
-- own focus check) so console-driven callers keep working.
function shell.onTextDelete(fid)
    shell.onDelete(fid)
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
    -- #748 round 10: shell receives LuaFramebufferResize straight from
    -- the engine (Engine.Scripting.Lua.Thread.Dispatch broadcasts to
    -- every loaded script directly), which is exactly why round 7
    -- deliberately did NOT also route it through
    -- responsive.notifyResize (that would double-fire this handler on
    -- every real resize) — but it also means shell never got
    -- notifyResize's 0x0-minimize guard. A minimize would otherwise
    -- destroy+rebuild an already-visible shell against a degenerate
    -- 0x0 framebuffer (rebuildBox/rebuildHistoryDisplay read
    -- engine.getFramebufferSize() directly, producing negative/
    -- off-frame geometry). Skip entirely on a non-positive size — the
    -- very next real-size resize rebuilds normally on its own, since
    -- shellvisible is untouched by a minimize.
    if (width or 0) <= 0 or (height or 0) <= 0 then
        return
    end

    -- #1959: nothing to invalidate here any more. This used to reset the
    -- `maxInputWidth` memo, which was the ONLY width a resize refreshed --
    -- the box geometry and the history budgets were recomputed from UI
    -- scale alone, so a framebuffer-width change with an unchanged scale
    -- reached none of them. Every width now derives from
    -- shell.getContentWidth(), which reads the live framebuffer.

    -- #748 round 6: shell was never registered with the shared
    -- responsive.notifyResize contract — a UI-scale Apply/Save (same
    -- framebuffer size, new scale) never reached it at all; it only
    -- ever rescaled lazily the next time shell.show() ran rescale()
    -- itself. Call rescale() here too so an already-visible shell
    -- picks up a live scale change immediately, not just on next open.
    local scaleChanged = shell.rescale()

    -- If visible, rebuild everything with new dimensions
    if shellvisible then
        shell.destroyAllElements()
        boxSpawned = false
        shell.rebuildBox()
        shell.rebuildHistoryDisplay()
        -- #1959: rebuildBox's CREATE branch spawns shell_buffer from the
        -- RAW inputBuffer and parks the cursor at bufferX, because it has
        -- no idea what the scroll window is. Without this the resize path
        -- left a long input drawn unscrolled past the newly fitted width,
        -- the cursor at the start of the line, and no ghost at all (it was
        -- just destroyed) until the next keystroke happened to redraw
        -- them. updateDisplay is the one path that re-derives all three
        -- from the new width -- including re-scrolling the window, which
        -- is what keeps the cursor inside a field that just got narrower.
        shell.updateDisplay()
    elseif scaleChanged then
        -- #1959: rescale() above runs whether or not the console is open,
        -- but a HIDDEN one has no rebuild to follow it -- shell.hide keeps
        -- every element and leaves boxSpawned true. The next show() then
        -- finds nothing left to rescale and takes rebuildBox's
        -- existing-element branch, which repositions from the new tileSize
        -- while the corner sprites keep the old one (and every text element
        -- keeps the old font size): apply 1x after opening at 4x and
        -- shell_ne is still 256px wide, hanging 192px past a 1280px
        -- framebuffer. Drop the stale elements so the next show() builds
        -- them at the scale that is now current.
        --
        -- A framebuffer-size change with no scale change needs nothing
        -- here: tileSize and fontSize are unchanged, and rebuildBox's
        -- existing-element branch already resizes and repositions
        -- everything the fitted center governs.
        shell.destroyAllElements()
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
    
    -- `middleWidth` is the PREFERRED center only (#1959); the width the
    -- box is drawn at is fitted per-framebuffer by shell.getContentWidth().
    
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
