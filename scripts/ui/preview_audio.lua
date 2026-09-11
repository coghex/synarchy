-- Audio authoring pane. All playback goes through the engine's audio worker.
-- Text controls need no additional textures in a trimmed visual preview.
local wrap = require("scripts.ui.text_wrap")
local pane = {}
local font, basePage, navPage, page, footer, statusLabel
local elements, actions, navElements = {}, {}, {}
local opened, audioOnly = false, false
local category, selected, entries, offset = "synth", nil, {}, 0
local width, height, rowCount = 800, 600, 8
local snapshot, reloadSequence, playSequence, autoplayPath
local message = "Choose a sound"
local rows, controls = {}, {}
local statusSize, statusWidth = 16, 1

local function status()
    return audio and audio.getStatus and audio.getStatus() or
        {lifecycle="disabled", previewEntries={}, snapshotSequence=0}
end

local function filtered()
    local result = {}
    for _, entry in ipairs(entries) do
        if entry.category == category then result[#result + 1] = entry end
    end
    return result
end

local function chosen()
    for _, entry in ipairs(entries) do
        if entry.id == selected then return entry end
    end
end

local function identity(entry)
    return entry and (entry.category .. ":" .. (entry.path or entry.label))
end

local function clear()
    for _, handle in ipairs(elements) do UI.deleteElement(handle) end
    elements, actions, rows, controls = {}, {}, {}, {}
    statusLabel = nil
end

local function text(name, label, x, y, size, action, maxWidth)
    label = wrap.truncateToWidth(label, font, size, maxWidth or math.max(1, width - x - 24))
    local handle = UI.newText(name, label, font, size, 1, 1, 1, 1, page)
    elements[#elements + 1] = handle
    UI.addToPage(page, handle, x, y)
    if not action then return handle end
    -- Text positions are baselines and their bounds are zero-sized. Give the
    -- visible glyphs a real, independently clickable rectangle above it.
    local hit = UI.newElement(name .. "_hit", math.max(1, engine.getTextWidth(font, label, size)), size + 4, page)
    elements[#elements + 1] = hit
    UI.addToPage(page, hit, x, y - size)
    UI.setClickable(hit, true)
    UI.setOnClick(hit, "onPreviewAudioClick")
    actions[hit] = action
    controls[name] = hit
    return hit
end

function pane.play()
    local entry = chosen()
    if reloadSequence or not entry then return false end
    if not entry.playable then
        message = "Could not load: " .. ((snapshot or {}).lastError or entry.label)
        return false
    end
    playSequence = (snapshot or status()).snapshotSequence or 0
    if audio.previewPlay(entry.id) then message = "Queued"; return true end
    message = "Audio unavailable"
    return false
end

function pane.stop()
    if audio and audio.previewStop then audio.previewStop() end
    playSequence = nil
    message = "Stopped"
end

function pane.reload()
    if reloadSequence then return false end
    if not (audio and audio.previewReload) then return false end
    local before = status()
    if not audio.previewReload() then return false end
    reloadSequence = before.previewRevision or 0
    playSequence = nil
    message = "Reloading sounds..."
    return true
end

function pane.select(id, play)
    selected = id
    message = "Ready"
    pane.render()
    if play then return pane.play() end
    return true
end

function pane.chooseCategory(value)
    pane.stop()
    category, offset = value, 0
    local list = filtered()
    selected = list[1] and list[1].id or nil
    message = selected and "Ready" or "No sounds in this category"
    pane.render()
end

function pane.render()
    if not opened or not page then return end
    clear()
    local size = math.max(12, math.min(28, math.floor(width / 70)))
    local left, right = 28, math.floor(width * 0.52)
    local line = math.max(28, size + 12)
    rowCount = math.max(1, math.floor((height - 240) / line))
    local list = filtered()
    offset = math.min(offset, math.max(0, #list - rowCount))
    text("preview_audio_title", "Audio", left, 42, size + 8)
    local synthWidth = engine.getTextWidth(font, "[ Synth ]", size)
    text("preview_audio_synth", category == "synth" and "[ Synth ]" or "Synth", left, 84, size,
        function() pane.chooseCategory("synth") end)
    text("preview_audio_files", category == "files" and "[ Files ]" or "Files", left + synthWidth + 32, 84, size,
        function() pane.chooseCategory("files") end)
    for index = offset + 1, math.min(#list, offset + rowCount) do
        local entry = list[index]
        local label = (entry.id == selected and "> " or "  ") .. entry.label
            .. (entry.playable and "" or " (unavailable)")
        local handle = text("preview_audio_row_" .. index, label, left,
            128 + (index - offset - 1) * line, size,
            function() pane.select(entry.id, true) end, right - left - 32)
        rows[#rows + 1] = {id=entry.id, label=entry.label, handle=handle, playable=entry.playable}
    end
    if #list == 0 then
        text("preview_audio_empty", category == "files" and "Add WAV files to assets/audio" or
            "Add synth sounds to data/audio", left, 128, size, nil, right - left - 32)
    end
    if #list > rowCount then
        text("preview_audio_previous", "Previous", left, height - 110, size,
            function() offset = math.max(0, offset - rowCount); pane.render() end, 140)
        text("preview_audio_next", "Next", left + 170, height - 110, size,
            function() offset = math.min(math.max(0, #list-rowCount), offset+rowCount); pane.render() end, 100)
    end
    local entry = chosen()
    text("preview_audio_selected", entry and entry.label or "Select a sound", right, 128, size + 2)
    text("preview_audio_source", entry and (entry.path or "Engine synth") or "", right, 166, size - 2)
    text("preview_audio_play", "Play", right, 224, size, pane.play)
    text("preview_audio_stop", "Stop", right + engine.getTextWidth(font, "Play", size) + 48, 224, size, pane.stop)
    text("preview_audio_reload", "Reload", right, 282, size, pane.reload)
    text("preview_audio_reload_help", "Reload after editing a sound", right, 316, size - 2)
    statusLabel = text("preview_audio_status", message, right, 362, size - 2)
    statusSize, statusWidth = size - 2, math.max(1, width - right - 24)
    local output = snapshot and snapshot.lifecycle or "disabled"
    if output ~= "running_real" then
        text("preview_audio_output", output == "running_null" and "Silent test output" or
            "Audio output unavailable", right, 398, size - 2)
    end
    local volumes = snapshot and snapshot.volumes
    if volumes then
        local function volume(bus, delta)
            local current = status().volumes
            if not current then return end
            current[bus] = math.max(0, math.min(100, current[bus] + delta))
            audio.setVolumes(current)
            snapshot = status()
            pane.render()
        end
        local masterWidth = engine.getTextWidth(font, "Master 100%", size)
        local uiWidth = engine.getTextWidth(font, "UI 100%", size)
        text("preview_audio_volume", "Master " .. volumes.master .. "%", left, height - 60, size)
        text("preview_audio_quieter", "-", left + masterWidth + 24, height - 60, size,
            function() volume("master", -10) end)
        text("preview_audio_louder", "+", left + masterWidth + 76, height - 60, size,
            function() volume("master", 10) end)
        text("preview_audio_ui_volume", "UI " .. volumes.ui .. "%", right, height - 60, size)
        text("preview_audio_ui_quieter", "-", right + uiWidth + 24, height - 60, size,
            function() volume("ui", -10) end)
        text("preview_audio_ui_louder", "+", right + uiWidth + 76, height - 60, size,
            function() volume("ui", 10) end)
    end
end

function pane.open(value, file)
    if not page then page = UI.newPage("preview_audio", "menu") end
    category = value or category
    snapshot = status()
    entries = snapshot.previewEntries or {}
    local list = filtered()
    local current = chosen()
    if not current or current.category ~= category then selected = list[1] and list[1].id end
    if file then
        for _, entry in ipairs(entries) do if entry.path == file then selected = entry.id end end
        autoplayPath = file
    end
    opened = true
    UI.hidePage(basePage)
    UI.showPage(page)
    pane.render()
end

function pane.close()
    if audioOnly then return end
    pane.stop()
    autoplayPath = nil
    opened = false
    if page then UI.hidePage(page) end
    UI.showPage(basePage)
end

function pane.init(labelFont, visualPage, browse)
    font, basePage = labelFont, visualPage
    audioOnly = browse and browse.mode == "audio" or false
    width, height = engine.getFramebufferSize()
    navPage = UI.newPage("preview_audio_navigation", "menu")
    local label = UI.newText("preview_audio_button_label", "[ Audio ]", font, 24, 1, 1, 1, 1, navPage)
    footer = UI.newElement("preview_audio_button", math.max(1, engine.getTextWidth(font, "[ Audio ]", 24)), 28, navPage)
    navElements = {label, footer}
    UI.addToPage(navPage, footer, 12, math.max(0, height - 36))
    UI.addChild(footer, label, 0, 24)
    UI.setClickable(footer, true)
    UI.setOnClick(footer, "onPreviewAudioClick")
    UI.showPage(navPage)
    if audioOnly then pane.open(browse.category, browse.file) end
end

function pane.click(handle)
    if handle == footer then
        if opened and not audioOnly then pane.close() else pane.open() end
        return true
    end
    local action = opened and actions[handle]
    if action then action(); return true end
    return false
end

function pane.key(key)
    if not opened then return false end
    if key == "Escape" then if audioOnly then engine.quit() else pane.close() end
    elseif key == "Space" then pane.play()
    elseif key == "Up" or key == "Down" then
        local list = filtered()
        for index, entry in ipairs(list) do
            if entry.id == selected then
                local nextIndex = math.max(1, math.min(#list, index + (key == "Up" and -1 or 1)))
                offset = math.max(0, math.min(offset, nextIndex - 1))
                if nextIndex > offset + rowCount then offset = nextIndex - rowCount end
                pane.select(list[nextIndex].id, true)
                break
            end
        end
    end
    return true
end

function pane.update()
    if not opened then return end
    local latest = status()
    local sequence = latest.snapshotSequence or 0
    if reloadSequence and (latest.previewRevision or 0) > reloadSequence and latest.lifecycle == "disabled" then
        reloadSequence = nil
        message = "Audio unavailable: " .. (latest.lastError or "")
    elseif reloadSequence and (latest.previewRevision or 0) > reloadSequence and latest.lifecycle ~= "starting" then
        local old = identity(chosen())
        entries = latest.previewEntries or {}
        selected = nil
        for _, entry in ipairs(entries) do if identity(entry) == old then selected = entry.id end end
        local list = filtered()
        selected = selected or (list[1] and list[1].id)
        reloadSequence, playSequence = nil, nil
        message = (latest.catalogWarnings or 0) > 0 and "Some sounds unavailable: " .. (latest.lastError or "") or "Ready"
        snapshot = latest
        pane.render()
    elseif playSequence and sequence > playSequence then
        message = (latest.native and latest.native.activeVoices or 0) > 0 and "Playing" or "Ready"
    end
    snapshot = latest
    if autoplayPath and latest.lifecycle ~= "starting" then
        autoplayPath = nil
        pane.play()
    end
    if statusLabel then UI.setText(statusLabel, wrap.truncateToWidth(message, font, statusSize, statusWidth)) end
end

function pane.resize(w, h)
    width, height = math.max(1, w), math.max(1, h)
    if footer then UI.setPosition(footer, 12, math.max(0, height - 36)) end
    pane.render() -- Geometry only: playback, category and selection survive.
end

function pane.isOpen() return opened end

function pane.dump()
    local function info(handle) return handle and UI.getElementInfo(handle) end
    local visibleRows, buttons = {}, {}
    for _, row in ipairs(rows) do
        visibleRows[#visibleRows + 1] = {id=row.id, label=row.label, playable=row.playable,
            handle=row.handle, bounds=info(row.handle)}
    end
    for name, handle in pairs(controls) do buttons[name] = {handle=handle, bounds=info(handle)} end
    return {open=opened, category=category, selected=selected, entries=entries, rows=visibleRows,
        buttons=buttons, footer={handle=footer,bounds=info(footer)}, state=message, reloading=reloadSequence ~= nil}
end

function pane.shutdown()
    if opened then pane.stop() end
    clear()
    for _, handle in ipairs(navElements) do UI.deleteElement(handle) end
    if page then UI.deletePage(page) end
    if navPage then UI.deletePage(navPage) end
    font, basePage, navPage, page, footer = nil, nil, nil, nil, nil
    opened, audioOnly, entries, selected, offset = false, false, {}, nil, 0
    snapshot, reloadSequence, playSequence, autoplayPath = nil, nil, nil, nil
    navElements, category, message = {}, "synth", "Choose a sound"
end

return pane
