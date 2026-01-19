local tickCount = 0

function init()
    engine.logInfo("Lua init script started.")
end

function update(dt)
    tickCount = tickCount + 1
    --engine.logInfo("update #" .. tickCount .. " dt=" .. tostring(dt))

    -- example, change tick rate after 5 updates
    if tickCount == 5 then
        engine.setTickInterval(0.5)
    --    engine.logInfo("Tick rate changed to 0.5 seconds.")
    end
end
