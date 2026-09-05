-- Controlled timing audit, not a live scheduler/load experiment.
-- Executes the shipped resource calculation and work-interval policy.
-- Usage: lua gameplay_timing.lua /path/to/reviewed/synarchy
local root = assert(arg[1], 'supply the reviewed source checkout path')
package.path = root .. '/?.lua;' .. package.path
package.loaded['scripts.unit_stats'] = {
    get = function(_, name) assert(name == 'max_stamina'); return 10 end,
}
package.loaded['scripts.movement_speed'] = { UPHILL_EXERTION_PER_GRADE = 0.5 }
package.loaded['scripts.injuries'] = {}
package.loaded['scripts.brain'] = {}
package.loaded['scripts.unit_resource_energy'] = { FAT_FLOOR_TOL = 0.0001 }
package.loaded['scripts.unit_resource_alerts'] = {
    emitDeathAlert = function() error('unexpected death') end,
    deathCauseFor = function() return 'starvation' end,
}
local tick = assert(loadfile(root .. '/scripts/unit_resource_tick.lua'))()
local params = assert(loadfile(root .. '/scripts/unit_resource_config.lua'))().acolyte.stamina

-- Each supplied entry is one completed nominal-0.1-second callback.
-- Both schedules are considered over the same one-second observation
-- window. Scheduler source proves late passes still supply 0.1; this
-- fixture tests the downstream consequence, not scheduler dispatch.
local function recover(callbacks)
    local stamina = 6
    unit = {
        getStat = function(_, name)
            if name == 'stamina' then return stamina end
            if name == 'endurance' then return 1 end
            if name == 'fat_mass' then return 10 end
            if name == 'height' then return 1.7 end
            if name == 'caffeine' then return 0 end
            error(name)
        end,
        setStat = function(_, name, value)
            assert(name == 'stamina'); stamina = value
        end,
        kill = function() error('unexpected kill') end,
        collapse = function() error('unexpected collapse') end,
    }
    for _ = 1, callbacks do
        tick.tickResource(1, 'acolyte', 'stamina', params, 'idle', 'standing', 0.1)
    end
    return stamina
end
local timely, delayed = recover(10), recover(2)
assert(math.abs(timely - 6.5) < 1e-9)
assert(math.abs(delayed - 6.1) < 1e-9)
print(string.format('same 1s observation: ten callbacks=%.2f; two callbacks=%.2f', timely, delayed))

local stall = assert(loadfile(root .. '/scripts/unit_ai_stall.lua'))()
assert(stall.workInterval(100, 103) == 3)
assert(stall.workInterval(100, 106) == 0)
print(string.format('production workInterval: 3s gap=%.1f credited; 6s gap=%.1f credited',
    stall.workInterval(100, 103), stall.workInterval(100, 106)))
print('Controlled timing assertions passed; no concurrent stat mutation was injected.')
