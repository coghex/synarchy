-- Audit reproduction; engine API interleaving is simulated.
-- Usage: lua stamina_interleaving.lua /path/to/reviewed/synarchy
local root = assert(arg[1], 'supply the reviewed source checkout path')
package.loaded['scripts.unit_stats'] = {
    get = function(_, name) assert(name == 'max_stamina'); return 10 end,
}
package.loaded['scripts.movement_speed'] = { UPHILL_EXERTION_PER_GRADE = 0.5 }
package.loaded['scripts.injuries'] = {}
package.loaded['scripts.brain'] = {}
package.loaded['scripts.unit_resource_energy'] = { FAT_FLOOR_TOL = 0.0001 }
package.loaded['scripts.unit_resource_alerts'] = {
    emitDeathAlert = function() end,
    deathCauseFor = function() return 'starvation' end,
}
local tick = assert(loadfile(root .. '/scripts/unit_resource_tick.lua'))()
local params = assert(loadfile(root .. '/scripts/unit_resource_config.lua'))().acolyte.stamina
local function run(initial, inject)
    local stamina, charged, deaths = initial, false, 0
    unit = {
        getStat = function(_, name)
            if name == 'stamina' then return stamina end
            if name == 'endurance' then
                if inject and not charged then
                    -- Current heavy-strike cost: 0.25 * maximum 10.
                    stamina = math.max(0, stamina - 2.5)
                    charged = true
                end
                return 1
            end
            if name == 'fat_mass' then return 10 end
            if name == 'height' then return 1.7 end
            if name == 'caffeine' then return 0 end
            error(name)
        end,
        setStat = function(_, name, value)
            assert(name == 'stamina')
            stamina = value
        end,
        kill = function() deaths = deaths + 1 end,
        collapse = function() end,
    }
    tick.tickResource(1, 'acolyte', 'stamina', params, 'idle', 'standing', 0.1)
    assert(charged == inject)
    print(string.format('start=%.2f interleaved_debit=%s final=%.2f kill_calls=%d',
        initial, tostring(inject), stamina, deaths))
    return stamina, deaths
end
local s, k = run(6, true)
assert(math.abs(s - 6.05) < 1e-9 and k == 0)
s, k = run(2, true)
assert(math.abs(s - 2.05) < 1e-9 and k == 0)
s, k = run(0, false)
assert(math.abs(s - 0.05) < 1e-9 and k == 1)
print('Confirmed overwrite and missed zero-stamina kill under the injected schedule.')
