-- Audit evidence, not a production regression test.
-- Usage: lua stance_interleaving.lua /path/to/reviewed/synarchy
-- Runs the actual recovery function and simulates a combat mutation at
-- an engine API boundary. Does not start an engine or access a real unit.
local root = assert(arg[1], 'supply the reviewed source checkout path')
for _, name in ipairs({
    'scripts.injuries', 'scripts.brain', 'scripts.unit_resource_alerts',
}) do
    package.loaded[name] = {}
end
local injury = assert(loadfile(root .. '/scripts/unit_resource_injury.lua'))()
local stance = 0.6
local charged = false
unit = {
    getStat = function(uid, name)
        assert(uid == 1)
        if name == 'stance' then return stance end
        if name == 'dexterity' and not charged then
            -- Current quick-strike stance cost at reviewed revision:
            -- Combat.Resolution.Constants.stanceAttackCost Quick = 0.25.
            stance = math.max(0, stance - 0.25)
            charged = true
            print(string.format('Injected quick-strike charge: %.3f', stance))
        end
        return 1
    end,
    setStat = function(uid, name, value)
        assert(uid == 1 and name == 'stance')
        stance = value
        return true
    end,
}
injury.tickStance(1, 0.1)
local expected = 0.6 - 0.25 + (0.35 + 0.12 * (1 + 1)) * 0.1
print(string.format('Actual: %.3f; preserving both changes: %.3f', stance, expected))
assert(charged)
assert(math.abs(stance - 0.659) < 1e-9)
assert(math.abs((stance - expected) - 0.25) < 1e-9)
print('CONFIRMED: the injected charge was fully overwritten.')
print('Controlled API-boundary interleaving; not a live-thread frequency measurement.')
