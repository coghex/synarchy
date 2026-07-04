-- Knowledge registry — the catalogue of all knowledge TYPES a unit can
-- learn. Distinct from skills: a unit only trains a knowledge it KNOWS
-- (acquired from a source — a book or a teacher, or spawned-known via the
-- unit def's `knowledge:` block). Per-unit state lives engine-side in
-- uiKnowledge (unit.getKnowledge / setKnowledge / getKnowledgeList).
--
-- `icon` is the icon basename (assets/textures/icons/<icon>.png). Knowledge
-- icons are prefixed "knowledge_" to keep the icon set tidy.
-- Effective capability at the point of use = level × the relevant stat
-- (intelligence for medical work). Extend REGISTRY freely.

local M = {}

M.REGISTRY = {
    {
        id   = "bleed_control",
        name = "Bleed Control",
        icon = "knowledge_bleed_control",
        desc = "Stem bleeding with sutures and bandages. The quality of a\n"
            .. "dressing scales with this level and the unit's intelligence.",
    },
    {
        id   = "infection_control",
        name = "Infection Control",
        icon = "knowledge_infection_control",
        desc = "Clean wounds and administer antibiotics to fight infection.\n"
            .. "How much an antibiotic dose cuts infection scales with this\n"
            .. "level and the unit's intelligence.",
    },
    {
        id   = "basic_cuisine",
        name = "Basic Cuisine",
        icon = "knowledge_basic_cuisine",
        desc = "Gates the cooking recipe tier — brewing a pot of coffee and\n"
            .. "future cuisine besides. Output quality scales with this\n"
            .. "level and the cooking skill.",
    },
}

-- Icon shown for a knowledge the unit hasn't learned.
M.UNKNOWN_ICON = "knowledge_unknown"

function M.list()
    return M.REGISTRY
end

function M.byId(id)
    for _, k in ipairs(M.REGISTRY) do
        if k.id == id then return k end
    end
    return nil
end

return M
