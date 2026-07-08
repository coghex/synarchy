-- Unit AI tunables (#538 split from unit_ai.lua).
--
-- Per-species utility-AI config: thought cadence, wander/drink/eat/
-- forage/refill/search/notify/deliver/store/build/construct/craft/dig/
-- chop/repair/treat parameters. Pure data — read via the `params`
-- argument threaded through every action's utility/execute, and
-- extended at runtime by unitAi.setConfig (satellite species scripts,
-- e.g. bear_ai.lua). See scripts/unit_ai.lua for the dispatch loop
-- that reads this table.

-----------------------------------------------------------
-- Tunables per unit def
-----------------------------------------------------------

-- Wander utility formula:
--   base + stamina_weight * (stamina / max_stamina)
--                         - time_penalty * (now - actionStartedAt)
-- Clamped to -inf if stamina < min_wander_stamina_fraction * max.
--
-- follow_command utility is constant 7.0 when a task is pending
-- (FOLLOW_COMMAND_UTILITY). The ladder (highest first, #306):
--   dire SELF survival (drink/eat ~10–15, derived from need)
--   combat / treatment        (engage·retreat 8.0+, treat_ally 8.0)
--   ───── player orders ─────  (follow_command 7.0, pickup 7.5,
--                               dry-canteen refill peak 7.5)
--   situational goals          (find_water 3.0–6.0 derived from thirst,
--                               notify_allies 4.0) — a routine goal
--                               yields to a command; it only wins on
--                               its own when need (not the goal) makes it
--   routine work / wander      (dig·deliver locks ≤6.0, wander ~0.8)
-- So: a fight or a wound supersedes a move order; a move order supersedes
-- a routine goal; and a goal only climbs above a command when the
-- underlying NEED (thirst→drink/refill) does, which is derived. See the
-- FOLLOW_COMMAND_UTILITY block and the per-action notes for the values.
--
-- Derived roles (#265) reshuffle preference WITHIN the routine band
-- only: work ENTRY utilities are multiplied by ×1.4 (on-role) / ×0.7
-- (off-role), capped by construction below the 6.0 locks (max weighted
-- entry = deliver 4.0 · 1.4 = 5.6), so a role can never lift routine
-- work over a player order or drop an in-progress job's lock.
local config = {
    acolyte = {
        thought_interval = 1.0,    -- seconds between decisions
        thought_jitter   = 0.5,    -- ± fraction of interval
        -- Override when active attack / retreat goal — combat needs
        -- 10× faster re-evaluation so two units charging each other
        -- catch the in-range moment instead of walking through it.
        combat_thought_interval = 0.1,
        wander_radius    = 5.0,    -- tiles
        -- Movement speeds come from the comfort/ordered/sprint regime
        -- (scripts/movement_speed.lua), not per-action fractions.
        base_wander_utility          = 0.5,
        wander_stamina_weight        = 0.3,
        wander_time_penalty          = 0.1,    -- per second in session
        wander_min_stamina_fraction  = 0.2,
        -- Drinking
        drink_sip_litres        = 0.5,    -- canteen water consumed per sip
        drink_min_thirst        = 0.2,    -- 1 - hydration/max; below this, no drink
        -- Drinking utility = thirst · drink_weight (thirst = 1 - hyd/max).
        -- At weight 15 it crosses follow_command (7.0) at thirst ≈ 0.47,
        -- so a unit whose hydration drops below ~half diverts to drink
        -- even under a move order (moderate thirst interrupts commands,
        -- #306), and a near-empty unit (thirst→1) tops out at ~15, well
        -- above every order/goal. Shared with drink_from_source.
        drink_weight            = 15.0,
        drink_canteen_def       = "canteen_steel_2l",
        -- One litre of canteen water restores N L of hydration. A 2 L
        -- canteen at K=11 gives 22 L of hydration ≈ 50% of the
        -- average max_hydration (~43 L), matching the design target
        -- "a full canteen should refill an average acolyte by ~50%".
        drink_hydration_per_litre = 11.0,
        -- Sleep (go_to_sleep, circadian epic #479/#611/#612). Utility =
        -- base + deficit_weight·deficit + urge_weight·circadianUrge
        --      + exhaustion_weight·exhaustionDeficit,
        -- where deficit = 1 - sleep_pressure/max (0 = fully rested, 1 =
        -- fully sleep-deprived), circadianUrge is the dusk-centered bump
        -- (scripts/circadian.lua, 0..1), and exhaustionDeficit = 1 -
        -- exhaustion.fraction (scripts/exhaustion.lua's SHORT-horizon
        -- fatigue, 0..1). Gated by sleep_min_deficit so a freshly-rested
        -- unit never even considers napping.
        --   deficit at the floor (0.35), midday (urge≈0), rested:
        --     0 + 9·0.35 + 4·0 + 2·0 = 3.15 — routine, below
        --     follow_command, so ordinary daytime tiredness doesn't
        --     interrupt work.
        --   deficit at the floor (0.35), dusk peak (urge=1), rested:
        --     0 + 9·0.35 + 4·1 + 2·0 = 7.15 — just crosses
        --     follow_command (7.0): a moderately tired acolyte settles
        --     down for the night once dusk hits rather than working
        --     through it.
        --   deficit at the floor, midday, fully exhausted (rare — a hard
        --   day's labour without meaningful sleep debt yet):
        --     0 + 9·0.35 + 4·0 + 2·1 = 5.15 — still below
        --     follow_command; exhaustion alone is a nudge, never the
        --     dominant term (matches exhaustion.lua's own "doesn't
        --     collapse or seek rest on its own").
        --   deficit high (0.78+), any time of day:
        --     0 + 9·0.78 ≈ 7.0+ — genuine multi-day sleep deprivation
        --     forces sleep even under a standing order, mirroring
        --     hunger/thirst's dire-need override (#306).
        -- Once the lie-down/sleep/wake sequence starts it locks in at
        -- math.huge (sleepUtility), same as drink_from_source.
        sleep_min_deficit     = 0.35,
        sleep_base_weight     = 0.0,
        sleep_deficit_weight  = 9.0,
        sleep_urge_weight     = 4.0,
        sleep_exhaustion_weight = 2.0,
        -- Walk-to-spot geometry: a rosette-style widening search (8
        -- compass points per ring, geometrically the same pattern as
        -- search_for_water below) samples candidate tiles directly —
        -- flatness/fluid are queryable from where the unit stands, no
        -- physical walk-and-scan needed — picking the first flat, dry
        -- one within radius. "Any flat open tile" (v1 decision): no
        -- dedicated threat/hazard safety filtering, just enough to skip
        -- a slope or a lake. sleep_spot_ring_spacing=2.0 over radius=6.0
        -- gives 3 rings. sleep_spot_max_wait bounds a bad pick anyway
        -- (unreachable — e.g. the stuck-walk watchdog gave up on it):
        -- comfortably above the worst-case walk time at meander speed
        -- (radius / a slow ~0.5 tiles/s ≈ 12s), so it never cuts off a
        -- genuinely slow-but-progressing walk, but still re-picks a
        -- spot rather than retrying a dead one forever.
        sleep_spot_radius        = 6.0,
        sleep_spot_ring_spacing  = 2.0,
        sleep_spot_arrival_tiles = 1.0,
        sleep_spot_max_wait      = 25.0,
        -- Eating. Mirror of drink_from_canteen: only fires when hunger
        -- drops below eat_max_fraction (0.25) of max_hunger; utility =
        -- (1 - hungerFrac) · eat_weight. Because it only fires past the
        -- 0.25 threshold, the term is always ≥ 0.75·10 = 7.5 > command
        -- (7.0) — a hungry unit interrupts orders to eat (#306).
        -- Inventory-only; a unit carrying nothing edible forages from
        -- flora / ground food instead (forage below, #94).
        eat_max_fraction        = 0.25,
        eat_weight              = 10.0,
        -- Foraging (#94). Fires when the stomach is below
        -- forage_max_fraction AND the unit carries no food (eating
        -- what you hold always beats going to get more). Utility =
        -- base + scale·need², where need blends stomach emptiness
        -- (×0.6) with calorie-STORE emptiness — the true starvation
        -- signal. The quadratic ramp is the #306 dire-need shape:
        --   fresh spawn, stomach half-empty:  ~1.7 — above wander,
        --                                     far below command (7.0).
        --   store half drained:               ~2.9 — still routine.
        --   store ~10% (catabolism looming):  ~7.0 — crosses command:
        --                                     the EMERGENCY override.
        --   store empty (starving):           ~8.4 — above command
        --                                     (7.0) and pickup (7.5),
        --                                     below eat-in-hand (≥7.5,
        --                                     →10) so food already
        --                                     carried is always eaten
        --                                     before more is fetched.
        forage_max_fraction   = 0.5,
        forage_search_radius  = 24,   -- tiles (engine caps at 64)
        forage_base_weight    = 1.0,
        forage_urgency_scale  = 7.4,
        -- Refilling. Utility ramps quadratically above the threshold:
        -- util = base + scale·x², x = (emptiness-0.25)/0.75 ∈ [0,1].
        --   25% empty (threshold): ~0.85 — beats wander, stays well
        --                          below follow_command (7.0). Tops off
        --                          only when nothing important is going on.
        --   50% empty:             ~1.6 — still far below command, so a
        --                          half-empty canteen won't interrupt
        --                          player orders.
        --   75% empty:             ~3.8 — climbing, but a busy/ordered
        --                          unit still finishes its task first.
        --   ~97% empty:            ~7.0 — crosses command. A near-dry
        --                          canteen now interrupts a move order.
        --   100% empty:            ~7.5 — above command. The canteen has
        --                          run dry; refilling pre-empts orders (#306).
        canteen_def              = "canteen_steel_2l",
        refill_min_emptiness     = 0.25,
        refill_base_weight       = 0.85,
        -- Peak (x=1, dry) = base + scale = 0.85 + 6.65 = 7.5, just above
        -- FOLLOW_COMMAND_UTILITY (7.0); the x² shape keeps partial
        -- canteens well under command (only a near-dry one interrupts).
        refill_urgency_scale     = 6.65,
        refill_arrival_tiles     = 1.5,
        -- Searching. Fires when canteen has headroom but no water is
        -- known. Walks a rosette: 8 compass waypoints per ring,
        -- distance = ring_index * search_spacing. Spacing roughly
        -- matches FOV radius so successive rings sweep new ground.
        -- Threshold matches refill — if we'd want to top up (had we
        -- known where water is), we want to spiral to find some.
        search_min_emptiness     = 0.25,
        search_base_weight       = 0.85,
        search_emptiness_weight  = 0.1,    -- → 0.85 at threshold, 0.95 at empty
        search_spacing           = 6,
        search_arrival_tiles     = 1.5,
        search_max_step          = 32,     -- ~4 rings; then re-anchor origin
        -- "find_water" goal urgency — DERIVED, not a flat weight (#306).
        -- A standing goal is not automatically more important than what
        -- the player just ordered; its priority should track how badly
        -- water is actually needed. So searchUtility returns
        --   goal_search_floor + goal_search_urgency · thirst
        -- where thirst = 1 - hydration/max ∈ [0,1]:
        --   * well-hydrated scout → ~3.0: above wander (so it searches
        --     when idle) but BELOW follow_command (7.0), combat, and
        --     treatment — a player move order, a fight, or a medic's
        --     patient all supersede a routine search;
        --   * as the searcher runs dry it climbs toward ~6.0, and its
        --     own thirst (drink/refill, thirst·15 / dry-canteen 7.5)
        --     takes over above command before the goal alone ever would.
        -- This is the "some goals are critical, some are not" rule: the
        -- criticality is derived from need rather than asserted by a
        -- constant. Capped below command by construction (floor+urgency<7).
        goal_search_floor        = 3.0,
        goal_search_urgency      = 3.0,
        -- Notify-allies (second goal). Radio branch: stand still N
        -- seconds, then push known sources to every other radio-
        -- bearing acolyte. Walk branch: pick an uninformed acolyte
        -- by rank-based split, walk to them, stand N seconds, transfer.
        notify_broadcast_seconds = 1.0,
        notify_transfer_seconds  = 1.0,
        notify_arrival_tiles     = 1.5,
        -- notify_allies is a routine, non-critical goal (sharing where
        -- water is) — it sits ABOVE wander but BELOW follow_command (7.0)
        -- so a player order supersedes it, and below combat/treatment so
        -- an attacked or injured notifier breaks off (#306). The phase
        -- lock (notifyAlliesUtility) only needs to out-rank ambient
        -- wander so a half-done broadcast isn't yanked by idle drift.
        goal_notify_weight       = 4.0,
        -- Construction (build_nearby). Utility shape:
        --   util = base · (1 − workers_present/saturation) · dist_factor
        -- workers_present EXCLUDES the unit asking, so the curve is
        -- 3.0 / 2.4 / 1.8 / 1.2 / 0.6 / 0 as 0..5 others have already
        -- joined. dist_factor is 1.0 at adjacent and linearly falls to
        -- 0 at build_scan_range tiles away.
        build_scan_range       = 30.0,
        build_arrival_tiles    = 1.5,
        build_base_utility     = 3.0,
        build_saturation_n     = 5,
        -- Material delivery (deliver_to_build_site). Higher than the
        -- build_nearby max so a unit with both materials and an
        -- adjacent build site will deliver first, then transition to
        -- building once empty.
        deliver_scan_range     = 30.0,
        deliver_utility        = 4.0,
        -- Fetching from the technomule. Construction materials live
        -- on the mule (technomule.yaml), so a deliverer whose own
        -- inventory lacks claimed materials walks to the mule first
        -- and takes the shortfall (unit.transferItemToUnit preserves
        -- the instances). Capacity-gated per item like pickup.
        mule_fetch_arrival     = 1.5,
        -- Construction designations (construct_job, #96). Executes the
        -- construction-tool blueprints: structure pieces get sourced
        -- (inventory → ground → mule), built in place, and placed;
        -- building blueprints get STAKED (building.spawn) and handed to
        -- the deliver/build_nearby machinery above. Base sits between
        -- build_nearby (3.0) and deliver (4.0); the lock matches the
        -- other menial-work locks (6.0) so dire needs still preempt.
        -- construct_rate is worker-seconds of effort per real second;
        -- a piece takes build_work / construct_rate seconds to build.
        construct_scan_range    = 30.0,
        construct_scan_chunks   = 2,     -- getPendingJobs region radius
        construct_arrival_tiles = 1.5,
        construct_base_utility  = 3.5,
        construct_lock_utility  = 6.0,
        construct_rate          = 1.0,
        construct_claim_timeout = 30.0,  -- stale-claim expiry (seconds)
        construct_xp_per_piece  = 1.0,   -- construction XP per placed
                                         -- piece (#265; diminishing via
                                         -- applySkillXP, like mining)
        -- Auto-store materials. Utility curve = base · fill³ where
        -- fill = carrying_weight / carrying_capacity. Below ~65 %
        -- full the action sits under wander (0.8); past that it
        -- climbs to base at 100 %. Acolytes only auto-deposit items
        -- in the "Materials" category — supplies, tools, and worn
        -- equipment stay on them.
        store_scan_range       = 30.0,
        store_base_utility     = 3.0,
        -- Mining (dig_designation). Utility shape mirrors build:
        --   util = base · min(toolSpeed, 1) · dist_factor
        -- toolSpeed is the best dig-rate multiplier among carried
        -- tools for the tile's material (data/materials pick_speed /
        -- shovel_speed), so hard rock with only a shovel reads as
        -- high effort and scores near zero. No tool → no dig.
        -- Lock-in while a dig is underway is finite (dire needs
        -- still preempt; never math.huge).
        dig_scan_range       = 30.0,
        dig_arrival_tiles    = 0.4,   -- tight: stand AT the corner
        dig_base_utility     = 2.0,
        dig_lock_utility     = 6.0,
        dig_rate             = 0.5,   -- corner-units/sec at tool speed
                                      -- 1.0 AND unit factor 1.0 (a
                                      -- tile holds 4.0 total). Actual
                                      -- rate = dig_rate · toolSpeed ·
                                      -- strength · (0.5 + mining/100),
                                      -- so a strength-1.0 / mining-50
                                      -- unit is the baseline.
        dig_xp_per_tile      = 1.0,   -- mining XP per completed tile
                                      -- (diminishing returns via
                                      -- applySkillXP: gain ∝ 1/level²)
        dig_equip_seconds    = 1.0,
        dig_claim_timeout    = 30.0,  -- stale-claim expiry (seconds)
        dig_tools = {
            shovel = { defs = { shovel_steel = true },
                       equip_anim = "standing_to_holding_shovel",
                       work_anim  = "shoveling" },
            pick   = { defs = { pick_steel = true },
                       equip_anim = "standing_to_holding_pickaxe",
                       work_anim  = "using_pickaxe" },
        },
        -- Tree felling (chop_designation, #97). Utility shape mirrors
        -- dig with a wood-stockpile damper:
        --   util = base · dist_factor · stock_factor
        -- stock_factor runs 1.0 (no wood anywhere) down to
        -- chop_stock_floor once chop_stock_target logs are lying
        -- around / carried — designated trees still get felled with a
        -- full stockpile, just after scarcer work. Unlike dig there is
        -- no tool GATE: an axe only speeds the swing
        -- (chop_tools.axe.speed vs chop_bare_speed), matching the
        -- issue's "chop works without one, just slower". Lock-in while
        -- felling is finite (dire needs still preempt; never
        -- math.huge). Claims mirror digClaims (module-local, expire on
        -- chop_claim_timeout or claimant death).
        chop_scan_range      = 30.0,
        chop_base_utility    = 2.0,
        chop_lock_utility    = 6.0,
        chop_rate            = 0.1,   -- felling progress/sec at speed 1.0 and
                                      -- strength 1.0 (a tree is 1.0 total —
                                      -- ~10 s with an axe, ~40 s bare-handed;
                                      -- deliberately slower than foraging)
        chop_equip_seconds   = 1.0,
        chop_claim_timeout   = 30.0,  -- stale-claim expiry (seconds)
        chop_xp_per_fell     = 2.0,   -- woodcutting XP per felled tree
                                      -- (#265): a fell is one chunky job
                                      -- vs dig's 1.0/tile
        chop_stock_target    = 20,    -- colony logs at which urgency floors
        chop_stock_floor     = 0.5,
        chop_bare_speed      = 0.25,  -- no axe: hacking with what's at hand
        chop_tools = {
            axe = { defs = { axe_steel = true }, speed = 1.0 },
        },
        -- No dedicated axe animation exists yet — the pickaxe swing set
        -- is the closest two-handed-tool visual.
        chop_equip_anim = "standing_to_holding_pickaxe",
        chop_work_anim  = "using_pickaxe",
        -- Tilling (till_designation, #333). Utility shape mirrors dig,
        -- but bare-handed only — #333 left "does tilling need a tool
        -- item" open; a tiller item is a future speed-up, not a gate
        -- (no tool table at all here, unlike dig/chop). Lock-in while
        -- tilling is finite (dire needs still preempt; never
        -- math.huge). Claims mirror chopClaims (module-local, expire on
        -- till_claim_timeout or claimant death). Rate scaling + XP grant
        -- by the farming skill (#265) landed with #336, alongside the
        -- rest of the farm loop.
        till_scan_range    = 30.0,
        till_base_utility  = 2.0,
        till_lock_utility  = 6.0,
        till_rate          = 0.2,   -- tilling progress/sec at strength 1.0
                                    -- (a tile is 1.0 total — 5s bare-handed)
        till_equip_seconds = 1.0,
        till_claim_timeout = 30.0,  -- stale-claim expiry (seconds)
        till_xp_per_till   = 1.0,   -- farming XP (#265/#336) per tilled tile
        -- No dedicated push/tiller animation exists yet — the shovel
        -- work set is the closest hand-tool-on-ground visual (same
        -- reuse-until-real-art convention as chop_equip_anim above).
        till_equip_anim = "standing_to_holding_shovel",
        till_work_anim  = "shoveling",
        -- Planting (plant_designation, #336). Structure mirrors till
        -- exactly (claim/walk/equip/work-progress), skill-scaled by the
        -- new farming skill (#265) like chop/mining. Completion
        -- dispatches to world.plantCropAt (groundcover) or
        -- world.plantRowCropAt (row_crop) by the designation's category.
        plant_scan_range    = 30.0,
        plant_base_utility  = 2.0,
        plant_lock_utility  = 6.0,
        plant_rate          = 0.2,   -- planting progress/sec at strength
                                     -- 1.0 and farming 50 (a tile is 1.0
                                     -- total — mirrors till_rate)
        plant_equip_seconds = 1.0,
        plant_claim_timeout = 30.0,  -- stale-claim expiry (seconds)
        plant_xp_per_plant  = 1.0,   -- farming XP (#265) per planted tile
        -- No dedicated planting animation exists yet — the shovel work
        -- set is the closest hand-tool-on-ground visual (same
        -- reuse-until-real-art convention as till_equip_anim above).
        plant_equip_anim = "standing_to_holding_shovel",
        plant_work_anim  = "shoveling",
        -- Auto-harvest (auto_harvest, #336). Instant harvest — mirrors
        -- forage's shape (#94), not till/chop's progress accumulator,
        -- since picking ripe fruit is quick. NOT hunger-gated like
        -- forage: this is routine farm-tending work, weighted by the
        -- farming skill/role (#265) instead of scaled by need. Reuses
        -- world.findHarvestableFlora/world.harvestFlora, which cover
        -- planted crops AND wild flora alike (a diligent farmer keeps
        -- the whole area picked, not just the tilled fields).
        harvest_scan_range     = 24.0,
        harvest_base_utility   = 2.0,
        harvest_xp_per_harvest = 1.0,  -- farming XP (#265) per harvest
        -- Equipment repair (repair_job, #302). AI-autonomous kit
        -- maintenance: an acolyte notices its own or the technomule's
        -- gear has degraded past a threshold and carries it to the
        -- right #301 station (furnace = condition, workbench =
        -- sharpness). Utility shape:
        --   util = base · severity(item)
        -- severity is tiered: a broken (condition 0) item scores a
        -- flat band (armor higher than weapons — broken armor gives
        -- ZERO protection vs. a broken weapon's 0.15× effectiveness,
        -- Combat/Resolution.hs), otherwise a quadratic ramp toward the
        -- threshold. base=1.2 keeps the top tier (armor, 2.5) at 3.0
        -- unweighted / 4.2 role-weighted — under every 6.0 lock and
        -- deliver's 5.6 weighted ceiling (see unit_roles.lua). Lock-in
        -- while fetching/walking/repairing is finite, matching
        -- dig/chop/construct (dire needs still preempt).
        repair_condition_threshold    = 50.0,
        repair_sharpness_threshold    = 50.0,
        repair_severity_broken_weapon = 1.5,
        repair_severity_broken_armor  = 2.5,
        repair_base_utility  = 1.2,
        repair_lock_utility  = 6.0,
        repair_scan_range    = 30.0,   -- ground-item consumable search radius
        repair_claim_timeout = 30.0,   -- stale-claim expiry (seconds)
        repair_xp_per_repair = 1.0,    -- smithing XP (#265) — the "smith"
                                       -- role's first real work action
        -- Craft bills (craft_job, #329). Works the per-station bill
        -- queue (craft.addBill → engine-side Craft.Bills): claim a
        -- bill, source the recipe's inputs + fuel (inventory → ground
        -- → mule, the #96 ladder), stand beside the Built station,
        -- pour work in, and at 1.0 execute the craft + lay the outputs
        -- down at the station. Base sits with the other menial work
        -- (between dig 2.0 and deliver 4.0); the lock matches the
        -- menial-work locks (6.0) so dire needs still preempt.
        -- craft_rate is worker-seconds of effort per real second; a
        -- recipe takes work / (craft_rate · (0.5 + skill/100)) seconds.
        craft_scan_range    = 30.0,
        craft_base_utility  = 3.2,
        craft_lock_utility  = 6.0,
        craft_rate          = 1.0,
        craft_claim_timeout = 30.0,  -- stale-claim expiry (game seconds;
                                     -- enforced engine-side by
                                     -- craft.claimBill)
        craft_xp_per_craft  = 1.5,   -- trade-skill XP per completed
                                     -- craft (#265; diminishing via
                                     -- applySkillXP, like the others)
        -- Ground-item pickup (player order via right-click → Pick up).
        -- An explicit pickup is a player order peer to a move order, so
        -- it sits just ABOVE follow_command (7.0): commandMove and
        -- commandPickup set independent fields without clearing each
        -- other, so when both are pending the more-specific pickup must
        -- win the arbitration rather than time out behind the move (#306).
        -- 7.5 is just above follow_command (7.0) — a peer player order
        -- that wins the move-vs-pickup tie — but BELOW combat/treatment
        -- (≥8.0) and dire survival, so a fight or a fresh wound still
        -- interrupts it. Capacity is checked at the moment of pickup.
        pickup_utility       = 7.5,
        pickup_arrival_tiles = 1.2,
        pickup_timeout       = 30.0,
        -- Medic auto-treat (treat_ally, Phase D). A unit that KNOWS
        -- bleed-control (knowledge × intelligence = its capability)
        -- rushes to bandage a bleeding ally, fetching the first-aid
        -- kit from the technomule first. Base/lock utility sit ABOVE a
        -- player move order (follow_command 7.0) and the menial-work
        -- locks (dig/deliver = 6.0) so treating a bleeding ally is not
        -- cancelled by a stray move command or routine labour (#306) —
        -- but BELOW dire SELF survival (drink/eat ~10) and combat
        -- (engage/retreat 8.0, ties broken to combat by list order), so
        -- a medic still drinks when dying of thirst and defends itself
        -- when attacked (medicBusyInCombat then frees a lesser medic).
        -- Treating ANOTHER unit is high but, per design, below a unit's
        -- own survival/getting-treated. (A future refinement could scale
        -- this with the patient's bleed severity for a fully situational
        -- value; today it is a fixed above-command band.) Squad rule: the
        -- best available medic takes a patient; a lesser one only steps
        -- in when the best is tied up in combat and nobody else claimed it.
        treat_scan_range     = 60.0,
        treat_base_utility   = 8.0,
        treat_lock_utility   = 8.0,
        treat_arrival        = 1.5,   -- tiles to the patient to treat
        treat_min_seep       = 0.6,   -- a wound dressed below this seep
                                      -- is "good enough" — not re-treated
    },
    -- Species-specific config blocks (bear, future wildlife) are
    -- registered via unitAi.setConfig from their own AI scripts
    -- (scripts/bear_ai.lua, etc.). Keep `acolyte` here since the
    -- universal candidates + acolyte ambient candidates all live
    -- in this module.
}

return config
