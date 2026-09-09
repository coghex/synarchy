# Lua boundary profile evidence, 2026-09-09

Companion evidence for [the API contract](lua_api_contract.md) and issue #2483.
This archive preserves the raw capture and the exact helper used. The raw label
`enemy-location encounter` was entered before play; the owner subsequently
confirmed exploration toward a ruin with no combat before the capture ended.

## Raw capture

Original basename: `foreground-2483-profile.json`.
SHA-256: `6f8720022c7ea19250cc5ce9ea20d33cef2c4e15dde5a813a9465b0d7ae7cae4`.
Byte length: 24846 (UTF-8, including the final newline).
To recover the original, copy only the contents of the following JSON fence,
including its final newline. Do not reserialize it if checking the byte checksum.

```json
{
  "issue": 2483,
  "commit": "9f92dc0b291784dd6404dba8a21c57e981895cb2",
  "world_and_activity": "Wind of Havens; seed 1334661219; enemy-location encounter",
  "platform": "macOS-26.6-arm64-arm-64bit-Mach-O",
  "started_utc": "2026-09-09T12:55:36.329909+00:00",
  "capture_kind": "owner-run rendered session",
  "requested_seconds": 120,
  "elapsed_lower_bound_seconds": 120.00507129193284,
  "elapsed_upper_bound_seconds": 120.02284791716374,
  "reset_round_trip_seconds": 0.004796833032742143,
  "snapshot_round_trip_seconds": 0.012979792198166251,
  "reset_sequence": 272176,
  "stats": {
    "sequence": 2291929,
    "available": true,
    "verbs": [
      {
        "id": "UI.addChild",
        "count": 55,
        "maxDurationNs": 90000,
        "totalDurationNs": 1044000
      },
      {
        "id": "UI.addToPage",
        "count": 431,
        "maxDurationNs": 997000,
        "totalDurationNs": 4359000
      },
      {
        "id": "UI.clearFocus",
        "count": 22,
        "maxDurationNs": 1000,
        "totalDurationNs": 16000
      },
      {
        "id": "UI.deleteElement",
        "count": 356,
        "maxDurationNs": 257000,
        "totalDurationNs": 628000
      },
      {
        "id": "UI.deletePage",
        "count": 4,
        "maxDurationNs": 14000,
        "totalDurationNs": 42000
      },
      {
        "id": "UI.findHoverTarget",
        "count": 1197,
        "maxDurationNs": 843000,
        "totalDurationNs": 33789000
      },
      {
        "id": "UI.getElementInfo",
        "count": 391,
        "maxDurationNs": 275000,
        "totalDurationNs": 7363000
      },
      {
        "id": "UI.hasFocus",
        "count": 3654,
        "maxDurationNs": 28000,
        "totalDurationNs": 697000
      },
      {
        "id": "UI.hidePage",
        "count": 37,
        "maxDurationNs": 21000,
        "totalDurationNs": 100000
      },
      {
        "id": "UI.isInputBlocked",
        "count": 4074,
        "maxDurationNs": 963000,
        "totalDurationNs": 25918000
      },
      {
        "id": "UI.isPageInScope",
        "count": 52,
        "maxDurationNs": 37000,
        "totalDurationNs": 364000
      },
      {
        "id": "UI.newBox",
        "count": 56,
        "maxDurationNs": 11000,
        "totalDurationNs": 156000
      },
      {
        "id": "UI.newElement",
        "count": 7,
        "maxDurationNs": 2000,
        "totalDurationNs": 11000
      },
      {
        "id": "UI.newPage",
        "count": 5,
        "maxDurationNs": 326000,
        "totalDurationNs": 339000
      },
      {
        "id": "UI.newSprite",
        "count": 233,
        "maxDurationNs": 11000,
        "totalDurationNs": 336000
      },
      {
        "id": "UI.newText",
        "count": 190,
        "maxDurationNs": 8000,
        "totalDurationNs": 239000
      },
      {
        "id": "UI.placePopup",
        "count": 3,
        "maxDurationNs": 4000,
        "totalDurationNs": 10000
      },
      {
        "id": "UI.setBoxTextures",
        "count": 11,
        "maxDurationNs": 6000,
        "totalDurationNs": 33000
      },
      {
        "id": "UI.setClickable",
        "count": 158,
        "maxDurationNs": 3000,
        "totalDurationNs": 162000
      },
      {
        "id": "UI.setClipChildren",
        "count": 1,
        "maxDurationNs": 2000,
        "totalDurationNs": 2000
      },
      {
        "id": "UI.setColor",
        "count": 10531,
        "maxDurationNs": 7880000,
        "totalDurationNs": 28329000
      },
      {
        "id": "UI.setDragActivation",
        "count": 3,
        "maxDurationNs": 1000,
        "totalDurationNs": 3000
      },
      {
        "id": "UI.setInteractiveOverflow",
        "count": 43,
        "maxDurationNs": 2000,
        "totalDurationNs": 49000
      },
      {
        "id": "UI.setOnClick",
        "count": 78,
        "maxDurationNs": 2000,
        "totalDurationNs": 85000
      },
      {
        "id": "UI.setOnRightClick",
        "count": 82,
        "maxDurationNs": 2000,
        "totalDurationNs": 65000
      },
      {
        "id": "UI.setPointerBlocking",
        "count": 4,
        "maxDurationNs": 2000,
        "totalDurationNs": 5000
      },
      {
        "id": "UI.setPosition",
        "count": 10692,
        "maxDurationNs": 5382000,
        "totalDurationNs": 21340000
      },
      {
        "id": "UI.setScrollCapture",
        "count": 15,
        "maxDurationNs": 2000,
        "totalDurationNs": 16000
      },
      {
        "id": "UI.setSize",
        "count": 3681,
        "maxDurationNs": 5989000,
        "totalDurationNs": 25450000
      },
      {
        "id": "UI.setSpriteFrame",
        "count": 1285,
        "maxDurationNs": 7952000,
        "totalDurationNs": 11258000
      },
      {
        "id": "UI.setText",
        "count": 24927,
        "maxDurationNs": 4230000,
        "totalDurationNs": 56194000
      },
      {
        "id": "UI.setTooltip",
        "count": 1,
        "maxDurationNs": 1000,
        "totalDurationNs": 1000
      },
      {
        "id": "UI.setTooltipRich",
        "count": 3595,
        "maxDurationNs": 2183000,
        "totalDurationNs": 23397000
      },
      {
        "id": "UI.setVisible",
        "count": 55,
        "maxDurationNs": 3000,
        "totalDurationNs": 60000
      },
      {
        "id": "UI.setZIndex",
        "count": 491,
        "maxDurationNs": 4000,
        "totalDurationNs": 395000
      },
      {
        "id": "UI.showPage",
        "count": 19,
        "maxDurationNs": 6000,
        "totalDurationNs": 52000
      },
      {
        "id": "building.clearGhost",
        "count": 7,
        "maxDurationNs": 2000,
        "totalDurationNs": 5000
      },
      {
        "id": "building.deselect",
        "count": 17,
        "maxDurationNs": 2000,
        "totalDurationNs": 13000
      },
      {
        "id": "building.existsWithDef",
        "count": 120,
        "maxDurationNs": 13000,
        "totalDurationNs": 420000
      },
      {
        "id": "building.getActiveIds",
        "count": 3580,
        "maxDurationNs": 777000,
        "totalDurationNs": 5217000
      },
      {
        "id": "building.getActivity",
        "count": 4132,
        "maxDurationNs": 960000,
        "totalDurationNs": 8083000
      },
      {
        "id": "building.getBuildRequired",
        "count": 1514,
        "maxDurationNs": 36000,
        "totalDurationNs": 984000
      },
      {
        "id": "building.getInfo",
        "count": 552,
        "maxDurationNs": 649000,
        "totalDurationNs": 5416000
      },
      {
        "id": "building.getSelected",
        "count": 1201,
        "maxDurationNs": 6000,
        "totalDurationNs": 1211000
      },
      {
        "id": "building.getSpawnRemaining",
        "count": 552,
        "maxDurationNs": 96000,
        "totalDurationNs": 493000
      },
      {
        "id": "building.hitTestAt",
        "count": 12,
        "maxDurationNs": 11000,
        "totalDurationNs": 70000
      },
      {
        "id": "camera.applyScrollZoom",
        "count": 175,
        "maxDurationNs": 9000,
        "totalDurationNs": 304000
      },
      {
        "id": "camera.getZoom",
        "count": 1372,
        "maxDurationNs": 6000,
        "totalDurationNs": 915000
      },
      {
        "id": "camera.getZoomFadeEnd",
        "count": 1372,
        "maxDurationNs": 3000,
        "totalDurationNs": 229000
      },
      {
        "id": "camera.getZoomFadeStart",
        "count": 1372,
        "maxDurationNs": 3000,
        "totalDurationNs": 493000
      },
      {
        "id": "camera.goToTile",
        "count": 1,
        "maxDurationNs": 140000,
        "totalDurationNs": 140000
      },
      {
        "id": "chop.nearestDesignation",
        "count": 1514,
        "maxDurationNs": 88000,
        "totalDurationNs": 1720000
      },
      {
        "id": "combat.attack",
        "count": 3,
        "maxDurationNs": 2000,
        "totalDurationNs": 5000
      },
      {
        "id": "combat.drainEvents",
        "count": 1197,
        "maxDurationNs": 340000,
        "totalDurationNs": 2035000
      },
      {
        "id": "construction.clearStructureTarget",
        "count": 7,
        "maxDurationNs": 1000,
        "totalDurationNs": 5000
      },
      {
        "id": "construction.getPendingJobs",
        "count": 1514,
        "maxDurationNs": 21000,
        "totalDurationNs": 2423000
      },
      {
        "id": "construction.setLineMode",
        "count": 7,
        "maxDurationNs": 1000,
        "totalDurationNs": 4000
      },
      {
        "id": "craft.getBills",
        "count": 1514,
        "maxDurationNs": 28000,
        "totalDurationNs": 1671000
      },
      {
        "id": "debug.recordOutcome",
        "count": 14,
        "maxDurationNs": 24000,
        "totalDurationNs": 162000
      },
      {
        "id": "engine.emitEventForUnit",
        "count": 8,
        "maxDurationNs": 6000,
        "totalDurationNs": 33000
      },
      {
        "id": "engine.gameTime",
        "count": 46430,
        "maxDurationNs": 2640000,
        "totalDurationNs": 8286000
      },
      {
        "id": "engine.getFPS",
        "count": 1197,
        "maxDurationNs": 512000,
        "totalDurationNs": 6416000
      },
      {
        "id": "engine.getFramebufferSize",
        "count": 126,
        "maxDurationNs": 2000,
        "totalDurationNs": 54000
      },
      {
        "id": "engine.getMousePosition",
        "count": 2433,
        "maxDurationNs": 6000,
        "totalDurationNs": 1020000
      },
      {
        "id": "engine.getNotificationCfg",
        "count": 4,
        "maxDurationNs": 63000,
        "totalDurationNs": 162000
      },
      {
        "id": "engine.getTextWidth",
        "count": 25140,
        "maxDurationNs": 9698000,
        "totalDurationNs": 100424000
      },
      {
        "id": "engine.getUIScale",
        "count": 4659,
        "maxDurationNs": 28000,
        "totalDurationNs": 4836000
      },
      {
        "id": "engine.getWindowSize",
        "count": 1372,
        "maxDurationNs": 291000,
        "totalDurationNs": 1066000
      },
      {
        "id": "engine.isActionDown",
        "count": 2,
        "maxDurationNs": 8000,
        "totalDurationNs": 13000
      },
      {
        "id": "engine.isKeyDown",
        "count": 24,
        "maxDurationNs": 6000,
        "totalDurationNs": 42000
      },
      {
        "id": "engine.isPaused",
        "count": 2996,
        "maxDurationNs": 50000,
        "totalDurationNs": 1379000
      },
      {
        "id": "engine.keyMatchesAction",
        "count": 18,
        "maxDurationNs": 6000,
        "totalDurationNs": 41000
      },
      {
        "id": "engine.listFiles",
        "count": 6,
        "maxDurationNs": 94000,
        "totalDurationNs": 278000
      },
      {
        "id": "engine.loadTexture",
        "count": 5,
        "maxDurationNs": 5000,
        "totalDurationNs": 13000
      },
      {
        "id": "engine.logDebug",
        "count": 38,
        "maxDurationNs": 8000,
        "totalDurationNs": 127000
      },
      {
        "id": "engine.realTime",
        "count": 1,
        "maxDurationNs": 7000,
        "totalDurationNs": 7000
      },
      {
        "id": "engine.setPaused",
        "count": 1,
        "maxDurationNs": 2000,
        "totalDurationNs": 2000
      },
      {
        "id": "equipment.getAccessories",
        "count": 10036,
        "maxDurationNs": 16488000,
        "totalDurationNs": 140068000
      },
      {
        "id": "equipment.getClass",
        "count": 4352,
        "maxDurationNs": 8239000,
        "totalDurationNs": 123243000
      },
      {
        "id": "equipment.getLoadout",
        "count": 10034,
        "maxDurationNs": 8365000,
        "totalDurationNs": 250620000
      },
      {
        "id": "faction.areAllies",
        "count": 10612,
        "maxDurationNs": 26000,
        "totalDurationNs": 4669000
      },
      {
        "id": "faction.canAttack",
        "count": 2,
        "maxDurationNs": 1000,
        "totalDurationNs": 2000
      },
      {
        "id": "faction.isPlayerCommandable",
        "count": 8,
        "maxDurationNs": 1000,
        "totalDurationNs": 2000
      },
      {
        "id": "faction.relation",
        "count": 519,
        "maxDurationNs": 41000,
        "totalDurationNs": 753000
      },
      {
        "id": "injury.drainEvents",
        "count": 1197,
        "maxDurationNs": 6000,
        "totalDurationNs": 487000
      },
      {
        "id": "item.deselect",
        "count": 17,
        "maxDurationNs": 2000,
        "totalDurationNs": 21000
      },
      {
        "id": "item.getGroundForUnit",
        "count": 4542,
        "maxDurationNs": 12944000,
        "totalDurationNs": 30147000
      },
      {
        "id": "item.getSelected",
        "count": 1201,
        "maxDurationNs": 6000,
        "totalDurationNs": 1385000
      },
      {
        "id": "item.hitTestAt",
        "count": 10,
        "maxDurationNs": 17000,
        "totalDurationNs": 83000
      },
      {
        "id": "item.listGround",
        "count": 1514,
        "maxDurationNs": 8574000,
        "totalDurationNs": 38188000
      },
      {
        "id": "plant.nearestDesignation",
        "count": 1514,
        "maxDurationNs": 7000,
        "totalDurationNs": 1441000
      },
      {
        "id": "structure.unresolvedPaletteIds",
        "count": 120,
        "maxDurationNs": 9000,
        "totalDurationNs": 541000
      },
      {
        "id": "thought.drainEvents",
        "count": 1197,
        "maxDurationNs": 1281000,
        "totalDurationNs": 1844000
      },
      {
        "id": "thought.emit",
        "count": 23,
        "maxDurationNs": 3000,
        "totalDurationNs": 51000
      },
      {
        "id": "till.clearAnchor",
        "count": 7,
        "maxDurationNs": 1000,
        "totalDurationNs": 7000
      },
      {
        "id": "till.nearestDesignation",
        "count": 1514,
        "maxDurationNs": 9000,
        "totalDurationNs": 1484000
      },
      {
        "id": "unit.clearAnimOverride",
        "count": 18,
        "maxDurationNs": 2000,
        "totalDurationNs": 21000
      },
      {
        "id": "unit.deselectAll",
        "count": 6,
        "maxDurationNs": 2000,
        "totalDurationNs": 6000
      },
      {
        "id": "unit.exists",
        "count": 2989,
        "maxDurationNs": 7000,
        "totalDurationNs": 1213000
      },
      {
        "id": "unit.getActivity",
        "count": 42098,
        "maxDurationNs": 102000,
        "totalDurationNs": 12671000
      },
      {
        "id": "unit.getAllIds",
        "count": 5740,
        "maxDurationNs": 1390000,
        "totalDurationNs": 21070000
      },
      {
        "id": "unit.getAnimDuration",
        "count": 3,
        "maxDurationNs": 3000,
        "totalDurationNs": 6000
      },
      {
        "id": "unit.getAttackCooldown",
        "count": 70,
        "maxDurationNs": 1000,
        "totalDurationNs": 42000
      },
      {
        "id": "unit.getAttackRange",
        "count": 1470,
        "maxDurationNs": 18000,
        "totalDurationNs": 1609000
      },
      {
        "id": "unit.getBlood",
        "count": 20401,
        "maxDurationNs": 9898000,
        "totalDurationNs": 70528000
      },
      {
        "id": "unit.getCarryingWeight",
        "count": 7411,
        "maxDurationNs": 457000,
        "totalDurationNs": 23245000
      },
      {
        "id": "unit.getCurrentAnim",
        "count": 16515,
        "maxDurationNs": 278000,
        "totalDurationNs": 4964000
      },
      {
        "id": "unit.getEquippedWeaponWeight",
        "count": 70,
        "maxDurationNs": 13000,
        "totalDurationNs": 89000
      },
      {
        "id": "unit.getFaction",
        "count": 14138,
        "maxDurationNs": 311000,
        "totalDurationNs": 5311000
      },
      {
        "id": "unit.getFrameSample",
        "count": 5492,
        "maxDurationNs": 460000,
        "totalDurationNs": 21532000
      },
      {
        "id": "unit.getImmunities",
        "count": 3506,
        "maxDurationNs": 2849000,
        "totalDurationNs": 6638000
      },
      {
        "id": "unit.getInfo",
        "count": 100830,
        "maxDurationNs": 25782000,
        "totalDurationNs": 854723000
      },
      {
        "id": "unit.getInsulation",
        "count": 1520,
        "maxDurationNs": 59000,
        "totalDurationNs": 1328000
      },
      {
        "id": "unit.getInventory",
        "count": 10199,
        "maxDurationNs": 259075000,
        "totalDurationNs": 4489334000
      },
      {
        "id": "unit.getItemContents",
        "count": 2,
        "maxDurationNs": 48000,
        "totalDurationNs": 69000
      },
      {
        "id": "unit.getJumpReach",
        "count": 1,
        "maxDurationNs": 3000,
        "totalDurationNs": 3000
      },
      {
        "id": "unit.getKnowledge",
        "count": 1518,
        "maxDurationNs": 5000,
        "totalDurationNs": 1021000
      },
      {
        "id": "unit.getLastAttacker",
        "count": 2543,
        "maxDurationNs": 7000,
        "totalDurationNs": 1314000
      },
      {
        "id": "unit.getMaxSpeed",
        "count": 7408,
        "maxDurationNs": 553000,
        "totalDurationNs": 4409000
      },
      {
        "id": "unit.getPain",
        "count": 15392,
        "maxDurationNs": 322000,
        "totalDurationNs": 6100000
      },
      {
        "id": "unit.getPortraitTexture",
        "count": 5492,
        "maxDurationNs": 11193000,
        "totalDurationNs": 14750000
      },
      {
        "id": "unit.getPose",
        "count": 45457,
        "maxDurationNs": 3137000,
        "totalDurationNs": 30504000
      },
      {
        "id": "unit.getScars",
        "count": 3506,
        "maxDurationNs": 305000,
        "totalDurationNs": 2882000
      },
      {
        "id": "unit.getSelected",
        "count": 5164,
        "maxDurationNs": 1869000,
        "totalDurationNs": 48292000
      },
      {
        "id": "unit.getSkill",
        "count": 12470,
        "maxDurationNs": 23000,
        "totalDurationNs": 5253000
      },
      {
        "id": "unit.getStat",
        "count": 865928,
        "maxDurationNs": 9090000,
        "totalDurationNs": 481425000
      },
      {
        "id": "unit.getTransferOrders",
        "count": 1818,
        "maxDurationNs": 1219000,
        "totalDurationNs": 3994000
      },
      {
        "id": "unit.getVisibleTiles",
        "count": 9308,
        "maxDurationNs": 8273000,
        "totalDurationNs": 219074000
      },
      {
        "id": "unit.getWeaponClass",
        "count": 52,
        "maxDurationNs": 3000,
        "totalDurationNs": 52000
      },
      {
        "id": "unit.getWeaponWieldedFrom",
        "count": 70,
        "maxDurationNs": 2000,
        "totalDurationNs": 67000
      },
      {
        "id": "unit.getWoundSeverityOn",
        "count": 70,
        "maxDurationNs": 1000,
        "totalDurationNs": 41000
      },
      {
        "id": "unit.getWounds",
        "count": 136490,
        "maxDurationNs": 9464000,
        "totalDurationNs": 538529000
      },
      {
        "id": "unit.hitTestAt",
        "count": 14,
        "maxDurationNs": 18000,
        "totalDurationNs": 133000
      },
      {
        "id": "unit.hitTestInRect",
        "count": 2,
        "maxDurationNs": 30000,
        "totalDurationNs": 46000
      },
      {
        "id": "unit.lungeImpactSpeed",
        "count": 1,
        "maxDurationNs": 1000,
        "totalDurationNs": 1000
      },
      {
        "id": "unit.moveTo",
        "count": 38,
        "maxDurationNs": 6000,
        "totalDurationNs": 132000
      },
      {
        "id": "unit.recomputeBody",
        "count": 5505,
        "maxDurationNs": 8974000,
        "totalDurationNs": 41801000
      },
      {
        "id": "unit.recoverStance",
        "count": 8354,
        "maxDurationNs": 784000,
        "totalDurationNs": 12461000
      },
      {
        "id": "unit.select",
        "count": 2,
        "maxDurationNs": 2000,
        "totalDurationNs": 3000
      },
      {
        "id": "unit.setAnimOverride",
        "count": 47,
        "maxDurationNs": 3000,
        "totalDurationNs": 88000
      },
      {
        "id": "unit.setMoveSpeed",
        "count": 2068,
        "maxDurationNs": 11000,
        "totalDurationNs": 3126000
      },
      {
        "id": "unit.setSelection",
        "count": 2,
        "maxDurationNs": 6000,
        "totalDurationNs": 10000
      },
      {
        "id": "unit.setStat",
        "count": 218169,
        "maxDurationNs": 20062000,
        "totalDurationNs": 720465000
      },
      {
        "id": "unit.stop",
        "count": 674,
        "maxDurationNs": 6000,
        "totalDurationNs": 732000
      },
      {
        "id": "unit.transferContract",
        "count": 3,
        "maxDurationNs": 14000,
        "totalDurationNs": 21000
      },
      {
        "id": "unit.transferEndpointInfo",
        "count": 80,
        "maxDurationNs": 2292000,
        "totalDurationNs": 41909000
      },
      {
        "id": "world.clearMineAnchor",
        "count": 7,
        "maxDurationNs": 2000,
        "totalDurationNs": 12000
      },
      {
        "id": "world.clearWorldCursorSelect",
        "count": 21,
        "maxDurationNs": 4000,
        "totalDurationNs": 32000
      },
      {
        "id": "world.clearZoomCursorSelect",
        "count": 9,
        "maxDurationNs": 1000,
        "totalDurationNs": 7000
      },
      {
        "id": "world.findHarvestableFlora",
        "count": 1514,
        "maxDurationNs": 272000,
        "totalDurationNs": 13641000
      },
      {
        "id": "world.getActiveWorldId",
        "count": 7690,
        "maxDurationNs": 387000,
        "totalDurationNs": 2707000
      },
      {
        "id": "world.getAmbientAt",
        "count": 1537,
        "maxDurationNs": 91000,
        "totalDurationNs": 7557000
      },
      {
        "id": "world.getClimateAt",
        "count": 8357,
        "maxDurationNs": 1063000,
        "totalDurationNs": 14241000
      },
      {
        "id": "world.getEtymology",
        "count": 1,
        "maxDurationNs": 4762000,
        "totalDurationNs": 4762000
      },
      {
        "id": "world.getFluidAt",
        "count": 193031,
        "maxDurationNs": 3298000,
        "totalDurationNs": 43707000
      },
      {
        "id": "world.getIdentity",
        "count": 480,
        "maxDurationNs": 6642000,
        "totalDurationNs": 9774000
      },
      {
        "id": "world.getLocationAwareness",
        "count": 1197,
        "maxDurationNs": 7840000,
        "totalDurationNs": 79352000
      },
      {
        "id": "world.getSeed",
        "count": 1,
        "maxDurationNs": 0,
        "totalDurationNs": 0
      },
      {
        "id": "world.getSelectedTile",
        "count": 480,
        "maxDurationNs": 51000,
        "totalDurationNs": 641000
      },
      {
        "id": "world.getSunAngleAt",
        "count": 17,
        "maxDurationNs": 1000,
        "totalDurationNs": 11000
      },
      {
        "id": "world.getToolMode",
        "count": 208,
        "maxDurationNs": 6000,
        "totalDurationNs": 132000
      },
      {
        "id": "world.getWrapWidth",
        "count": 1,
        "maxDurationNs": 1000,
        "totalDurationNs": 1000
      },
      {
        "id": "world.listPlacedLocations",
        "count": 1333,
        "maxDurationNs": 8365000,
        "totalDurationNs": 117660000
      },
      {
        "id": "world.localizeTile",
        "count": 2,
        "maxDurationNs": 4000,
        "totalDurationNs": 6000
      },
      {
        "id": "world.nearestMineDesignation",
        "count": 1514,
        "maxDurationNs": 72000,
        "totalDurationNs": 3108000
      },
      {
        "id": "world.pickTile",
        "count": 6,
        "maxDurationNs": 11000,
        "totalDurationNs": 36000
      },
      {
        "id": "world.setLocationEncounterEpisodeState",
        "count": 2,
        "maxDurationNs": 2000,
        "totalDurationNs": 3000
      },
      {
        "id": "world.setLocationEncounterOccupantState",
        "count": 4,
        "maxDurationNs": 2000,
        "totalDurationNs": 5000
      },
      {
        "id": "world.setWorldCursorHover",
        "count": 1122,
        "maxDurationNs": 49000,
        "totalDurationNs": 2045000
      },
      {
        "id": "world.setZoomCursorHover",
        "count": 28,
        "maxDurationNs": 4000,
        "totalDurationNs": 46000
      }
    ]
  },
  "interpretation": "Inclusive elapsed Haskell action time. Nested totals overlap; not isolated crossing overhead or exclusive CPU time."
}
```

## Capture helper and repeat recipe

Save the Python fence below as `capture_lua_boundary.py`. Use a clean build of
the revision named in the contract. The owner manually launches the rendered
game with `cabal run exe:synarchy -- --port 9123`, loads a world, displays the
HUD, and prepares units to work. Run the helper in a second terminal and play
normally during the window. Use a fresh output path for every run. The helper
records the supplied commit label; independently verify that it matches the
running executable. It does not build, launch, or stop the game.

```bash
python3 capture_lua_boundary.py --port 9123 --seconds 120 \
  --commit 9f92dc0b291784dd6404dba8a21c57e981895cb2 \
  --world "Wind of Havens; seed 1334661219; exploration toward ruin, no combat" \
  --output /tmp/lua-profile-repeat.json
```

This repeats the capture procedure; the original save, exact unit count and
interactive input stream were not archived, so it cannot reproduce the original
workload exactly. For a future comparison, retain those inputs as well.

```python
#!/usr/bin/env python3
"""Owner-run capture for issue 2483. Start with a rendered world already loaded."""
import argparse
import datetime
import json
import platform
import socket
import time
from pathlib import Path

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--port', type=int, default=9123)
p.add_argument('--seconds', type=float, default=120)
p.add_argument('--commit', required=True)
p.add_argument('--world', required=True, help='World name and a short description of the activity')
p.add_argument('--output', type=Path, default=Path('/tmp/foreground-2483-profile.json'))
a = p.parse_args()
if not 1 <= a.seconds <= 3600:
    p.error('--seconds must be between 1 and 3600')
if a.output.exists():
    p.error('output already exists; choose a new --output path to preserve the previous capture')

def query(code):
    with socket.create_connection(('127.0.0.1', a.port), timeout=10) as s:
        s.settimeout(60)
        s.sendall((code + '\n').encode())
        s.shutdown(socket.SHUT_WR)
        pieces = []
        while True:
            chunk = s.recv(65536)
            if not chunk:
                break
            pieces.append(chunk)
    lines = b''.join(pieces).decode().splitlines()
    answer = '\n'.join(line.removeprefix('> ').strip() for line in lines
                       if line.strip() not in ('synarchy debug console', '>')).strip()
    return json.loads(answer)

start_utc = datetime.datetime.now(datetime.timezone.utc).isoformat()
reset_sent = time.monotonic()
reset = query('debug.resetLuaCallStats(); return debug.getLuaCallStats()')
reset_done = time.monotonic()
assert not reset['available'] and not reset['verbs'], 'reset did not produce an empty window'
print(f'Capture started. Play normally for {a.seconds:g} seconds with the HUD visible and units working.', flush=True)
time.sleep(a.seconds)
snapshot_sent = time.monotonic()
stats = query('return debug.getLuaCallStats()')
snapshot_done = time.monotonic()
record = {
    'issue': 2483, 'commit': a.commit, 'world_and_activity': a.world,
    'platform': platform.platform(), 'started_utc': start_utc,
    'capture_kind': 'owner-run rendered session', 'requested_seconds': a.seconds,
    'elapsed_lower_bound_seconds': snapshot_sent - reset_done,
    'elapsed_upper_bound_seconds': snapshot_done - reset_sent,
    'reset_round_trip_seconds': reset_done - reset_sent,
    'snapshot_round_trip_seconds': snapshot_done - snapshot_sent,
    'reset_sequence': reset['sequence'], 'stats': stats,
    'interpretation': 'Inclusive elapsed Haskell action time. Nested totals overlap; not isolated crossing overhead or exclusive CPU time.'
}
with a.output.open('x') as out:
    json.dump(record, out, indent=2)
    out.write('\n')
print(f'Capture saved to {a.output}; {len(stats["verbs"])} called verbs. Return to the foreground session with this path.')
```
