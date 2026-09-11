#include "audio_test_helpers.h"

static void loops_and_concurrency(void)
{
    syn_audio_core* core = test_core();
    syn_audio_instrument_desc instrument = test_instrument();
    syn_audio_sound_desc desc = test_sound(SYN_AUDIO_SYNTH, 0);
    desc.max_instances = 2; desc.cooldown_ms = 1;
    uint32_t sound = test_add_synth(core, instrument, desc);
    desc.cooldown_ms = 0; desc.max_instances = 128; desc.priority = 60;
    uint32_t high = test_add_synth(core, instrument, desc);
    uint32_t peer = test_add_synth(core, instrument, desc);
    desc.priority = 40;
    uint32_t low = test_add_synth(core, instrument, desc);
    syn_audio_command start = test_command(SYN_AUDIO_START_LOOP, sound, 1);
    assert(test_submit(core, start).result == SYN_AUDIO_OK);
    assert(test_submit(core, start).result == SYN_AUDIO_OK); /* Idempotent through cooldown. */
    assert(core->status.active_voices == 1 && core->status.accepted == 1);
    start.sound_handle = high;
    assert(test_submit(core, start).result == SYN_AUDIO_CONFLICT);
    start.sound_handle = sound; start.loop_key = 2;
    assert(test_submit(core, start).result == SYN_AUDIO_COOLDOWN);
    float output[2048];
    test_render(core, output, 48);
    assert(test_submit(core, start).result == SYN_AUDIO_OK);
    test_render(core, output, 48);
    start.loop_key = 3;
    assert(test_submit(core, start).result == SYN_AUDIO_CAPACITY);
    assert(test_submit(core, test_command(SYN_AUDIO_STOP_LOOP, 0, 1)).result == SYN_AUDIO_OK);
    assert(core->status.active_loops == 1 && core->status.active_voices == 2);
    assert(test_submit(core, test_command(SYN_AUDIO_UPDATE_LOOP, 0, 1)).result == SYN_AUDIO_MISSING);
    assert(test_submit(core, start).result == SYN_AUDIO_CAPACITY); /* Fades consume capacity. */
    test_render(core, output, 480);
    start.loop_key = 1;
    assert(test_submit(core, start).result == SYN_AUDIO_OK);
    assert(test_submit(core, test_command(SYN_AUDIO_CLEAR_WORLD, 0, 0)).result == SYN_AUDIO_OK);
    for (uint32_t i = 0; i < 128; ++i)
        assert(test_submit(core, test_command(SYN_AUDIO_START_LOOP, high, i + 100)).result == SYN_AUDIO_OK);
    assert(test_submit(core, test_command(SYN_AUDIO_PLAY, low, 0)).result == SYN_AUDIO_PRIORITY);
    /* Same priority can steal: all start frames tie, so slot zero loses. */
    syn_audio_command_result result = test_submit(core, test_command(SYN_AUDIO_PLAY, peer, 0));
    assert(result.result == SYN_AUDIO_OK && result.evicted_loop_key == 100
        && result.steal_reason == SYN_AUDIO_STEAL_GLOBAL);
    assert(core->status.steals == 1 && core->status.active_voices == 128 && core->status.peak_loops == 128);
    syn_audio_destroy(core);
}

static void spatial_buses_and_reset(void)
{
    syn_audio_core* core = test_core();
    syn_audio_instrument_desc instrument = test_instrument();
    syn_audio_sound_desc desc = test_sound(SYN_AUDIO_SYNTH, 0); desc.spatial = 1;
    uint32_t world = test_add_synth(core, instrument, desc);
    desc.spatial = 0; desc.bus = SYN_AUDIO_UI; desc.freeze = 0;
    uint32_t ui = test_add_synth(core, instrument, desc);
    syn_audio_command start = test_command(SYN_AUDIO_START_LOOP, world, 1);
    start.values[0] = 10;
    assert(test_submit(core, start).result == SYN_AUDIO_OK);
    float output[4800];
    test_render(core, output, 2400);
    assert(test_energy(output, 2400, 0) == 0 && test_energy(output, 2400, 1) > 0.001);
    syn_audio_command rebase = test_command(SYN_AUDIO_REBASE, 0, 0);
    rebase.values[0] = rebase.values[5] = -1; rebase.values[10] = 1;
    assert(test_submit(core, rebase).result == SYN_AUDIO_OK);
    test_render(core, output, 2400);
    assert(test_energy(output, 2400, 1) == 0 && test_energy(output, 2400, 0) > 0.001);
    syn_audio_command update = test_command(SYN_AUDIO_UPDATE_LOOP, 0, 1);
    update.flags = SYN_AUDIO_HAS_POSITION; update.values[2] = 100;
    assert(test_submit(core, update).result == SYN_AUDIO_OK);
    test_render(core, output, 2400);
    assert(test_energy(output, 2400, 0) == 0 && test_energy(output, 2400, 1) == 0);
    update.values[2] = 0;
    assert(test_submit(core, update).result == SYN_AUDIO_OK);
    test_render(core, output, 2400);
    assert(test_energy(output, 2400, 0) > 0.001);
    assert(test_energy(output, 2400, 0) == test_energy(output, 2400, 1));
    syn_audio_command zoom = test_command(SYN_AUDIO_WORLD_MIX, 0, 0); zoom.values[0] = 1;
    assert(test_submit(core, zoom).result == SYN_AUDIO_OK);
    test_render(core, output, 2400);
    for (uint32_t i = 960 * 2; i < 4800; ++i) assert(output[i] == 0);
    assert(core->voices[0].age == 12000); /* Zoom mute advanced source state. */
    assert(test_submit(core, test_command(SYN_AUDIO_START_LOOP, ui, 2)).result == SYN_AUDIO_OK);
    syn_audio_command pause = test_command(SYN_AUDIO_PAUSE, 0, 0); pause.flags = 1;
    assert(test_submit(core, pause).result == SYN_AUDIO_OK);
    test_render(core, output, 2400);
    assert(test_energy(output, 2400, 0) > 0.001); /* UI ignores pause and World zoom. */
    assert(test_submit(core, test_command(SYN_AUDIO_CLEAR_WORLD, 0, 0)).result == SYN_AUDIO_OK);
    assert(core->status.active_voices == 1 && core->status.active_loops == 1);
    assert(core->voices[1].active && core->voices[1].loop_key == 2);
    assert(syn_audio_service(core, NULL, 0, NULL, NULL) == SYN_AUDIO_OK);
    assert(ma_pcm_rb_available_read(&core->ring) > 0);
    assert(test_submit(core, test_command(SYN_AUDIO_RESET, 0, 0)).result == SYN_AUDIO_OK);
    assert(core->status.active_voices == 0 && core->status.active_loops == 0);
    assert(ma_pcm_rb_available_read(&core->ring) == 0 && !core->player_paused);
    syn_audio_destroy(core);
}

static void wrapped_frame_and_steal_reason(void)
{
    syn_audio_core* core = test_core();
    syn_audio_sound_desc sound = test_sound(SYN_AUDIO_SYNTH, 0);
    sound.spatial = 1; sound.max_instances = 1; sound.overflow = SYN_AUDIO_STEAL_OLDEST;
    uint32_t handle = test_add_synth(core, test_instrument(), sound);
    syn_audio_command start = test_command(SYN_AUDIO_START_LOOP, handle, 1);
    start.values[0] = 7;
    assert(test_submit(core, start).result == SYN_AUDIO_OK);
    start.loop_key = 2;
    syn_audio_command_result stolen = test_submit(core, start);
    assert(stolen.result == SYN_AUDIO_OK && stolen.steal_reason == SYN_AUDIO_STEAL_PER_SOUND);
    assert(stolen.evicted_loop_key == 1);
    syn_audio_command move = test_command(SYN_AUDIO_REBASE, 0, 0);
    move.values[0] = move.values[5] = move.values[10] = 1;
    move.values[3] = 2;
    assert(test_submit(core, move).result == SYN_AUDIO_OK);
    syn_audio_command wrap = test_command(SYN_AUDIO_WRAP_FRAME, 0, 0);
    wrap.values[0] = 16;
    assert(test_submit(core, wrap).result == SYN_AUDIO_OK);
    assert(core->voices[0].xyz[0] == -7);
    wrap.values[3] = 16; /* Non-orthogonal periods are refused atomically. */
    assert(test_submit(core, wrap).result == SYN_AUDIO_INVALID);
    assert(core->voices[0].xyz[0] == -7);
    syn_audio_destroy(core);
}

void audio_policy_tests(void)
{
    loops_and_concurrency(); spatial_buses_and_reset(); wrapped_frame_and_steal_reason();
    puts("PASS: logical loops, cooldown/capacity/priority eviction, pan/rebase/distance, buses/zoom/pause/reset");
}
