/* Failure injection is compiled only into the standalone test executable.
   Simulated physical attempts still initialize ONLY ma_backend_null. */
#include "audio_test_helpers.h"

static void service(syn_audio_core* core)
{
    assert(syn_audio_service(core, NULL, 0, NULL, NULL) == SYN_AUDIO_OK);
    if (core->context_ready) assert(core->context.backend == ma_backend_null);
}

static void recover_now(syn_audio_core* core)
{
    atomic_store(&core->device_event, 1);
    service(core);
}

static void null_stays_null(void)
{
    syn_audio_core* core = test_core();
    core->test_force_null = 1;
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    uint32_t attempts = core->test_init_attempts[SYN_AUDIO_NULL];
    recover_now(core);
    assert(core->test_init_attempts[SYN_AUDIO_NULL] == attempts + 1);
    assert(core->test_init_attempts[SYN_AUDIO_REAL] == 0);
    assert(core->status.lifecycle == SYN_AUDIO_RUNNING_NULL);
    syn_audio_stop(core);
    attempts = core->test_init_attempts[SYN_AUDIO_NULL];
    recover_now(core);
    assert(core->test_init_attempts[SYN_AUDIO_NULL] == attempts);
    assert(core->status.lifecycle == SYN_AUDIO_STOPPED);
    syn_audio_destroy(core);
}

static void fallback_on_init_and_start(void)
{
    syn_audio_core* core = test_core();
    core->test_force_null = 1;
    core->requested_sink = SYN_AUDIO_REAL;
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    core->test_fail_init[SYN_AUDIO_REAL] = 1;
    uint32_t null_attempts = core->test_init_attempts[SYN_AUDIO_NULL];
    recover_now(core);
    assert(core->test_fail_init[SYN_AUDIO_REAL] == 0);
    assert(core->test_init_attempts[SYN_AUDIO_NULL] == null_attempts + 1);
    assert(core->status.lifecycle == SYN_AUDIO_DEGRADED_NULL);
    core->test_fail_start[SYN_AUDIO_REAL] = 1;
    recover_now(core);
    assert(core->test_fail_start[SYN_AUDIO_REAL] == 0);
    assert(core->test_init_attempts[SYN_AUDIO_NULL] == null_attempts + 2);
    assert(core->status.lifecycle == SYN_AUDIO_DEGRADED_NULL);
    assert(core->device_ready && core->started);
    syn_audio_destroy(core);
}

static void disabled_backoff_and_reset(void)
{
    syn_audio_core* core = test_core();
    core->test_force_null = 1;
    core->requested_sink = SYN_AUDIO_REAL;
    syn_audio_sound_desc sound = test_sound(SYN_AUDIO_SYNTH, 0);
    uint32_t handle = test_add_synth(core, test_instrument(), sound);
    assert(test_submit(core, test_command(SYN_AUDIO_START_LOOP, handle, 9)).result == SYN_AUDIO_OK);
    syn_audio_command volumes = test_command(SYN_AUDIO_VOLUMES, 0, 0);
    volumes.values[0] = 0.5f; volumes.values[1] = 0.25f; volumes.values[2] = 0.75f;
    assert(test_submit(core, volumes).result == SYN_AUDIO_OK);
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    core->retry_ms = core->config.retry_max_ms;
    core->test_fail_start[SYN_AUDIO_REAL] = 1;
    core->test_fail_start[SYN_AUDIO_NULL] = 1;
    recover_now(core);
    assert(core->status.lifecycle == SYN_AUDIO_DISABLED);
    assert(!core->device_ready && !core->context_ready && core->started);
    assert(core->retry_ms == core->config.retry_max_ms);
    assert(atomic_load(&core->device_event) == 0);
    uint32_t attempts = core->test_init_attempts[SYN_AUDIO_REAL];
    service(core);
    assert(core->test_init_attempts[SYN_AUDIO_REAL] == attempts);
    /* A session reset remains safe when neither device could start. */
    core->test_fail_init[SYN_AUDIO_REAL] = 1;
    core->test_fail_init[SYN_AUDIO_NULL] = 1;
    syn_audio_command reset = test_command(SYN_AUDIO_RESET, 0, 0);
    syn_audio_command_result result;
    assert(syn_audio_service(core, &reset, 1, &result, NULL) == SYN_AUDIO_OK);
    assert(result.result == SYN_AUDIO_OK && core->started && !core->device_ready);
    assert(core->status.active_voices == 0 && core->status.active_loops == 0);
    assert(core->status.sound_count == 1 && !core->sounds[0].has_started);
    assert(core->master_gain.target == 0.5f && core->world_gain.target == 0.25f);
    assert(core->ui_gain.target == 0.75f);
    core->retry_at_ns = 0;
    service(core);
    assert(core->status.lifecycle == SYN_AUDIO_DEGRADED_NULL && core->device_ready);
    syn_audio_destroy(core);
}

void audio_recovery_tests(void)
{
    null_stays_null();
    fallback_on_init_and_start();
    disabled_backoff_and_reset();
    puts("PASS: null-only recovery, init/start fallback, disabled backoff and session reset");
}
