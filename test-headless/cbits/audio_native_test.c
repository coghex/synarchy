/* Native behavior tests. This executable must never initialize a real backend. */
#include "syn_audio_internal.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "audio_test_helpers.h"

static syn_audio_core* create(void)
{
    syn_audio_config config;
    syn_audio_default_config(&config);
    syn_audio_core* core = NULL;
    assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_OK);
    assert(core && core->context.backend == ma_backend_null);
    return core;
}

static void invalid_configuration(void)
{
    syn_audio_config config;
    syn_audio_core* core = (syn_audio_core*)1;
    syn_audio_default_config(&config);
    config.sample_rate = 0;
    assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_INVALID);
    assert(core == NULL);
    syn_audio_default_config(&config);
    config.target_fill_frames = config.ring_capacity_frames;
    assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_INVALID);
    syn_audio_default_config(&config);
    config.limiter_knee = NAN;
    assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_INVALID);
    syn_audio_default_config(&config);
    config.struct_size--;
    assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_INVALID);
    assert(syn_audio_create(NULL, SYN_AUDIO_NULL, &core) == SYN_AUDIO_INVALID);
    syn_audio_stop(NULL);
    syn_audio_destroy(NULL);
    syn_audio_default_config(&config);
    for (int allocation = 0; allocation < 5; ++allocation) {
        syn_audio_test_fail_allocation = allocation;
        core = (syn_audio_core*)1;
        assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_NO_MEMORY);
        assert(core == NULL);
    }
    syn_audio_test_fail_allocation = -1;
}

static void offline_and_ring(void)
{
    syn_audio_core* core = create();
    float samples[10000], output[10000];
    syn_audio_status status;
    for (size_t i = 0; i < 10000; ++i) samples[i] = (float)(i % 97) / 97;
    for (size_t i = 0; i < 10000; ++i) output[i] = NAN;
    assert(syn_audio_render_offline(core, output, 257, &status) == SYN_AUDIO_OK);
    for (int i = 0; i < 514; ++i) assert(output[i] == 0);
    assert(isnan(output[514])); /* Exact output boundary, including odd frame count. */
    assert(status.rendered_frames == 257 && status.callbacks == 0);
    assert(syn_audio_ring_write(core, samples, 3000) == 3000);
    syn_audio_ring_read(core, output, 2500);
    assert(memcmp(output, samples, 2500 * 2 * sizeof(float)) == 0);
    /* The second write crosses the physical ring boundary. */
    assert(syn_audio_ring_write(core, samples + 6000, 1500) == 1500);
    syn_audio_ring_read(core, output, 2000);
    assert(memcmp(output, samples + 5000, 2000 * 2 * sizeof(float)) == 0);
    for (size_t i = 0; i < 10000; ++i) output[i] = NAN;
    assert(syn_audio_ring_write(core, samples, 3) == 3);
    syn_audio_ring_read(core, output, 11);
    assert(memcmp(output, samples, 6 * sizeof(float)) == 0);
    for (int i = 6; i < 22; ++i) assert(output[i] == 0);
    assert(isnan(output[22]));
    syn_audio_get_status(core, &status);
    assert(status.underruns == 1 && status.callback_frames == 4511);
    assert(status.ring_fill == 0);
    assert(syn_audio_ring_write(core, samples, 5000) == 4096);
    assert(syn_audio_ring_write(core, samples, 1) == 0);
    syn_audio_ring_read(core, output, 4096);
    assert(memcmp(output, samples, 8192 * sizeof(float)) == 0);
    syn_audio_destroy(core);
}

static void null_lifecycle(void)
{
    syn_audio_core* core = create();
    syn_audio_status before, after;
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    assert(syn_audio_render_offline(core, NULL, 0, NULL) == SYN_AUDIO_BAD_STATE);
    struct timespec pause = {0, 2000000};
    /* Poll up to one second: callback scheduling is not a fixed sleep oracle. */
    for (int i = 0; i < 500; ++i) {
        assert(syn_audio_service(core, NULL, 0, NULL, &before) == SYN_AUDIO_OK);
        if (before.callbacks >= 3) break;
        nanosleep(&pause, NULL);
    }
    assert(before.callbacks >= 3 && before.callback_frames > 0);
    assert(before.lifecycle == SYN_AUDIO_RUNNING_NULL);
    assert(before.backend == ma_backend_null && before.sink == SYN_AUDIO_NULL);
    assert(before.ring_max <= core->config.target_fill_frames);
    syn_audio_stop(core);
    syn_audio_stop(core);
    syn_audio_get_status(core, &before);
    for (int i = 0; i < 10; ++i) nanosleep(&pause, NULL);
    syn_audio_get_status(core, &after);
    assert(after.callbacks == before.callbacks); /* Stop joined the callback. */
    assert(after.lifecycle == SYN_AUDIO_STOPPED);
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    syn_audio_destroy(core); /* Destruction while started also joins. */
}

int main(void)
{
    invalid_configuration();
    offline_and_ring();
    null_lifecycle();
    audio_dsp_tests();
    audio_policy_tests();
    audio_decoder_tests();
    audio_recovery_tests();
    puts("PASS: native audio configuration, exact silence, ring wrap/full/underrun, forced-null lifecycle");
    return 0;
}
