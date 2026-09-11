#include "syn_audio_internal.h"
#include <stdio.h>

static void audio_callback(ma_device* device, void* output, const void* input,
                           ma_uint32 frames)
{
    (void)input;
    syn_audio_ring_read(device->pUserData, output, frames);
}

static void notification(const ma_device_notification* event)
{
    syn_audio_core* core = event->pDevice->pUserData;
    if (event->type != ma_device_notification_type_started)
        atomic_store_explicit(&core->device_event, 1, memory_order_relaxed);
}

syn_audio_result syn_audio_device_init(syn_audio_core* core, uint32_t sink)
{
#ifdef SYN_AUDIO_TEST
    core->test_attempt_sink = sink;
    core->test_init_attempts[sink]++;
    if (core->test_fail_init[sink]) {
        core->test_fail_init[sink]--;
        return SYN_AUDIO_DEVICE_ERROR;
    }
    if (core->test_force_null) sink = SYN_AUDIO_NULL;
#endif
    ma_backend null_backend = ma_backend_null;
    ma_context_config context_config = ma_context_config_init();
    ma_result result = ma_context_init(sink == SYN_AUDIO_NULL ? &null_backend : NULL,
                                      sink == SYN_AUDIO_NULL ? 1 : 0,
                                      &context_config, &core->context);
    if (result != MA_SUCCESS) {
        syn_audio_error(core, ma_result_description(result));
        return SYN_AUDIO_DEVICE_ERROR;
    }
    core->context_ready = 1;
    /* A default backend list may end in null. Treat it as degraded real output. */
    core->status.sink = core->context.backend == ma_backend_null ? SYN_AUDIO_NULL : SYN_AUDIO_REAL;
    ma_device_config device_config = ma_device_config_init(ma_device_type_playback);
    device_config.playback.format = ma_format_f32;
    device_config.playback.channels = 2;
    device_config.sampleRate = core->config.sample_rate;
    device_config.periodSizeInFrames = core->config.period_frames;
    device_config.periods = core->config.periods;
    device_config.dataCallback = audio_callback;
    device_config.notificationCallback = notification;
    device_config.pUserData = core;
    device_config.noPreSilencedOutputBuffer = MA_TRUE;
    device_config.noClip = MA_TRUE;
    result = ma_device_init(&core->context, &device_config, &core->device);
    if (result != MA_SUCCESS) {
        syn_audio_error(core, ma_result_description(result));
        syn_audio_device_uninit(core);
        return SYN_AUDIO_DEVICE_ERROR;
    }
    core->device_ready = 1;
    core->status.backend = core->context.backend;
    core->status.sample_rate = core->device.sampleRate;
    core->status.period_frames = core->device.playback.internalPeriodSizeInFrames;
    snprintf(core->status.device_name, sizeof(core->status.device_name), "%.*s",
             (int)sizeof(core->status.device_name) - 1, core->device.playback.name);
    atomic_store_explicit(&core->device_event, 0, memory_order_relaxed);
    return SYN_AUDIO_OK;
}

void syn_audio_device_uninit(syn_audio_core* core)
{
    if (core->device_ready) {
        ma_device_uninit(&core->device); /* Stops and joins callback before return. */
        core->device_ready = 0;
    }
    if (core->context_ready) {
        ma_context_uninit(&core->context);
        core->context_ready = 0;
    }
    /* Intentional teardown can emit a stopped notification. After the join no
       callback can race this clear; it must not bypass a failed retry's delay. */
    atomic_store_explicit(&core->device_event, 0, memory_order_relaxed);
}

static syn_audio_result start_ready(syn_audio_core* core)
{
    if (!core->device_ready) return SYN_AUDIO_DEVICE_ERROR;
#ifdef SYN_AUDIO_TEST
    if (core->test_fail_start[core->test_attempt_sink]) {
        core->test_fail_start[core->test_attempt_sink]--;
        return SYN_AUDIO_DEVICE_ERROR;
    }
#endif
    syn_audio_refill(core); /* Fill before the callback can consume. */
    ma_result result = ma_device_start(&core->device);
    if (result == MA_SUCCESS) return SYN_AUDIO_OK;
    syn_audio_error(core, ma_result_description(result));
    return SYN_AUDIO_DEVICE_ERROR;
}

/* Shared initial/recovery start path: BOTH init and start failures fall back. */
syn_audio_result syn_audio_device_activate(syn_audio_core* core)
{
    syn_audio_result result = core->device_ready ? SYN_AUDIO_OK
        : syn_audio_device_init(core, core->requested_sink);
    if (result == SYN_AUDIO_OK) result = start_ready(core);
    if (result != SYN_AUDIO_OK && core->requested_sink == SYN_AUDIO_REAL) {
        syn_audio_device_uninit(core);
        ma_pcm_rb_reset(&core->ring);
        result = syn_audio_device_init(core, SYN_AUDIO_NULL);
        if (result == SYN_AUDIO_OK) result = start_ready(core);
    }
    if (result == SYN_AUDIO_OK) {
        core->status.lifecycle = core->status.sink == SYN_AUDIO_REAL
            ? SYN_AUDIO_RUNNING_REAL : core->requested_sink == SYN_AUDIO_REAL
                ? SYN_AUDIO_DEGRADED_NULL : SYN_AUDIO_RUNNING_NULL;
    } else {
        syn_audio_device_uninit(core);
        core->status.lifecycle = SYN_AUDIO_DISABLED;
        syn_audio_error(core, "Audio device recovery failed; will retry");
    }
    core->status.transitions++;
    if (core->status.lifecycle == SYN_AUDIO_RUNNING_REAL || core->status.lifecycle == SYN_AUDIO_RUNNING_NULL)
        core->retry_ms = core->config.retry_initial_ms;
    core->retry_at_ns = syn_audio_now_ns() + (uint64_t)core->retry_ms * 1000000;
    if (core->status.lifecycle == SYN_AUDIO_DEGRADED_NULL || core->status.lifecycle == SYN_AUDIO_DISABLED) {
      if (core->retry_ms < core->config.retry_max_ms)
        core->retry_ms = core->retry_ms > core->config.retry_max_ms / 2
            ? core->config.retry_max_ms : core->retry_ms * 2;
    }
    return result;
}

/* Only the worker calls this. Forced-null recovery stays on the null backend. */
void syn_audio_device_recover(syn_audio_core* core)
{
    if (!core->started) return;
    uint64_t now = syn_audio_now_ns();
    uint32_t event = atomic_exchange_explicit(&core->device_event, 0, memory_order_relaxed);
    uint32_t healthy = core->status.lifecycle == SYN_AUDIO_RUNNING_REAL
        || core->status.lifecycle == SYN_AUDIO_RUNNING_NULL;
    if (!event && (healthy || now < core->retry_at_ns)) return;
    syn_audio_device_uninit(core);
    ma_pcm_rb_reset(&core->ring); /* Both endpoints are stopped here. */
    syn_audio_device_activate(core);
}
