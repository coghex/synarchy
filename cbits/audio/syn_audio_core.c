#include "syn_audio_internal.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

_Static_assert(sizeof(syn_audio_config) == 68, "Audio config ABI changed");
_Static_assert(_Alignof(syn_audio_config) == 4, "Audio config alignment changed");
_Static_assert(offsetof(syn_audio_status, rendered_frames) == 64, "Audio status ABI changed");
_Static_assert(sizeof(syn_audio_status) == 568, "Audio status size changed");
_Static_assert(sizeof(syn_audio_decode_limits) == 32, "Decode limits ABI changed");
_Static_assert(sizeof(syn_audio_instrument_desc) == 64, "Instrument ABI changed");
_Static_assert(sizeof(syn_audio_sound_desc) == 80, "Sound ABI changed");
_Static_assert(sizeof(syn_audio_command) == 128, "Command ABI changed");
_Static_assert(_Alignof(syn_audio_command) == 8, "Command alignment changed");
_Static_assert(sizeof(syn_audio_command_result) == 24, "Command result ABI changed");
_Static_assert(ATOMIC_LLONG_LOCK_FREE == 2, "Audio counters must be lock-free");
_Static_assert(ATOMIC_INT_LOCK_FREE == 2, "Audio flags must be lock-free");

#ifdef SYN_AUDIO_TEST
int syn_audio_test_fail_allocation = -1;
static void* core_calloc(size_t count, size_t size)
{
    if (syn_audio_test_fail_allocation == 0) return NULL;
    if (syn_audio_test_fail_allocation > 0) syn_audio_test_fail_allocation--;
    return calloc(count, size);
}
#else
#define core_calloc calloc
#endif

uint64_t syn_audio_now_ns(void)
{
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (uint64_t)now.tv_sec * 1000000000 + (uint64_t)now.tv_nsec;
}

void syn_audio_error(syn_audio_core* core, const char* message)
{
    snprintf(core->status.last_error, sizeof(core->status.last_error), "%s", message);
}

void syn_audio_default_config(syn_audio_config* config)
{
    *config = (syn_audio_config){SYN_AUDIO_ABI, sizeof(*config), 48000, 256, 1024,
        4096, 128, 256, 256, 2, 1000, 30000, 20, 10, 5, 10, 0.95f};
}

static int valid_config(const syn_audio_config* c)
{
    return c && c->abi_version == SYN_AUDIO_ABI && c->struct_size == sizeof(*c)
        && c->sample_rate >= 22050 && c->sample_rate <= 96000
        && c->chunk_frames >= 64 && c->chunk_frames <= 1024
        && !(c->chunk_frames & (c->chunk_frames - 1))
        && c->target_fill_frames >= 2 * c->chunk_frames
        && c->target_fill_frames <= 1048576
        && c->target_fill_frames % c->chunk_frames == 0
        && c->ring_capacity_frames >= c->target_fill_frames + 2 * c->chunk_frames
        && c->ring_capacity_frames <= 2097152
        && c->ring_capacity_frames % c->chunk_frames == 0
        && c->max_voices >= 16 && c->max_voices <= 1024
        && c->command_batch_limit >= 16 && c->command_batch_limit <= 1024
        && (c->period_frames == 0 || (c->period_frames >= 64 && c->period_frames <= 1024))
        && (c->periods == 0 || (c->periods >= 2 && c->periods <= 4))
        && c->retry_initial_ms > 0 && c->retry_initial_ms <= c->retry_max_ms
        && c->retry_max_ms <= 300000
        && isfinite(c->bus_gain_ms) && c->bus_gain_ms > 0 && c->bus_gain_ms <= 10000
        && isfinite(c->instance_gain_ms) && c->instance_gain_ms > 0 && c->instance_gain_ms <= 10000
        && isfinite(c->pause_out_ms) && c->pause_out_ms > 0 && c->pause_out_ms <= 10000
        && isfinite(c->pause_in_ms) && c->pause_in_ms > 0 && c->pause_in_ms <= 10000
        && isfinite(c->limiter_knee) && c->limiter_knee >= 0.5f && c->limiter_knee < 1;
}

syn_audio_result syn_audio_create(const syn_audio_config* config, uint32_t sink,
                                  syn_audio_core** output)
{
    if (!output) return SYN_AUDIO_INVALID;
    *output = NULL;
    if (!valid_config(config) || sink > SYN_AUDIO_REAL) return SYN_AUDIO_INVALID;
    syn_audio_core* core = core_calloc(1, sizeof(*core));
    if (!core) return SYN_AUDIO_NO_MEMORY;
    core->config = *config;
    core->requested_sink = sink;
    core->retry_ms = config->retry_initial_ms;
    core->status.abi_version = SYN_AUDIO_ABI;
    core->status.struct_size = sizeof(core->status);
    core->status.lifecycle = SYN_AUDIO_STARTING;
    core->status.ring_min = config->ring_capacity_frames;
    atomic_init(&core->callback_frames, 0);
    atomic_init(&core->callbacks, 0);
    atomic_init(&core->underruns, 0);
    atomic_init(&core->device_event, 0);
    core->mix = core_calloc((size_t)config->chunk_frames * 2, sizeof(float));
    core->world = core_calloc((size_t)config->chunk_frames * 2, sizeof(float));
    core->ui = core_calloc((size_t)config->chunk_frames * 2, sizeof(float));
    core->voices = core_calloc(config->max_voices, sizeof(*core->voices));
    core->master_gain.current = core->master_gain.target = 1;
    core->world_gain.current = core->world_gain.target = 1;
    core->ui_gain.current = core->ui_gain.target = 1;
    core->zoom_gain.current = core->zoom_gain.target = 1;
    core->range_scale = 1;
    if (!core->mix || !core->world || !core->ui || !core->voices) {
        syn_audio_destroy(core); return SYN_AUDIO_NO_MEMORY;
    }
    if (ma_pcm_rb_init(ma_format_f32, 2, config->ring_capacity_frames, NULL, NULL,
                       &core->ring) != MA_SUCCESS) {
        syn_audio_destroy(core); return SYN_AUDIO_NO_MEMORY;
    }
    core->ring_ready = 1;
    syn_audio_result result = syn_audio_device_init(core, sink);
    if (result != SYN_AUDIO_OK && sink == SYN_AUDIO_REAL)
        result = syn_audio_device_init(core, SYN_AUDIO_NULL);
    if (result != SYN_AUDIO_OK) { syn_audio_destroy(core); return result; }
    *output = core;
    return SYN_AUDIO_OK;
}

syn_audio_result syn_audio_service(syn_audio_core* core, const syn_audio_command* commands,
    uint32_t count, syn_audio_command_result* results, syn_audio_status* status)
{
    if (!core) return SYN_AUDIO_INVALID;
    uint64_t begin = syn_audio_now_ns();
    syn_audio_result applied = syn_audio_commands(core, commands, count, results);
    if (applied != SYN_AUDIO_OK) return applied;
    syn_audio_device_recover(core);
    syn_audio_refill(core);
    core->status.service_ns = syn_audio_now_ns() - begin;
    syn_audio_get_status(core, status);
    return SYN_AUDIO_OK;
}

void syn_audio_refill(syn_audio_core* core)
{
    uint32_t fill = ma_pcm_rb_available_read(&core->ring);
    if (fill < core->status.ring_min) core->status.ring_min = fill;
    /* Bound this pass even when the callback consumes while we produce. */
    uint32_t remaining = core->config.target_fill_frames > fill
        ? core->config.target_fill_frames - fill : 0;
    while (remaining) {
        uint32_t frames = remaining < core->config.chunk_frames ? remaining : core->config.chunk_frames;
        syn_audio_mix(core, core->mix, frames);
        uint32_t written = syn_audio_ring_write(core, core->mix, frames);
        remaining -= written;
        if (written != frames) break;
    }
    fill = ma_pcm_rb_available_read(&core->ring);
    if (fill > core->status.ring_max) core->status.ring_max = fill;
}

syn_audio_result syn_audio_start(syn_audio_core* core)
{
    if (!core) return SYN_AUDIO_INVALID;
    if (core->started) return SYN_AUDIO_OK;
    core->catalog_sealed = 1;
    /* Running intent survives a temporary device failure so service can retry.
       activate checks device_ready before touching miniaudio (including reset
       while disabled, where the previous device has already been destroyed). */
    core->started = 1;
    return syn_audio_device_activate(core);
}

void syn_audio_stop(syn_audio_core* core)
{
    if (!core) return;
    if (core->device_ready && core->started) ma_device_stop(&core->device);
    core->started = 0;
    core->status.lifecycle = SYN_AUDIO_STOPPED;
}

void syn_audio_destroy(syn_audio_core* core)
{
    if (!core) return;
    syn_audio_stop(core);
    syn_audio_device_uninit(core);
    if (core->ring_ready) ma_pcm_rb_uninit(&core->ring);
    syn_audio_catalog_free(core);
    free(core->mix);
    free(core->world); free(core->ui); free(core->voices);
    free(core);
}

syn_audio_result syn_audio_render_offline(syn_audio_core* core, float* output,
                                         uint32_t frames, syn_audio_status* status)
{
    if (!core || (!output && frames)) return SYN_AUDIO_INVALID;
    if (core->started) return SYN_AUDIO_BAD_STATE;
    if (frames > 1048576) return SYN_AUDIO_LIMIT;
    if (frames) syn_audio_mix(core, output, frames);
    syn_audio_get_status(core, status);
    return SYN_AUDIO_OK;
}

void syn_audio_get_status(syn_audio_core* core, syn_audio_status* status)
{
    if (!core || !status) return;
    core->status.callback_frames = atomic_load_explicit(&core->callback_frames, memory_order_relaxed);
    core->status.callbacks = atomic_load_explicit(&core->callbacks, memory_order_relaxed);
    core->status.underruns = atomic_load_explicit(&core->underruns, memory_order_relaxed);
    core->status.ring_fill = ma_pcm_rb_available_read(&core->ring);
    *status = core->status;
}

const char* syn_audio_last_error(const syn_audio_core* core)
{
    return core ? core->status.last_error : "No audio core";
}
