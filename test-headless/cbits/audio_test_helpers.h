#ifndef AUDIO_TEST_HELPERS_H
#define AUDIO_TEST_HELPERS_H
#include "syn_audio_internal.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

static inline syn_audio_core* test_core(void)
{
    syn_audio_config config;
    syn_audio_default_config(&config);
    syn_audio_core* core = NULL;
    assert(syn_audio_create(&config, SYN_AUDIO_NULL, &core) == SYN_AUDIO_OK);
    assert(core->context.backend == ma_backend_null);
    return core;
}

static inline syn_audio_instrument_desc test_instrument(void)
{
    return (syn_audio_instrument_desc){.abi_version = SYN_AUDIO_ABI,
        .struct_size = sizeof(syn_audio_instrument_desc), .waveform = SYN_AUDIO_SINE,
        .seed = 42, .frequency = 440, .sustain = 1, .cutoff = 1000, .q = 0.707f,
        .gain_db = -18, .gate_ms = 1000};
}

static inline syn_audio_sound_desc test_sound(uint32_t kind, uint32_t handle)
{
    return (syn_audio_sound_desc){.abi_version = SYN_AUDIO_ABI,
        .struct_size = sizeof(syn_audio_sound_desc), .source_kind = kind, .source_handle = handle,
        .bus = SYN_AUDIO_WORLD, .priority = 50, .max_instances = 128,
        .loop = 1, .freeze = 1, .seed = 12345, .max_distance = 100, .vertical_scale = 1,
        .gate_ms = 1000, .stop_fade_ms = 10};
}

static inline uint32_t test_add_synth(syn_audio_core* core, syn_audio_instrument_desc instrument,
                                      syn_audio_sound_desc sound)
{
    uint32_t handle = 0;
    assert(syn_audio_add_instrument(core, &instrument, &handle) == SYN_AUDIO_OK);
    sound.source_kind = SYN_AUDIO_SYNTH; sound.source_handle = handle;
    assert(syn_audio_add_sound(core, &sound, &handle) == SYN_AUDIO_OK);
    return handle;
}

static inline syn_audio_command test_command(uint32_t kind, uint32_t sound, uint64_t loop)
{
    return (syn_audio_command){.abi_version = SYN_AUDIO_ABI,
        .struct_size = sizeof(syn_audio_command), .kind = kind,
        .sound_handle = sound, .loop_key = loop};
}

static inline syn_audio_command_result test_submit(syn_audio_core* core, syn_audio_command command)
{
    syn_audio_command_result result;
    assert(syn_audio_submit_offline(core, &command, 1, &result) == SYN_AUDIO_OK);
    assert(result.abi_version == SYN_AUDIO_ABI && result.struct_size == sizeof(result));
    return result;
}

static inline void test_render(syn_audio_core* core, float* output, uint32_t frames)
{
    assert(syn_audio_render_offline(core, output, frames, NULL) == SYN_AUDIO_OK);
}

static inline double test_energy(const float* output, uint32_t frames, uint32_t channel)
{
    double energy = 0;
    for (uint32_t i = 0; i < frames; ++i) energy += output[i * 2 + channel] * output[i * 2 + channel];
    return energy / frames;
}

void audio_dsp_tests(void);
void audio_policy_tests(void);
void audio_decoder_tests(void);
void audio_recovery_tests(void);
#endif
