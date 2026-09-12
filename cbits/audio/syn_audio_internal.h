#ifndef SYN_AUDIO_INTERNAL_H
#define SYN_AUDIO_INTERNAL_H

#if defined(__linux__) && !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200809L
#endif

#include "syn_audio.h"
#include "miniaudio.h"
#include <stdatomic.h>
#include <stddef.h>

#ifdef SYN_AUDIO_TEST
extern int syn_audio_test_fail_allocation;
#endif

/* Shared layouts are private to C. BuildSupport.AudioDependencies invalidates
   every consuming object when this header changes, including incremental builds. */

typedef struct { float current, target, step; uint32_t left; } syn_audio_ramp;
typedef struct { char* path; float* pcm; uint64_t frames; uint32_t mp3; } syn_audio_sample;
typedef struct {
    syn_audio_sound_desc desc;
    uint64_t last_start;
    uint32_t has_started;
} syn_audio_sound;
typedef struct {
    uint32_t active, sound, looping, stopping, noise;
    uint64_t loop_key, start_frame, age, attack, decay, gate, release;
    double cursor, phase, phase_step;
    float triangle, envelope, release_level, pitch_ratio, synth_gain, sound_gain;
    float xyz[3], last_output[2];
    float b0, b1, b2, a1, a2, z1[2], z2[2];
    syn_audio_ramp gain, stop_gain, pause_gain;
} syn_audio_voice;

struct syn_audio_core {
    syn_audio_config config;
    syn_audio_status status;
    ma_context context;
    ma_device device;
    ma_pcm_rb ring;
    float *mix, *world, *ui;
    syn_audio_voice* voices;
    syn_audio_sample* samples;
    syn_audio_instrument_desc* instruments;
    syn_audio_sound* sounds;
    syn_audio_ramp master_gain, world_gain, ui_gain, zoom_gain;
    float range_scale;
    uint32_t player_paused, catalog_sealed;
    uint64_t voice_sequence;
    uint32_t requested_sink, context_ready, device_ready, ring_ready, started;
    uint32_t retry_ms;
    uint64_t retry_at_ns;
    _Atomic uint64_t callback_frames, callbacks, underruns;
    _Atomic uint32_t device_event;
#ifdef SYN_AUDIO_TEST
    /* Deterministic failure injection exists only in the standalone C test. */
    uint32_t test_force_null, test_attempt_sink;
    uint32_t test_fail_init[2], test_fail_start[2], test_init_attempts[2];
#endif
};

uint64_t syn_audio_now_ns(void);
void syn_audio_error(syn_audio_core*, const char*);
uint32_t syn_audio_ring_write(syn_audio_core*, const float*, uint32_t);
void syn_audio_ring_read(syn_audio_core*, float*, uint32_t);
syn_audio_result syn_audio_device_init(syn_audio_core*, uint32_t sink);
void syn_audio_device_uninit(syn_audio_core*);
void syn_audio_device_recover(syn_audio_core*);
syn_audio_result syn_audio_device_activate(syn_audio_core*);
void syn_audio_refill(syn_audio_core*);
void syn_audio_mix(syn_audio_core*, float*, uint32_t);
syn_audio_result syn_audio_commands(syn_audio_core*, const syn_audio_command*,
    uint32_t, syn_audio_command_result*);
void syn_audio_catalog_free(syn_audio_core*);
void syn_audio_voice_begin(syn_audio_core*, syn_audio_voice*, uint32_t sound,
    const syn_audio_command*);
void syn_audio_voice_retire(syn_audio_core*, syn_audio_voice*);
void syn_audio_ramp_set(syn_audio_ramp*, float, float ms, uint32_t rate);
float syn_audio_ramp_next(syn_audio_ramp*);
float syn_audio_db(float);
uint64_t syn_audio_frames(const syn_audio_core*, float ms);
float syn_audio_envelope(syn_audio_voice*, const syn_audio_instrument_desc*,
    uint64_t attack, uint64_t decay, uint64_t gate, uint64_t release);
void syn_audio_synth(syn_audio_core*, syn_audio_voice*, float* stereo);

#endif
