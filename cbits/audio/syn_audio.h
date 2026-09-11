#ifndef SYN_AUDIO_H
#define SYN_AUDIO_H

/* Worker-owned ABI. No miniaudio layout or callback crosses into Haskell. */
#include <stdint.h>

#define SYN_AUDIO_ABI 1u
typedef struct syn_audio_core syn_audio_core;
typedef uint32_t syn_audio_result;
enum {
    SYN_AUDIO_OK, SYN_AUDIO_INVALID, SYN_AUDIO_NO_MEMORY, SYN_AUDIO_DEVICE_ERROR,
    SYN_AUDIO_BAD_STATE, SYN_AUDIO_DECODE_ERROR, SYN_AUDIO_LIMIT,
    SYN_AUDIO_MISSING, SYN_AUDIO_DROPPED, SYN_AUDIO_COOLDOWN,
    SYN_AUDIO_CAPACITY, SYN_AUDIO_PAUSED, SYN_AUDIO_CONFLICT, SYN_AUDIO_PRIORITY
};
enum { SYN_AUDIO_NULL, SYN_AUDIO_REAL };
enum {
    SYN_AUDIO_STARTING, SYN_AUDIO_RUNNING_NULL, SYN_AUDIO_RUNNING_REAL,
    SYN_AUDIO_DEGRADED_NULL, SYN_AUDIO_STOPPED, SYN_AUDIO_DISABLED
};

typedef struct {
    uint32_t abi_version, struct_size;
    uint32_t sample_rate, chunk_frames, target_fill_frames, ring_capacity_frames;
    uint32_t max_voices, command_batch_limit, period_frames, periods;
    uint32_t retry_initial_ms, retry_max_ms;
    float bus_gain_ms, instance_gain_ms, pause_out_ms, pause_in_ms, limiter_knee;
} syn_audio_config;

typedef struct {
    uint32_t abi_version, struct_size;
    uint32_t lifecycle, sink, backend, sample_rate, period_frames;
    uint32_t ring_fill, ring_min, ring_max, active_voices, peak_voices;
    uint32_t active_loops, sample_count, instrument_count, sound_count;
    uint64_t rendered_frames, callback_frames, callbacks, underruns;
    uint64_t accepted, dropped, steals, missing_loops;
    uint64_t decoded_frames, decoded_bytes, limited_samples, nonfinite_samples;
    uint64_t transitions, service_ns;
    float mix_peak;
    uint32_t peak_loops;
    char device_name[128], last_error[256];
} syn_audio_status;

enum { SYN_AUDIO_SAMPLE, SYN_AUDIO_SYNTH };
enum { SYN_AUDIO_WORLD, SYN_AUDIO_UI };
enum { SYN_AUDIO_DROP_NEW, SYN_AUDIO_STEAL_OLDEST };
enum { SYN_AUDIO_NO_STEAL, SYN_AUDIO_STEAL_PER_SOUND, SYN_AUDIO_STEAL_GLOBAL };
enum { SYN_AUDIO_SINE, SYN_AUDIO_SAW, SYN_AUDIO_SQUARE,
       SYN_AUDIO_TRIANGLE, SYN_AUDIO_NOISE };
enum { SYN_AUDIO_BYPASS, SYN_AUDIO_LOWPASS, SYN_AUDIO_HIGHPASS, SYN_AUDIO_BANDPASS };
enum {
    SYN_AUDIO_PLAY, SYN_AUDIO_START_LOOP, SYN_AUDIO_UPDATE_LOOP, SYN_AUDIO_STOP_LOOP,
    SYN_AUDIO_REBASE, SYN_AUDIO_VOLUMES, SYN_AUDIO_WORLD_MIX, SYN_AUDIO_PAUSE,
    SYN_AUDIO_RESET, SYN_AUDIO_CLEAR_WORLD, SYN_AUDIO_WRAP_FRAME
};
enum { SYN_AUDIO_HAS_POSITION = 1, SYN_AUDIO_HAS_GAIN = 2 };

typedef struct {
    uint32_t abi_version, struct_size;
    uint64_t max_encoded_bytes, max_frames, max_pcm_bytes;
} syn_audio_decode_limits;

typedef struct {
    uint32_t abi_version, struct_size;
    uint32_t waveform, seed, filter, random_phase;
    float frequency, phase, attack_ms, decay_ms, sustain, release_ms;
    float cutoff, q, gain_db, gate_ms;
} syn_audio_instrument_desc;

typedef struct {
    uint32_t abi_version, struct_size;
    uint32_t source_kind, source_handle, bus, spatial, priority, max_instances;
    uint32_t overflow;
    float cooldown_ms;
    uint32_t loop, freeze, seed, reserved;
    float min_distance, max_distance, vertical_scale, gain_db, gate_ms, stop_fade_ms;
} syn_audio_sound_desc;

/* values: play/start = xyz, gain dB, pitch semitones; update = xyz, gain dB;
   rebase = row-major affine 3x4; volumes = Master/World/UI linear amplitudes;
   world mix = range scale, linear zoom gain; wrap frame = two orthogonal
   period vectors (zero disables an axis). Pause uses flags = 0 or 1. */
typedef struct {
    uint32_t abi_version, struct_size, kind, sound_handle;
    uint64_t loop_key;
    float values[24];
    uint32_t flags, reserved;
} syn_audio_command;

typedef struct {
    uint32_t abi_version, struct_size, result, steal_reason;
    uint64_t evicted_loop_key;
} syn_audio_command_result;

/* A configuration is validated before any backend or allocation is touched. */
void syn_audio_default_config(syn_audio_config*);
syn_audio_result syn_audio_create(const syn_audio_config*, uint32_t sink,
                                  syn_audio_core**);
syn_audio_result syn_audio_start(syn_audio_core*);
void syn_audio_stop(syn_audio_core*);
void syn_audio_destroy(syn_audio_core*);
syn_audio_result syn_audio_load_sample(syn_audio_core*, const char* utf8_path,
    const syn_audio_decode_limits*, uint32_t* handle);
syn_audio_result syn_audio_add_instrument(syn_audio_core*,
    const syn_audio_instrument_desc*, uint32_t* handle);
syn_audio_result syn_audio_add_sound(syn_audio_core*, const syn_audio_sound_desc*,
    uint32_t* handle);
syn_audio_result syn_audio_service(syn_audio_core*, const syn_audio_command*,
    uint32_t count, syn_audio_command_result*, syn_audio_status*);
/* Test/export path: identical command handling without pre-rendering the ring. */
syn_audio_result syn_audio_submit_offline(syn_audio_core*, const syn_audio_command*,
    uint32_t count, syn_audio_command_result*);
syn_audio_result syn_audio_render_offline(syn_audio_core*, float* stereo,
                                         uint32_t frames, syn_audio_status*);
void syn_audio_get_status(syn_audio_core*, syn_audio_status*);
const char* syn_audio_last_error(const syn_audio_core*);

#endif
