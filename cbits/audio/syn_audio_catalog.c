#include "syn_audio_internal.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>

static int between(float n, float low, float high)
{
    return isfinite(n) && n >= low && n <= high;
}

/* All catalog mutations are initialization-only, even after a device stop. */
syn_audio_result syn_audio_add_instrument(syn_audio_core* core,
    const syn_audio_instrument_desc* desc, uint32_t* handle)
{
    if (!core || !desc || !handle) return SYN_AUDIO_INVALID;
    *handle = 0;
    if (core->catalog_sealed) return SYN_AUDIO_BAD_STATE;
    float top = fminf(20000, 0.45f * core->config.sample_rate);
    if (desc->abi_version != SYN_AUDIO_ABI || desc->struct_size != sizeof(*desc)
        || desc->waveform > SYN_AUDIO_NOISE || desc->filter > SYN_AUDIO_BANDPASS || desc->random_phase > 1
        || !between(desc->frequency, 20, top) || !between(desc->phase, 0, 1)
        || !between(desc->attack_ms, 0, 30000) || !between(desc->decay_ms, 0, 30000)
        || !between(desc->sustain, 0, 1) || !between(desc->release_ms, 0, 30000)
        || !between(desc->cutoff, 20, 0.45f * core->config.sample_rate)
        || !between(desc->q, 0.1f, 20) || !between(desc->gain_db, -96, 24)
        || !between(desc->gate_ms, 0, 60000)) return SYN_AUDIO_INVALID;
    uint32_t n = core->status.instrument_count;
    if (n >= 65536) return SYN_AUDIO_LIMIT;
    void* next = realloc(core->instruments, (size_t)(n + 1) * sizeof(*desc));
    if (!next) return SYN_AUDIO_NO_MEMORY;
    core->instruments = next;
    core->instruments[n] = *desc;
    *handle = ++core->status.instrument_count; /* Zero is never a valid handle. */
    return SYN_AUDIO_OK;
}

syn_audio_result syn_audio_add_sound(syn_audio_core* core,
    const syn_audio_sound_desc* desc, uint32_t* handle)
{
    if (!core || !desc || !handle) return SYN_AUDIO_INVALID;
    *handle = 0;
    if (core->catalog_sealed) return SYN_AUDIO_BAD_STATE;
    if (desc->abi_version != SYN_AUDIO_ABI || desc->struct_size != sizeof(*desc)
        || desc->source_kind > SYN_AUDIO_SYNTH || !desc->source_handle
        || desc->source_handle > (desc->source_kind == SYN_AUDIO_SAMPLE
            ? core->status.sample_count : core->status.instrument_count)
        || desc->bus > SYN_AUDIO_UI || desc->spatial > 1 || desc->loop > 1
        || desc->freeze > 1 || desc->priority > 100 || !desc->max_instances
        || desc->max_instances > core->config.max_voices
        || desc->overflow > SYN_AUDIO_STEAL_OLDEST || !between(desc->cooldown_ms, 0, 60000)
        || !between(desc->min_distance, 0, 1024)
        || !between(desc->max_distance, 0, 4096) || desc->max_distance <= desc->min_distance
        || !between(desc->vertical_scale, 0, 8) || !between(desc->gain_db, -96, 24)
        || !between(desc->gate_ms, 0, 60000) || !between(desc->stop_fade_ms, 0, 5000)
        || (desc->bus == SYN_AUDIO_UI && (desc->spatial || desc->freeze))) return SYN_AUDIO_INVALID;
    if (desc->source_kind == SYN_AUDIO_SAMPLE && desc->loop
        && core->samples[desc->source_handle - 1].mp3) return SYN_AUDIO_INVALID;
    uint32_t n = core->status.sound_count;
    if (n >= 65536) return SYN_AUDIO_LIMIT;
    void* next = realloc(core->sounds, (size_t)(n + 1) * sizeof(*core->sounds));
    if (!next) return SYN_AUDIO_NO_MEMORY;
    core->sounds = next;
    core->sounds[n] = (syn_audio_sound){.desc = *desc};
    *handle = ++core->status.sound_count;
    return SYN_AUDIO_OK;
}

syn_audio_result syn_audio_load_sample(syn_audio_core* core, const char* path,
    const syn_audio_decode_limits* limits, uint32_t* handle)
{
    if (!core || !path || !limits || !handle) return SYN_AUDIO_INVALID;
    *handle = 0;
    if (core->catalog_sealed) return SYN_AUDIO_BAD_STATE;
    if (limits->abi_version != SYN_AUDIO_ABI || limits->struct_size != sizeof(*limits)
        || !limits->max_encoded_bytes || !limits->max_frames || !limits->max_pcm_bytes)
        return SYN_AUDIO_INVALID;
    for (uint32_t i = 0; i < core->status.sample_count; ++i) {
        if (!strcmp(path, core->samples[i].path)) { *handle = i + 1; return SYN_AUDIO_OK; }
    }
    const char* extension = strrchr(path, '.');
    if (!extension || (strcasecmp(extension, ".wav") && strcasecmp(extension, ".flac")
        && strcasecmp(extension, ".mp3"))) return SYN_AUDIO_INVALID;
    struct stat info;
    if (stat(path, &info) || !S_ISREG(info.st_mode)) {
        syn_audio_error(core, "Sample is missing or not a regular file");
        return SYN_AUDIO_MISSING;
    }
    if (info.st_size < 0 || (uint64_t)info.st_size > limits->max_encoded_bytes) {
        syn_audio_error(core, "Sample exceeds encoded byte budget"); return SYN_AUDIO_LIMIT;
    }
    uint64_t maximum = limits->max_frames;
    if (maximum > limits->max_pcm_bytes / (2 * sizeof(float)))
        maximum = limits->max_pcm_bytes / (2 * sizeof(float));
    if (maximum > SIZE_MAX / (2 * sizeof(float))) maximum = SIZE_MAX / (2 * sizeof(float));
    if (!maximum || core->status.sample_count >= 65536) return SYN_AUDIO_LIMIT;
    ma_decoder decoder;
    ma_decoder_config config = ma_decoder_config_init(ma_format_f32, 2, core->config.sample_rate);
    ma_result decoded = ma_decoder_init_file(path, &config, &decoder);
    if (decoded != MA_SUCCESS) {
        syn_audio_error(core, ma_result_description(decoded)); return SYN_AUDIO_DECODE_ERROR;
    }
    float scratch[4096 * 2];
    float* pcm = NULL;
    uint64_t used = 0, allocated = 0;
    syn_audio_result result = SYN_AUDIO_OK;
    for (;;) {
        /* Read at most the remaining allowance plus one frame; never trust a
           compressed header's reported length or allocate it speculatively. */
        uint64_t request = maximum - used + 1;
        if (request > 4096) request = 4096;
        ma_uint64 got = 0;
        decoded = ma_decoder_read_pcm_frames(&decoder, scratch, request, &got);
        if (decoded != MA_SUCCESS && decoded != MA_AT_END) {
            result = SYN_AUDIO_DECODE_ERROR; break;
        }
        if (got > maximum - used) { result = SYN_AUDIO_LIMIT; break; }
        if (got) {
            uint64_t required = used + got;
            if (required > allocated) {
                uint64_t capacity = allocated ? allocated * 2 : 4096;
                if (capacity < required) capacity = required;
                if (capacity > maximum) capacity = maximum;
                void* next = realloc(pcm, (size_t)capacity * 2 * sizeof(float));
                if (!next) { result = SYN_AUDIO_NO_MEMORY; break; }
                pcm = next; allocated = capacity;
            }
            memcpy(pcm + used * 2, scratch, (size_t)got * 2 * sizeof(float));
            used += got;
        }
        if (!got || decoded == MA_AT_END) break;
    }
    ma_decoder_uninit(&decoder);
    if (result == SYN_AUDIO_OK && !used) result = SYN_AUDIO_DECODE_ERROR;
    if (result == SYN_AUDIO_OK && allocated != used) {
        void* exact = realloc(pcm, (size_t)used * 2 * sizeof(float));
        if (!exact) result = SYN_AUDIO_NO_MEMORY;
        else pcm = exact;
    }
    char* saved_path = NULL;
    if (result == SYN_AUDIO_OK) {
        saved_path = malloc(strlen(path) + 1);
        if (!saved_path) result = SYN_AUDIO_NO_MEMORY;
        else strcpy(saved_path, path);
    }
    if (result == SYN_AUDIO_OK) {
        size_t size = (size_t)(core->status.sample_count + 1) * sizeof(*core->samples);
        void* next = realloc(core->samples, size);
        if (!next) result = SYN_AUDIO_NO_MEMORY;
        else core->samples = next;
    }
    if (result != SYN_AUDIO_OK) {
        free(pcm); free(saved_path);
        syn_audio_error(core, result == SYN_AUDIO_LIMIT ? "Sample exceeds remaining decoded frame/byte budget"
            : result == SYN_AUDIO_NO_MEMORY ? "Sample allocation failed" : "Sample decode failed or yielded no frames");
        return result;
    }
    core->samples[core->status.sample_count] = (syn_audio_sample){saved_path, pcm, used,
        strcasecmp(extension, ".mp3") == 0};
    *handle = ++core->status.sample_count;
    core->status.decoded_frames += used;
    core->status.decoded_bytes += used * 2 * sizeof(float);
    return SYN_AUDIO_OK;
}

void syn_audio_catalog_free(syn_audio_core* core)
{
    for (uint32_t i = 0; i < core->status.sample_count; ++i) {
        free(core->samples[i].pcm); free(core->samples[i].path);
    }
    free(core->samples); free(core->instruments); free(core->sounds);
}
