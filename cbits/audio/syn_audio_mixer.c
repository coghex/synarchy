#include "syn_audio_internal.h"
#include <math.h>
#include <string.h>

static void spatial(const syn_audio_core* core, const syn_audio_voice* voice,
                     const syn_audio_sound_desc* sound, float* left, float* right)
{
    *left = *right = voice->sound_gain;
    if (!sound->spatial) return;
    double x = voice->xyz[0], y = voice->xyz[1], z = voice->xyz[2] * sound->vertical_scale;
    double horizontal = hypot(x, y), distance = hypot(horizontal, z);
    double maximum = sound->max_distance * core->range_scale;
    double gain = distance <= sound->min_distance ? 1 : distance >= maximum ? 0
        : 1 - (distance - sound->min_distance) / (maximum - sound->min_distance);
    double pan = horizontal < 1e-9 ? 0 : fmax(-1, fmin(1, x / horizontal));
    *left *= (float)(gain * sqrt((1 - pan) / 2));
    *right *= (float)(gain * sqrt((1 + pan) / 2));
}

static void sample_render(const syn_audio_sample* sample, syn_audio_voice* voice,
                           float* stereo)
{
    uint64_t index = (uint64_t)voice->cursor;
    uint64_t next = index + 1;
    if (next >= sample->frames) next = voice->looping ? 0 : index;
    float blend = (float)(voice->cursor - index);
    for (uint32_t ch = 0; ch < 2; ++ch) {
        float a = sample->pcm[index * 2 + ch], b = sample->pcm[next * 2 + ch];
        stereo[ch] = a + (b - a) * blend;
    }
    voice->cursor += voice->pitch_ratio;
    if (voice->looping && voice->cursor >= sample->frames)
        voice->cursor = fmod(voice->cursor, (double)sample->frames);
}

static void render_voice(syn_audio_core* core, syn_audio_voice* voice, uint32_t frames)
{
    const syn_audio_sound_desc* sound = &core->sounds[voice->sound - 1].desc;
    float* bus = sound->bus == SYN_AUDIO_WORLD ? core->world : core->ui;
    float left, right;
    spatial(core, voice, sound, &left, &right);
    for (uint32_t i = 0; i < frames && voice->active; ++i) {
        float output[2];
        int frozen = core->player_paused && sound->freeze;
        if (frozen) {
            /* Only the remembered output ramp changes. Cursor, envelope, noise,
               oscillator, filter, and logical voice age remain bit-identical. */
            output[0] = voice->last_output[0]; output[1] = voice->last_output[1];
        } else {
            if (sound->source_kind == SYN_AUDIO_SAMPLE) {
                const syn_audio_sample* sample = &core->samples[sound->source_handle - 1];
                if (voice->cursor >= sample->frames) { syn_audio_voice_retire(core, voice); break; }
                sample_render(sample, voice, output);
            } else {
                if (!voice->looping && voice->age >= voice->gate + voice->release) {
                    syn_audio_voice_retire(core, voice); break;
                }
                syn_audio_synth(core, voice, output);
            }
            float gain = syn_audio_ramp_next(&voice->gain);
            output[0] *= gain * left; output[1] *= gain * right;
            voice->last_output[0] = output[0]; voice->last_output[1] = output[1];
            voice->age++;
        }
        float gain = syn_audio_ramp_next(&voice->pause_gain) * syn_audio_ramp_next(&voice->stop_gain);
        bus[i * 2] += output[0] * gain;
        bus[i * 2 + 1] += output[1] * gain;
        if (voice->stopping && !voice->stop_gain.left) syn_audio_voice_retire(core, voice);
        if (!frozen && !voice->looping && voice->active) {
            if (sound->source_kind == SYN_AUDIO_SAMPLE
                ? voice->cursor >= core->samples[sound->source_handle - 1].frames
                : voice->age >= voice->gate + voice->release) syn_audio_voice_retire(core, voice);
        }
    }
}

static float protect(syn_audio_core* core, float value)
{
    if (!isfinite(value)) { core->status.nonfinite_samples++; return 0; }
    float level = fabsf(value), knee = core->config.limiter_knee;
    if (level > core->status.mix_peak) core->status.mix_peak = level;
    if (level <= knee) return value;
    core->status.limited_samples++;
    float above = level - knee;
    return copysignf(knee + (1 - knee) * above / (above + 1 - knee), value);
}

void syn_audio_mix(syn_audio_core* core, float* output, uint32_t frames)
{
    /* Offline exports can be larger than a chunk. Storage stays fixed and every
       boundary uses the same renderer as the live ring producer. */
    while (frames) {
        uint32_t count = frames < core->config.chunk_frames ? frames : core->config.chunk_frames;
        memset(core->world, 0, (size_t)count * 2 * sizeof(float));
        memset(core->ui, 0, (size_t)count * 2 * sizeof(float));
        for (uint32_t slot = 0; slot < core->config.max_voices; ++slot)
            if (core->voices[slot].active) render_voice(core, &core->voices[slot], count);
        for (uint32_t i = 0; i < count; ++i) {
            float master = syn_audio_ramp_next(&core->master_gain);
            float world = syn_audio_ramp_next(&core->world_gain) * syn_audio_ramp_next(&core->zoom_gain);
            float ui = syn_audio_ramp_next(&core->ui_gain);
            for (uint32_t ch = 0; ch < 2; ++ch) {
                uint32_t index = i * 2 + ch;
                output[index] = protect(core, (core->world[index] * world + core->ui[index] * ui) * master);
            }
        }
        frames -= count; output += count * 2;
        core->status.rendered_frames += count;
    }
}
