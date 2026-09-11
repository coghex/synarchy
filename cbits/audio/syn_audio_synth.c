#include "syn_audio_internal.h"
#include <math.h>
#include <string.h>

#define SYN_PI 3.14159265358979323846

float syn_audio_db(float db) { return powf(10, db / 20); }

uint64_t syn_audio_frames(const syn_audio_core* core, float ms)
{
    return (uint64_t)llround((double)ms * core->config.sample_rate / 1000);
}

void syn_audio_ramp_set(syn_audio_ramp* ramp, float target, float ms, uint32_t rate)
{
    if (target == ramp->target) return;
    ramp->target = target;
    ramp->left = (uint32_t)llround((double)ms * rate / 1000);
    if (!ramp->left) { ramp->current = target; ramp->step = 0; }
    else ramp->step = -expm1f(-6.9077553f / ramp->left);
}

float syn_audio_ramp_next(syn_audio_ramp* ramp)
{
    if (ramp->left) {
        ramp->current += (ramp->target - ramp->current) * ramp->step;
        if (!--ramp->left) ramp->current = ramp->target;
    }
    return ramp->current;
}

static float denormal(float value) { return fabsf(value) < 1e-20f ? 0 : value; }

static void filter_init(syn_audio_voice* voice, const syn_audio_instrument_desc* desc,
                        uint32_t rate)
{
    if (desc->filter == SYN_AUDIO_BYPASS) { voice->b0 = 1; return; }
    double w = 2 * SYN_PI * desc->cutoff / rate;
    double cosine = cos(w), alpha = sin(w) / (2 * desc->q), a0 = 1 + alpha;
    if (desc->filter == SYN_AUDIO_LOWPASS) {
        voice->b0 = voice->b2 = (float)((1 - cosine) / (2 * a0));
        voice->b1 = (float)((1 - cosine) / a0);
    } else if (desc->filter == SYN_AUDIO_HIGHPASS) {
        voice->b0 = voice->b2 = (float)((1 + cosine) / (2 * a0));
        voice->b1 = (float)(-(1 + cosine) / a0);
    } else {
        voice->b0 = (float)(alpha / a0);
        voice->b1 = 0; voice->b2 = -voice->b0;
    }
    voice->a1 = (float)(-2 * cosine / a0);
    voice->a2 = (float)((1 - alpha) / a0);
}

void syn_audio_voice_begin(syn_audio_core* core, syn_audio_voice* voice,
                           uint32_t sound_handle, const syn_audio_command* command)
{
    memset(voice, 0, sizeof(*voice));
    const syn_audio_sound_desc* sound = &core->sounds[sound_handle - 1].desc;
    voice->active = 1; voice->sound = sound_handle;
    voice->looping = command->kind == SYN_AUDIO_START_LOOP;
    voice->loop_key = voice->looping ? command->loop_key : 0;
    voice->start_frame = core->status.rendered_frames;
    voice->pitch_ratio = exp2f(command->values[4] / 12);
    voice->sound_gain = syn_audio_db(sound->gain_db);
    memcpy(voice->xyz, command->values, sizeof(voice->xyz));
    voice->gain.current = voice->gain.target = syn_audio_db(command->values[3]);
    voice->stop_gain.current = voice->stop_gain.target = 1;
    voice->pause_gain.current = voice->pause_gain.target = 1;
    uint64_t sequence = ++core->voice_sequence;
    if (sound->source_kind == SYN_AUDIO_SYNTH) {
        const syn_audio_instrument_desc* desc = &core->instruments[sound->source_handle - 1];
        voice->phase = desc->phase == 1 ? 0 : desc->phase;
        voice->phase_step = fmin(0.45, (double)desc->frequency * voice->pitch_ratio / core->config.sample_rate);
        voice->synth_gain = syn_audio_db(desc->gain_db);
        voice->attack = syn_audio_frames(core, desc->attack_ms);
        voice->decay = syn_audio_frames(core, desc->decay_ms);
        voice->gate = syn_audio_frames(core, sound->gate_ms);
        voice->release = syn_audio_frames(core, desc->release_ms);
        /* FNV-style mixing of authored seed, stable SoundId hash, and voice
           sequence. Scheduling and gameplay random draws never seed the DSP. */
        uint32_t seed = (desc->seed ^ sound->seed) * 16777619u;
        seed = (seed ^ (uint32_t)sequence) * 16777619u;
        seed = (seed ^ (uint32_t)(sequence >> 32)) * 16777619u;
        voice->noise = seed ? seed : 0x6d2b79f5u;
        if (desc->random_phase) voice->phase = (double)voice->noise / 4294967296.0;
        voice->triangle = (float)(voice->phase < 0.5 ? 4 * voice->phase - 1 : 3 - 4 * voice->phase);
        filter_init(voice, desc, core->config.sample_rate);
    }
    core->status.active_voices++;
    if (voice->loop_key) core->status.active_loops++;
    if (core->status.active_loops > core->status.peak_loops)
        core->status.peak_loops = core->status.active_loops;
    if (core->status.active_voices > core->status.peak_voices)
        core->status.peak_voices = core->status.active_voices;
}

void syn_audio_voice_retire(syn_audio_core* core, syn_audio_voice* voice)
{
    if (!voice->active) return;
    core->status.active_voices--;
    if (voice->loop_key) core->status.active_loops--;
    voice->active = 0; voice->loop_key = 0;
}

/* Envelope state holds the preceding sample's level. A gate interrupting an
   attack/decay releases from that exact level over exactly R output samples. */
float syn_audio_envelope(syn_audio_voice* voice, const syn_audio_instrument_desc* desc,
    uint64_t attack, uint64_t decay, uint64_t gate, uint64_t release)
{
    uint64_t age = voice->age;
    if (!voice->looping && age >= gate) {
        if (age == gate) voice->release_level = voice->envelope;
        uint64_t elapsed = age - gate;
        voice->envelope = !release || elapsed >= release ? 0
            : voice->release_level * (float)(release - elapsed - 1) / (float)release;
    } else if (age < attack) voice->envelope = (float)(age + 1) / (float)attack;
    else if (age - attack < decay)
        voice->envelope = 1 + (desc->sustain - 1) * (float)(age - attack + 1) / (float)decay;
    else voice->envelope = desc->sustain;
    return voice->envelope;
}

static double polyblep(double phase, double delta)
{
    if (phase < delta) {
        double t = phase / delta;
        return t + t - t * t - 1;
    }
    if (phase > 1 - delta) {
        double t = (phase - 1) / delta;
        return t * t + t + t + 1;
    }
    return 0;
}

static float oscillator(syn_audio_voice* voice, uint32_t waveform, double delta)
{
    double phase = voice->phase, value;
    if (waveform == SYN_AUDIO_SINE) value = sin(2 * SYN_PI * phase);
    else if (waveform == SYN_AUDIO_SAW) value = 2 * phase - 1 - polyblep(phase, delta);
    else if (waveform == SYN_AUDIO_NOISE) {
        uint32_t x = voice->noise;
        x ^= x << 13; x ^= x >> 17; x ^= x << 5;
        voice->noise = x;
        value = (double)x / 2147483648.0 - 1;
    } else {
        value = (phase < 0.5 ? 1 : -1) + polyblep(phase, delta)
            - polyblep(fmod(phase + 0.5, 1), delta);
        if (waveform == SYN_AUDIO_TRIANGLE) {
            voice->triangle = denormal(fmaxf(-1, fminf(1,
                (float)(voice->triangle * 0.99999 + value * 4 * delta))));
            value = voice->triangle;
        }
    }
    voice->phase += delta;
    voice->phase -= floor(voice->phase);
    return (float)value;
}

void syn_audio_synth(syn_audio_core* core, syn_audio_voice* voice, float* stereo)
{
    const syn_audio_sound_desc* sound = &core->sounds[voice->sound - 1].desc;
    const syn_audio_instrument_desc* desc = &core->instruments[sound->source_handle - 1];
    float envelope = syn_audio_envelope(voice, desc,
        voice->attack, voice->decay, voice->gate, voice->release);
    /* Pitch cannot push a band-limited oscillator beyond its stable range. */
    float input = oscillator(voice, desc->waveform, voice->phase_step) * envelope;
    for (uint32_t ch = 0; ch < 2; ++ch) {
        float output = voice->b0 * input + voice->z1[ch];
        voice->z1[ch] = denormal(voice->b1 * input - voice->a1 * output + voice->z2[ch]);
        voice->z2[ch] = denormal(voice->b2 * input - voice->a2 * output);
        stereo[ch] = output * voice->synth_gain;
    }
}
