#include "audio_test_helpers.h"
#include <stdlib.h>

#define TEST_PI 3.14159265358979323846

static double component(const float* output, uint32_t frames, double frequency)
{
    double sine = 0, cosine = 0;
    for (uint32_t i = 0; i < frames; ++i) {
        double phase = 2 * TEST_PI * frequency * i / 48000;
        sine += output[i * 2] * sin(phase); cosine += output[i * 2] * cos(phase);
    }
    return hypot(sine, cosine) * 2 / frames;
}

static void generators_and_pitch(void)
{
    float output[9600];
    for (uint32_t waveform = SYN_AUDIO_SINE; waveform <= SYN_AUDIO_NOISE; ++waveform) {
        syn_audio_core* core = test_core();
        syn_audio_instrument_desc instrument = test_instrument();
        instrument.waveform = waveform; instrument.frequency = 1000;
        uint32_t sound = test_add_synth(core, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
        syn_audio_command command = test_command(SYN_AUDIO_PLAY, sound, 0);
        command.values[4] = 12;
        assert(test_submit(core, command).result == SYN_AUDIO_OK);
        test_render(core, output, 4800);
        for (uint32_t i = 0; i < 9600; ++i) assert(isfinite(output[i]) && fabsf(output[i]) <= 0.13f);
        assert(test_energy(output, 4800, 0) > 0.001);
        assert(memcmp(output, output + 2, sizeof(float)) != 0 || waveform == SYN_AUDIO_SQUARE);
        if (waveform != SYN_AUDIO_NOISE) {
            assert(component(output, 4800, 2000) > 0.05);
            assert(component(output, 4800, 1000) < 0.005);
        }
        for (uint32_t i = 0; i < 4800; ++i) assert(output[i * 2] == output[i * 2 + 1]);
        syn_audio_destroy(core);
    }
    /* Near-Nyquist square: the folded third harmonic is strongly suppressed. */
    syn_audio_core* core = test_core();
    syn_audio_instrument_desc instrument = test_instrument();
    instrument.waveform = SYN_AUDIO_SQUARE; instrument.frequency = 18000;
    uint32_t sound = test_add_synth(core, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    assert(test_submit(core, test_command(SYN_AUDIO_PLAY, sound, 0)).result == SYN_AUDIO_OK);
    test_render(core, output, 4800);
    assert(component(output, 4800, 6000) < component(output, 4800, 18000) * 0.15);
    assert(fabsf(output[0]) < 1e-7f); /* Corrected discontinuity, no naive square jump. */
    syn_audio_destroy(core);
}

static void envelope_partial_release(void)
{
    syn_audio_instrument_desc instrument = test_instrument();
    instrument.sustain = 0.25f;
    syn_audio_voice voice = {0};
    for (uint64_t i = 0; i < 4; ++i) {
        voice.age = i;
        assert(fabsf(syn_audio_envelope(&voice, &instrument, 8, 4, 4, 4) - (i + 1) / 8.0f) < 1e-7f);
    }
    const float release[] = {0.375f, 0.25f, 0.125f, 0};
    for (uint64_t i = 0; i < 4; ++i) {
        voice.age = 4 + i;
        assert(syn_audio_envelope(&voice, &instrument, 8, 4, 4, 4) == release[i]);
    }
    voice = (syn_audio_voice){.looping = 1};
    for (uint64_t i = 0; i < 100; ++i) {
        voice.age = i;
        float level = syn_audio_envelope(&voice, &instrument, 4, 4, 1, 2);
        assert(level >= 0.25f && level <= 1);
        if (i >= 7) assert(level == 0.25f); /* Loop ignores its one-shot gate. */
    }
    syn_audio_core* core = test_core();
    instrument.attack_ms = 10; instrument.release_ms = 5;
    syn_audio_sound_desc desc = test_sound(SYN_AUDIO_SYNTH, 0); desc.gate_ms = 5;
    uint32_t sound = test_add_synth(core, instrument, desc);
    assert(test_submit(core, test_command(SYN_AUDIO_PLAY, sound, 0)).result == SYN_AUDIO_OK);
    float output[1000];
    test_render(core, output, 479);
    assert(core->status.active_voices == 1);
    test_render(core, output, 1);
    assert(core->status.active_voices == 0);
    test_render(core, output, 500);
    for (uint32_t i = 0; i < 1000; ++i) assert(output[i] == 0);
    syn_audio_destroy(core);
}

static double filter_energy(uint32_t filter, float frequency)
{
    syn_audio_core* core = test_core();
    syn_audio_instrument_desc instrument = test_instrument();
    instrument.filter = filter; instrument.frequency = frequency;
    uint32_t sound = test_add_synth(core, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    assert(test_submit(core, test_command(SYN_AUDIO_PLAY, sound, 0)).result == SYN_AUDIO_OK);
    float output[9600];
    test_render(core, output, 4800); /* Settle the filter. */
    test_render(core, output, 4800);
    double energy = test_energy(output, 4800, 0);
    syn_audio_destroy(core);
    return energy;
}

static void filter_response(void)
{
    double reference = filter_energy(SYN_AUDIO_BYPASS, 1000);
    assert(filter_energy(SYN_AUDIO_LOWPASS, 100) > reference * 0.9);
    assert(filter_energy(SYN_AUDIO_LOWPASS, 10000) < reference * 0.001);
    assert(filter_energy(SYN_AUDIO_HIGHPASS, 100) < reference * 0.001);
    assert(filter_energy(SYN_AUDIO_HIGHPASS, 10000) > reference * 0.9);
    assert(filter_energy(SYN_AUDIO_BANDPASS, 1000) > reference * 0.99);
    assert(filter_energy(SYN_AUDIO_BANDPASS, 100) < reference * 0.03);
    assert(filter_energy(SYN_AUDIO_BANDPASS, 10000) < reference * 0.03);
}

static void noise_and_pause(void)
{
    syn_audio_core *a = test_core(), *b = test_core();
    syn_audio_instrument_desc instrument = test_instrument();
    instrument.waveform = SYN_AUDIO_NOISE; instrument.filter = SYN_AUDIO_BANDPASS;
    instrument.seed = 0; instrument.attack_ms = 100; instrument.decay_ms = 100;
    instrument.release_ms = 100; instrument.sustain = 0.5f;
    uint32_t sa = test_add_synth(a, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    uint32_t sb = test_add_synth(b, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    assert(test_submit(a, test_command(SYN_AUDIO_START_LOOP, sa, 1)).result == SYN_AUDIO_OK);
    assert(test_submit(b, test_command(SYN_AUDIO_START_LOOP, sb, 1)).result == SYN_AUDIO_OK);
    float oa[9600], ob[9600];
    test_render(a, oa, 1000); test_render(b, ob, 1000);
    assert(memcmp(oa, ob, 2000 * sizeof(float)) == 0);
    syn_audio_voice saved = a->voices[0];
    syn_audio_command pause = test_command(SYN_AUDIO_PAUSE, 0, 0); pause.flags = 1;
    assert(test_submit(a, pause).result == SYN_AUDIO_OK);
    assert(test_submit(a, test_command(SYN_AUDIO_PLAY, sa, 0)).result == SYN_AUDIO_PAUSED);
    test_render(a, oa, 4800);
    assert(a->voices[0].age == saved.age && a->voices[0].phase == saved.phase);
    assert(a->voices[0].noise == saved.noise && a->voices[0].envelope == saved.envelope);
    assert(memcmp(a->voices[0].z1, saved.z1, sizeof(saved.z1)) == 0);
    assert(memcmp(a->voices[0].z2, saved.z2, sizeof(saved.z2)) == 0);
    for (uint32_t i = 240 * 2; i < 9600; ++i) assert(oa[i] == 0);
    pause.flags = 0;
    assert(test_submit(a, pause).result == SYN_AUDIO_OK);
    test_render(a, oa, 1000); test_render(b, ob, 1000);
    assert(memcmp(oa + 480 * 2, ob + 480 * 2, (1000 - 480) * 2 * sizeof(float)) == 0);
    assert(test_submit(a, test_command(SYN_AUDIO_START_LOOP, sa, 2)).result == SYN_AUDIO_OK);
    assert(a->voices[0].noise != a->voices[1].noise);
    syn_audio_destroy(a); syn_audio_destroy(b);
}

static void protection_and_gain(void)
{
    syn_audio_core* core = test_core();
    syn_audio_instrument_desc instrument = test_instrument();
    instrument.gain_db = 24; instrument.phase = 0.25f;
    syn_audio_sound_desc desc = test_sound(SYN_AUDIO_SYNTH, 0); desc.gain_db = 24;
    uint32_t sound = test_add_synth(core, instrument, desc);
    for (uint32_t i = 0; i < 128; ++i) {
        syn_audio_command play = test_command(SYN_AUDIO_START_LOOP, sound, i + 1);
        play.values[3] = 12;
        assert(test_submit(core, play).result == SYN_AUDIO_OK);
    }
    float output[4096];
    test_render(core, output, 1024);
    assert(core->status.limited_samples > 0 && core->status.mix_peak > 1000);
    for (uint32_t i = 0; i < 2048; ++i) assert(isfinite(output[i]) && fabsf(output[i]) <= 1);
    /* Make volume changes use the running-policy smoothing without speakers. */
    core->catalog_sealed = 1;
    syn_audio_command volume = test_command(SYN_AUDIO_VOLUMES, 0, 0);
    volume.values[1] = volume.values[2] = 1;
    assert(test_submit(core, volume).result == SYN_AUDIO_OK);
    test_render(core, output, 1024);
    for (uint32_t i = 960 * 2; i < 2048; ++i) assert(output[i] == 0);
    assert(core->voices[0].age == 2048); /* Master mute does not freeze clocks. */
    syn_audio_destroy(core);
}

static void zero_stages_phase_and_denormals(void)
{
    syn_audio_instrument_desc instrument = test_instrument();
    instrument.sustain = 0.4f;
    syn_audio_voice envelope = {0};
    assert(syn_audio_envelope(&envelope, &instrument, 0, 0, 1, 0) == 0.4f);
    envelope.age = 1;
    assert(syn_audio_envelope(&envelope, &instrument, 0, 0, 1, 0) == 0);

    syn_audio_core *a = test_core(), *b = test_core();
    instrument.random_phase = 1;
    uint32_t sa = test_add_synth(a, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    uint32_t sb = test_add_synth(b, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    for (uint32_t i = 0; i < 2; ++i) {
        assert(test_submit(a, test_command(SYN_AUDIO_START_LOOP, sa, i + 1)).result == SYN_AUDIO_OK);
        assert(test_submit(b, test_command(SYN_AUDIO_START_LOOP, sb, i + 1)).result == SYN_AUDIO_OK);
        assert(a->voices[i].phase == b->voices[i].phase);
        assert(a->voices[i].phase >= 0 && a->voices[i].phase < 1);
    }
    assert(a->voices[0].phase != a->voices[1].phase);
    syn_audio_destroy(a); syn_audio_destroy(b);

    a = test_core();
    instrument = test_instrument();
    instrument.filter = SYN_AUDIO_LOWPASS; instrument.sustain = 0;
    sa = test_add_synth(a, instrument, test_sound(SYN_AUDIO_SYNTH, 0));
    assert(test_submit(a, test_command(SYN_AUDIO_START_LOOP, sa, 1)).result == SYN_AUDIO_OK);
    for (uint32_t ch = 0; ch < 2; ++ch) {
        a->voices[0].z1[ch] = 1e-35f; a->voices[0].z2[ch] = -1e-35f;
    }
    float output[16];
    test_render(a, output, 8);
    for (uint32_t ch = 0; ch < 2; ++ch) {
        assert(a->voices[0].z1[ch] == 0 && a->voices[0].z2[ch] == 0);
    }
    for (uint32_t i = 2; i < 16; ++i) assert(output[i] == 0);
    syn_audio_destroy(a);
}

void audio_dsp_tests(void)
{
    generators_and_pitch(); envelope_partial_release(); filter_response();
    noise_and_pause(); protection_and_gain();
    zero_stages_phase_and_denormals();
    puts("PASS: native generators/pitch/band limiting, ADSR, filters, noise, exact pause, bounded mix/mute");
}
