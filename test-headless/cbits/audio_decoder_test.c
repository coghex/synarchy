#include "audio_test_helpers.h"

static syn_audio_decode_limits budgets(void)
{
    return (syn_audio_decode_limits){SYN_AUDIO_ABI, sizeof(syn_audio_decode_limits),
        16 * 1024 * 1024, 15 * 48000, 64 * 1024 * 1024};
}

void audio_decoder_tests(void)
{
    syn_audio_core* core = test_core();
    syn_audio_decode_limits limits = budgets();
    uint32_t wav, flac, mp3, duplicate, stereo;
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.wav", &limits, &wav) == SYN_AUDIO_OK);
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.flac", &limits, &flac) == SYN_AUDIO_OK);
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.mp3", &limits, &mp3) == SYN_AUDIO_OK);
    assert(syn_audio_load_sample(core, "test-headless/data/audio/stereo.wav", &limits, &stereo) == SYN_AUDIO_OK);
    const syn_audio_sample *w = &core->samples[wav - 1], *f = &core->samples[flac - 1],
        *m = &core->samples[mp3 - 1], *s = &core->samples[stereo - 1];
    assert(w->frames >= 11999 && w->frames <= 12001 && f->frames == w->frames);
    assert(s->frames == 480);
    for (uint64_t i = 0; i < w->frames; ++i) {
        assert(w->pcm[i * 2] == w->pcm[i * 2 + 1]);
        assert(fabsf(w->pcm[i * 2] - f->pcm[i * 2]) < 1e-6f);
    }
    for (uint32_t i = 0; i < 480; ++i) {
        assert(fabsf(s->pcm[i * 2] - i * 10.0f / 32768) < 1e-6f);
        assert(s->pcm[i * 2 + 1] == -s->pcm[i * 2]);
    }
    /* MP3 may retain encoder delay/padding. Judge signal duration and energy,
       with a frequency oracle insensitive to that phase/delay. */
    assert(m->frames >= 11900 && m->frames <= 16000);
    assert(test_energy(m->pcm, (uint32_t)m->frames, 0) > 0.01);
    assert(test_energy(m->pcm, (uint32_t)m->frames, 0) < 0.025);
    uint32_t crossings = 0;
    for (uint64_t i = 1; i < m->frames; ++i)
        if (m->pcm[i * 2 - 2] < -0.0001f && m->pcm[i * 2] >= 0) crossings++;
    assert(crossings >= 100 && crossings <= 125);
    uint64_t decoded = core->status.decoded_bytes;
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.wav", &limits, &duplicate) == SYN_AUDIO_OK);
    assert(duplicate == wav && core->status.sample_count == 4 && core->status.decoded_bytes == decoded);
    syn_audio_sound_desc desc = test_sound(SYN_AUDIO_SAMPLE, mp3);
    uint32_t sound = 0;
    assert(syn_audio_add_sound(core, &desc, &sound) == SYN_AUDIO_INVALID);
    desc.loop = 0;
    assert(syn_audio_add_sound(core, &desc, &sound) == SYN_AUDIO_OK);
    desc = test_sound(SYN_AUDIO_SAMPLE, stereo);
    assert(syn_audio_add_sound(core, &desc, &sound) == SYN_AUDIO_OK);
    syn_audio_command play = test_command(SYN_AUDIO_PLAY, sound, 0);
    play.values[4] = 12;
    assert(test_submit(core, play).result == SYN_AUDIO_OK);
    float output[2000];
    test_render(core, output, 240);
    assert(core->status.active_voices == 0);
    for (uint32_t i = 0; i < 240; ++i) assert(output[i * 2] == s->pcm[i * 4]);
    assert(test_submit(core, test_command(SYN_AUDIO_START_LOOP, sound, 1)).result == SYN_AUDIO_OK);
    test_render(core, output, 1000);
    for (uint32_t i = 0; i < 1000; ++i) assert(output[i * 2] == s->pcm[(i % 480) * 2]);
    syn_audio_command pause = test_command(SYN_AUDIO_PAUSE, 0, 0); pause.flags = 1;
    assert(test_submit(core, pause).result == SYN_AUDIO_OK);
    double cursor = core->voices[0].cursor;
    test_render(core, output, 1000);
    assert(core->voices[0].cursor == cursor);
    syn_audio_destroy(core);

    core = test_core();
    limits = budgets(); limits.max_frames = 11999;
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.wav", &limits, &wav) == SYN_AUDIO_LIMIT);
    assert(core->status.sample_count == 0 && core->status.decoded_bytes == 0);
    limits = budgets(); limits.max_pcm_bytes = 100;
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.flac", &limits, &flac) == SYN_AUDIO_LIMIT);
    limits = budgets(); limits.max_encoded_bytes = 10;
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.mp3", &limits, &mp3) == SYN_AUDIO_LIMIT);
    limits = budgets();
    assert(syn_audio_load_sample(core, "test-headless/data/audio/missing.wav", &limits, &wav) == SYN_AUDIO_MISSING);
    assert(syn_audio_load_sample(core, "test-headless/data/audio/corrupt.wav", &limits, &wav) == SYN_AUDIO_DECODE_ERROR);
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.wav", &limits, &wav) == SYN_AUDIO_OK);
    assert(core->status.sample_count == 1); /* Failures do not poison later loading. */
    assert(syn_audio_start(core) == SYN_AUDIO_OK);
    syn_audio_stop(core);
    assert(syn_audio_load_sample(core, "test-headless/data/audio/tone.flac", &limits, &flac) == SYN_AUDIO_BAD_STATE);
    syn_audio_destroy(core);
    puts("PASS: WAV/FLAC/MP3, mono/rate conversion, stereo, dedup, bounded decode, pitch/loop/pause, startup-only catalog");
}
