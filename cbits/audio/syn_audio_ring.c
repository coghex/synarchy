#include "syn_audio_internal.h"
#include <string.h>

/* One worker writes; one device callback reads. Never reset a running ring. */
uint32_t syn_audio_ring_write(syn_audio_core* core, const float* input,
                              uint32_t frames)
{
    uint32_t done = 0;
    while (done < frames) {
        ma_uint32 count = frames - done;
        void* destination = NULL;
        if (ma_pcm_rb_acquire_write(&core->ring, &count, &destination) != MA_SUCCESS
            || count == 0) break;
        memcpy(destination, input + (size_t)done * 2, (size_t)count * 2 * sizeof(float));
        ma_pcm_rb_commit_write(&core->ring, count);
        done += count;
    }
    return done;
}

/* This is the entire callback-side audio operation: copy, zero-fill, atomics. */
void syn_audio_ring_read(syn_audio_core* core, float* output, uint32_t frames)
{
    uint32_t done = 0;
    while (done < frames) {
        ma_uint32 count = frames - done;
        void* source = NULL;
        if (ma_pcm_rb_acquire_read(&core->ring, &count, &source) != MA_SUCCESS
            || count == 0) break;
        memcpy(output + (size_t)done * 2, source, (size_t)count * 2 * sizeof(float));
        ma_pcm_rb_commit_read(&core->ring, count);
        done += count;
    }
    if (done < frames) {
        memset(output + (size_t)done * 2, 0, (size_t)(frames - done) * 2 * sizeof(float));
        atomic_fetch_add_explicit(&core->underruns, 1, memory_order_relaxed);
    }
    atomic_fetch_add_explicit(&core->callback_frames, frames, memory_order_relaxed);
    atomic_fetch_add_explicit(&core->callbacks, 1, memory_order_relaxed);
}
