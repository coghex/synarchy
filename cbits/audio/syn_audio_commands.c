#include "syn_audio_internal.h"
#include <float.h>
#include <math.h>
#include <string.h>

static int finite_values(const syn_audio_command* command, uint32_t count)
{
    for (uint32_t i = 0; i < count; ++i) if (!isfinite(command->values[i])) return 0;
    return 1;
}

static syn_audio_voice* loop_voice(syn_audio_core* core, uint64_t key)
{
    if (!key) return NULL;
    for (uint32_t i = 0; i < core->config.max_voices; ++i)
        if (core->voices[i].active && core->voices[i].loop_key == key) return &core->voices[i];
    return NULL;
}

static int older(const syn_audio_voice* a, const syn_audio_voice* b)
{
    /* Equal frames retain the first encountered (lowest slot) deterministically. */
    return !b || a->start_frame < b->start_frame;
}

static syn_audio_result update(syn_audio_core* core, syn_audio_voice* voice,
                               const syn_audio_command* command, uint32_t flags)
{
    if ((flags & ~(SYN_AUDIO_HAS_POSITION | SYN_AUDIO_HAS_GAIN))
        || ((flags & SYN_AUDIO_HAS_POSITION) && !finite_values(command, 3))
        || ((flags & SYN_AUDIO_HAS_GAIN) && (!isfinite(command->values[3])
            || command->values[3] < -48 || command->values[3] > 12))) return SYN_AUDIO_INVALID;
    if (!voice) { core->status.missing_loops++; return SYN_AUDIO_MISSING; }
    if (flags & SYN_AUDIO_HAS_POSITION) memcpy(voice->xyz, command->values, sizeof(voice->xyz));
    if (flags & SYN_AUDIO_HAS_GAIN) syn_audio_ramp_set(&voice->gain,
        syn_audio_db(command->values[3]), core->config.instance_gain_ms, core->config.sample_rate);
    return SYN_AUDIO_OK;
}

static syn_audio_result play(syn_audio_core* core, const syn_audio_command* command,
                             uint64_t* evicted, uint32_t* steal_reason)
{
    if (!command->sound_handle || command->sound_handle > core->status.sound_count)
        return SYN_AUDIO_MISSING;
    if (!finite_values(command, 5) || command->values[3] < -48 || command->values[3] > 12
        || command->values[4] < -24 || command->values[4] > 24) return SYN_AUDIO_INVALID;
    syn_audio_sound* sound = &core->sounds[command->sound_handle - 1];
    if (command->kind == SYN_AUDIO_START_LOOP) {
        if (!command->loop_key || !sound->desc.loop) return SYN_AUDIO_INVALID;
        syn_audio_voice* existing = loop_voice(core, command->loop_key);
        if (existing) return existing->sound != command->sound_handle ? SYN_AUDIO_CONFLICT
            : update(core, existing, command, SYN_AUDIO_HAS_POSITION | SYN_AUDIO_HAS_GAIN);
    } else if (command->loop_key) return SYN_AUDIO_INVALID;
    if (core->player_paused && sound->desc.freeze) return SYN_AUDIO_PAUSED;
    uint64_t cooldown = syn_audio_frames(core, (float)sound->desc.cooldown_ms);
    if (sound->has_started && core->status.rendered_frames - sound->last_start < cooldown)
        return SYN_AUDIO_COOLDOWN;
    uint32_t instances = 0;
    syn_audio_voice *vacant = NULL, *same = NULL, *victim = NULL;
    for (uint32_t i = 0; i < core->config.max_voices; ++i) {
        syn_audio_voice* voice = &core->voices[i];
        if (!voice->active) { if (!vacant) vacant = voice; continue; }
        if (voice->sound == command->sound_handle) {
            instances++;
            if (older(voice, same)) same = voice;
        }
        uint32_t priority = core->sounds[voice->sound - 1].desc.priority;
        uint32_t lowest = victim ? core->sounds[victim->sound - 1].desc.priority : 101;
        if (priority < lowest || (priority == lowest && older(voice, victim))) victim = voice;
    }
    if (instances >= sound->desc.max_instances) {
        if (sound->desc.overflow == SYN_AUDIO_DROP_NEW) return SYN_AUDIO_CAPACITY;
        vacant = same;
        *steal_reason = SYN_AUDIO_STEAL_PER_SOUND;
    } else if (!vacant) {
        if (!victim || sound->desc.priority < core->sounds[victim->sound - 1].desc.priority)
            return SYN_AUDIO_PRIORITY;
        vacant = victim;
        *steal_reason = SYN_AUDIO_STEAL_GLOBAL;
    }
    if (vacant->active) {
        *evicted = vacant->loop_key;
        syn_audio_voice_retire(core, vacant);
        core->status.steals++;
    }
    syn_audio_voice_begin(core, vacant, command->sound_handle, command);
    sound->has_started = 1; sound->last_start = core->status.rendered_frames;
    core->status.accepted++;
    return SYN_AUDIO_OK;
}

static void clear_world(syn_audio_core* core)
{
    for (uint32_t i = 0; i < core->config.max_voices; ++i) {
        syn_audio_voice* voice = &core->voices[i];
        if (voice->active && core->sounds[voice->sound - 1].desc.bus == SYN_AUDIO_WORLD)
            syn_audio_voice_retire(core, voice);
    }
}

static syn_audio_result rebase(syn_audio_core* core, const syn_audio_command* command)
{
    if (!finite_values(command, 12)) return SYN_AUDIO_INVALID;
    /* Validate the whole transform before mutating any voice. */
    for (uint32_t pass = 0; pass < 2; ++pass) {
        for (uint32_t i = 0; i < core->config.max_voices; ++i) {
            syn_audio_voice* voice = &core->voices[i];
            if (!voice->active || core->sounds[voice->sound - 1].desc.bus != SYN_AUDIO_WORLD) continue;
            float xyz[3];
            for (uint32_t row = 0; row < 3; ++row) {
                const float* m = command->values + row * 4;
                double value = (double)m[0] * voice->xyz[0] + (double)m[1] * voice->xyz[1]
                    + (double)m[2] * voice->xyz[2] + m[3];
                if (!isfinite(value) || fabs(value) > FLT_MAX) return SYN_AUDIO_INVALID;
                xyz[row] = (float)value;
            }
            if (pass) memcpy(voice->xyz, xyz, sizeof(xyz));
        }
    }
    return SYN_AUDIO_OK;
}

/* Haskell supplies the current frame's period vectors. Rebase alone preserves
   an image of a source, but that image stops being the nearest one when camera
   motion crosses the source's antipode. No page/grid types enter this boundary. */
static syn_audio_result wrap_frame(syn_audio_core* core, const syn_audio_command* command)
{
    if (!finite_values(command, 6)) return SYN_AUDIO_INVALID;
    double length[2] = {0, 0}, dot = 0;
    for (uint32_t axis = 0; axis < 3; ++axis) {
        double a = command->values[axis], b = command->values[axis + 3];
        length[0] += a * a; length[1] += b * b; dot += a * b;
    }
    if (fabs(dot) > sqrt(length[0] * length[1]) * 1e-5) return SYN_AUDIO_INVALID;
    for (uint32_t pass = 0; pass < 2; ++pass) {
        for (uint32_t i = 0; i < core->config.max_voices; ++i) {
            syn_audio_voice* voice = &core->voices[i];
            if (!voice->active || core->sounds[voice->sound - 1].desc.bus != SYN_AUDIO_WORLD) continue;
            double xyz[3] = {voice->xyz[0], voice->xyz[1], voice->xyz[2]};
            for (uint32_t period = 0; period < 2; ++period) {
                if (length[period] == 0) continue;
                const float* basis = command->values + period * 3;
                double projection = 0;
                for (uint32_t axis = 0; axis < 3; ++axis) projection += xyz[axis] * basis[axis];
                double turns = floor(projection / length[period] + 0.5);
                for (uint32_t axis = 0; axis < 3; ++axis) xyz[axis] -= turns * basis[axis];
            }
            for (uint32_t axis = 0; axis < 3; ++axis) {
                if (!isfinite(xyz[axis]) || fabs(xyz[axis]) > FLT_MAX) return SYN_AUDIO_INVALID;
                if (pass) voice->xyz[axis] = (float)xyz[axis];
            }
        }
    }
    return SYN_AUDIO_OK;
}

static syn_audio_result apply(syn_audio_core* core, const syn_audio_command* command,
                              uint64_t* evicted, uint32_t* steal_reason)
{
    if (command->abi_version != SYN_AUDIO_ABI || command->struct_size != sizeof(*command))
        return SYN_AUDIO_INVALID;
    switch (command->kind) {
    case SYN_AUDIO_PLAY:
    case SYN_AUDIO_START_LOOP:
        return play(core, command, evicted, steal_reason);
    case SYN_AUDIO_UPDATE_LOOP:
        return update(core, loop_voice(core, command->loop_key), command, command->flags);
    case SYN_AUDIO_STOP_LOOP: {
        syn_audio_voice* voice = loop_voice(core, command->loop_key);
        if (!voice) { core->status.missing_loops++; return SYN_AUDIO_MISSING; }
        float fade = core->sounds[voice->sound - 1].desc.stop_fade_ms;
        voice->loop_key = 0; core->status.active_loops--; /* Release identity now. */
        voice->stopping = 1;
        syn_audio_ramp_set(&voice->stop_gain, 0, fade, core->config.sample_rate);
        if (!voice->stop_gain.left) syn_audio_voice_retire(core, voice);
        return SYN_AUDIO_OK;
    }
    case SYN_AUDIO_REBASE:
        return rebase(core, command);
    case SYN_AUDIO_WRAP_FRAME:
        return wrap_frame(core, command);
    case SYN_AUDIO_VOLUMES: {
        if (!finite_values(command, 3)) return SYN_AUDIO_INVALID;
        for (uint32_t i = 0; i < 3; ++i)
            if (command->values[i] < 0 || command->values[i] > 1) return SYN_AUDIO_INVALID;
        float ms = core->catalog_sealed ? core->config.bus_gain_ms : 0;
        syn_audio_ramp_set(&core->master_gain, command->values[0], ms, core->config.sample_rate);
        syn_audio_ramp_set(&core->world_gain, command->values[1], ms, core->config.sample_rate);
        syn_audio_ramp_set(&core->ui_gain, command->values[2], ms, core->config.sample_rate);
        return SYN_AUDIO_OK;
    }
    case SYN_AUDIO_WORLD_MIX:
        if (!finite_values(command, 2) || command->values[0] <= 0 || command->values[0] > 100
            || command->values[1] < 0 || command->values[1] > 16) return SYN_AUDIO_INVALID;
        core->range_scale = command->values[0];
        syn_audio_ramp_set(&core->zoom_gain, command->values[1], core->config.bus_gain_ms, core->config.sample_rate);
        return SYN_AUDIO_OK;
    case SYN_AUDIO_PAUSE:
        if (command->flags > 1) return SYN_AUDIO_INVALID;
        if (core->player_paused == command->flags) return SYN_AUDIO_OK;
        core->player_paused = command->flags;
        for (uint32_t i = 0; i < core->config.max_voices; ++i) {
            syn_audio_voice* voice = &core->voices[i];
            if (voice->active && core->sounds[voice->sound - 1].desc.freeze)
                syn_audio_ramp_set(&voice->pause_gain, core->player_paused ? 0 : 1,
                    core->player_paused ? core->config.pause_out_ms : core->config.pause_in_ms,
                    core->config.sample_rate);
        }
        return SYN_AUDIO_OK;
    case SYN_AUDIO_RESET: {
        uint32_t restart = core->started;
        syn_audio_stop(core); /* Join callback before resetting either ring cursor. */
        ma_pcm_rb_reset(&core->ring);
        atomic_store_explicit(&core->device_event, 0, memory_order_relaxed);
        for (uint32_t i = 0; i < core->config.max_voices; ++i) syn_audio_voice_retire(core, &core->voices[i]);
        for (uint32_t i = 0; i < core->status.sound_count; ++i) core->sounds[i].has_started = 0;
        core->player_paused = 0;
        /* Reset succeeded even when output is temporarily unavailable. Device
           health remains visible in status and service retains retry intent. */
        if (restart) syn_audio_start(core);
        return SYN_AUDIO_OK;
    }
    case SYN_AUDIO_CLEAR_WORLD:
        clear_world(core); return SYN_AUDIO_OK;
    default:
        return SYN_AUDIO_INVALID;
    }
}

syn_audio_result syn_audio_commands(syn_audio_core* core, const syn_audio_command* commands,
    uint32_t count, syn_audio_command_result* results)
{
    if (!core || (count && (!commands || !results))) return SYN_AUDIO_INVALID;
    if (count > core->config.command_batch_limit) return SYN_AUDIO_LIMIT;
    for (uint32_t i = 0; i < count; ++i) {
        results[i] = (syn_audio_command_result){.abi_version = SYN_AUDIO_ABI,
            .struct_size = sizeof(*results)};
        results[i].result = apply(core, &commands[i], &results[i].evicted_loop_key, &results[i].steal_reason);
        if ((commands[i].kind == SYN_AUDIO_PLAY || commands[i].kind == SYN_AUDIO_START_LOOP)
            && results[i].result != SYN_AUDIO_OK) core->status.dropped++;
    }
    return SYN_AUDIO_OK;
}

syn_audio_result syn_audio_submit_offline(syn_audio_core* core, const syn_audio_command* commands,
    uint32_t count, syn_audio_command_result* results)
{
    if (!core) return SYN_AUDIO_INVALID;
    if (core->started) return SYN_AUDIO_BAD_STATE;
    return syn_audio_commands(core, commands, count, results);
}
