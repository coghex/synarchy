"""Offline audition, not the unimplemented Synarchy native runtime.

Matches the planned DSP ingredients/order: xorshift32 noise -> linear ADSR
-> one fixed RBJ band-pass in transposed direct form II -> authored gain.
Uses constant-0dB-peak BPF coefficients from:
https://www.w3.org/TR/audio-eq-cookbook/#:~:text=constant%200%20dB%20peak
Seed derivation, floating-point precision, voice retirement and common mixer
staging are preview conventions, not an assertion of native bit parity.
"""
import hashlib
import json
import math
from pathlib import Path
import struct
import wave

ROOT = Path(__file__).resolve().parent
RATE = 48000
PATCHES = {
    "selected": dict(cutoff_hz=620, resonance_q=2.8, attack_ms=0.2,
                     decay_ms=5, sustain_level=0, gate_ms=6, release_ms=55,
                     noise_seed=1831565813, gain_db=12),
    "back": dict(cutoff_hz=310, resonance_q=4.2, attack_ms=0.25,
                 decay_ms=7, sustain_level=0, gate_ms=8, release_ms=85,
                 noise_seed=1831565813, gain_db=15.5),
}


def render(p):
    frames = lambda ms: round(ms * RATE / 1000)
    attack, decay, gate, release = [frames(p[k]) for k in
                                  ("attack_ms", "decay_ms", "gate_ms", "release_ms")]
    def held(n):
        if n < attack:
            return n / attack
        if n < attack + decay:
            return 1 - (1 - p["sustain_level"]) * (n - attack) / decay
        return p["sustain_level"]

    omega = 2 * math.pi * p["cutoff_hz"] / RATE
    alpha = math.sin(omega) / (2 * p["resonance_q"])
    a0 = 1 + alpha
    b0, b2 = alpha / a0, -alpha / a0
    a1, a2 = -2 * math.cos(omega) / a0, (1 - alpha) / a0
    z1 = z2 = 0.0
    seed = p["noise_seed"] or 1831565813
    gain = 10 ** (p["gain_db"] / 20)
    result = []
    for n in range(gate + release):
        seed ^= (seed << 13) & 0xffffffff
        seed ^= seed >> 17
        seed ^= (seed << 5) & 0xffffffff
        seed &= 0xffffffff
        env = held(n) if n < gate else held(gate) * (1 - (n - gate) / release)
        x = ((seed / 4294967296) * 2 - 1) * env
        y = b0 * x + z1
        z1, z2 = -a1 * y + z2, b2 * x - a2 * y
        z1 = 0.0 if abs(z1) < 1e-20 else z1
        z2 = 0.0 if abs(z2) < 1e-20 else z2
        result.append(y * gain)
    assert all(math.isfinite(x) for x in result)
    assert max(map(abs, result)) < 0.95, "Clipping/headroom failure"
    assert abs(result[-1]) < 1 / 32768, "Audible filter-tail truncation"
    return result


def write(name, samples):
    data = b"".join(struct.pack("<hh", round(x * 32767), round(x * 32767)) for x in samples)
    path = ROOT / name
    with wave.open(str(path), "wb") as f:
        f.setnchannels(2)
        f.setsampwidth(2)
        f.setframerate(RATE)
        f.writeframes(data)
    with wave.open(str(path), "rb") as f:
        assert (f.getnchannels(), f.getsampwidth(), f.getframerate(), f.getnframes()) == (2, 2, RATE, len(samples))
    return dict(path=str(path), duration_s=len(samples) / RATE,
                peak_dbfs=20 * math.log10(max(map(abs, samples))),
                sha256=hashlib.sha256(path.read_bytes()).hexdigest())


def main():
    sounds = {name: render(patch) for name, patch in PATCHES.items()}
    # Regeneration is deterministic for these fixed audition seeds.
    assert all(sounds[k] == render(PATCHES[k]) for k in sounds)
    report = {k: write(k + ".wav", v) for k, v in sounds.items()}
    silence = lambda seconds: [0.0] * round(seconds * RATE)
    sequence = silence(0.25)
    for _ in range(3):
        sequence += sounds["selected"] + silence(0.55)
        sequence += sounds["back"] + silence(0.85)
    report["audition"] = write("selected-then-back.wav", sequence)
    (ROOT / "patches.json").write_text(json.dumps(PATCHES, indent=2) + "\n")
    (ROOT / "verification.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report))


if __name__ == "__main__":
    main()
