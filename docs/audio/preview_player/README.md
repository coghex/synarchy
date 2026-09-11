# Audio preview evidence

Captured on 2026-09-10 from the foreground audio implementation on macOS.
`python3 tools/preview_probe.py --only audio --port 9194` passed all 11 checks
across three real GLFW/Vulkan preview boots. The probe forced hidden,
non-activating windows and the null audio backend. These screenshots establish
rendered UI behavior; they do not establish physical-device listening quality.
The absolute source paths in the hash manifest record the original local
captures; these colocated copies are the reviewable artifacts.

- [Synth](synth.png): both authored menu cues; real input reached each native
  voice, with one audition at a time. Play, Stop, Reload and Master/UI controls
  are visible and their labels fit.
- [Files](file.png): an explicitly selected external WAV with a Unicode/space
  path autoplayed through the engine decoder. This is a mathematical test tone,
  not proposed bear audio. Overwriting it with the stereo fixture and clicking
  Reload replaced 12,000 decoded frames with 480 without accumulating samples.
- [Visual footer](footer.png): the Audio control remains visible at the bottom
  left of a normal visual browser. Real clicks opened Audio and restored the
  previous visual selection on return.

The shipped `Audio.Preview` tests separately compare actual mixed PCM for both
menu cues against their in-game first playback, exercise sample decode failure,
reload and shutdown. `Audio.PreviewUI` drives the shipped Lua pane and checks
selection, reflow, controls, reload revision ordering and the required UI page
arguments. Full local validation history is in
[the implementation record](../../audio_implementation_progress.md).

On 2026-09-11 the owner said the preview looked good and was ready for a PR.
That approves the foundation/player delivery boundary. Physical listening and
real-device recovery have no recorded acceptance verdict yet. Bear sounds and
unit selection responses are the next foreground task.
