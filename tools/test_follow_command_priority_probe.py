#!/usr/bin/env python3
"""Unit tests for check E's grading in follow_command_priority_probe (#2945).

The probe is a manual engine run, so the half that must not regress silently
is the pure classifier ``grade_e``: it decides whether E's arbitration was
really exercised (setup) before E's two assertions may be graded or passed.
The property it exists for: E asserts that combat beats a pending NON-PLAYER
move, so a run in which that move was already gone when combat was observed
would otherwise pass both assertions vacuously. Pinned here:

  * a clean trace (hit landed, move pending in the combat sample) grades;
  * a hit and a pending move in the FIRST sample, then combat samples with
    the move already cleared (arrival before combat), is a SETUP failure,
    not a pass (the trace review round 3 reproduced);
  * no qualifying hit is a setup failure, whatever else was observed;
  * a pending PLAYER move does not count as the non-player move E needs;
  * hand-back counts only while the staged attacker is alive;
  * a landed hit with the move pending but combat never reached is an
    arbitration outcome (reached False), not a setup failure.
"""
import unittest

import follow_command_priority_probe as probe


def sample(action, *, hit=False, move=True, player=False, atk="standing"):
    return {"action": action, "hitByStaged": hit, "movePending": move,
            "movePlayer": player, "attackerPose": atk}


class GradeE(unittest.TestCase):
    def test_clean_trace_grades(self):
        s = [sample("follow_command"), sample("attack_target", hit=True)]
        tail = [sample("attack_target", hit=True)] * 3
        self.assertEqual(probe.grade_e(s + tail, tail),
                         (None, True, False))

    def test_move_gone_before_combat_is_setup_failure(self):
        s = [sample("follow_command", hit=True),
             sample("engage", hit=True, move=False),
             sample("attack_target", hit=True, move=False)]
        why, reached, reverted = probe.grade_e(s, s[1:])
        self.assertIn("no longer pending", why)
        self.assertTrue(reached)
        self.assertFalse(reverted)

    def test_no_hit_is_setup_failure(self):
        s = [sample("follow_command"), sample("search_for_water", move=False)]
        why, reached, _ = probe.grade_e(s, s)
        self.assertIn("never landed a qualifying hit", why)
        self.assertFalse(reached)

    def test_player_move_is_not_the_move_e_needs(self):
        s = [sample("attack_target", hit=True, player=True)]
        why, _, _ = probe.grade_e(s, s)
        self.assertIn("no longer pending", why)

    def test_hand_back_counts_only_while_the_attacker_lives(self):
        alive = [sample("attack_target", hit=True),
                 sample("follow_command", hit=True)]
        dead = [sample("attack_target", hit=True),
                sample("follow_command", hit=True, atk="dead")]
        self.assertTrue(probe.grade_e(alive, alive)[2])
        self.assertFalse(probe.grade_e(dead, dead)[2])

    def test_hit_with_pending_move_but_no_combat_is_arbitration(self):
        s = [sample("treat_ally", hit=True), sample("treat_ally", hit=True)]
        self.assertEqual(probe.grade_e(s, s), (None, False, False))


if __name__ == "__main__":
    unittest.main()
