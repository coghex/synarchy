#!/usr/bin/env python3
"""Exercise the real HLint config; temporary snippets need no game/GHC build.

Python owns this tool-boundary check because the assertions concern HLint's
CLI JSON, import matching and configuration, not compiled Haskell behavior.
"""
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class HLintRules(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        version = subprocess.check_output(
            ["hlint", "--numeric-version"], text=True).strip()
        if version != (ROOT / ".hlint-version").read_text().strip():
            raise RuntimeError(f"Wrong HLint version: {version}")

    def hints(self, body, module="Example"):
        with tempfile.TemporaryDirectory(prefix="synarchy-hlint-") as temp:
            source = Path(temp) / "Example.hs"
            source.write_text(f"module {module} where\n{body}\n")
            result = subprocess.run(
                ["hlint", "--json", str(source)], cwd=ROOT,
                capture_output=True, text=True, check=False)
            self.assertIn(result.returncode, (0, 1), result.stderr)
            hints = json.loads(result.stdout)
            self.assertFalse([h for h in hints if h["hint"].startswith("Parse error")])
            return hints

    def test_tshow_application_and_composition(self):
        hints = self.hints("""import qualified Data.Text as TXT
f x = TXT.pack (show x)
g = TXT.pack . show
h = TXT.pack ∘ show
""")
        matches = [h for h in hints if h["hint"] == "Use tshow"]
        self.assertEqual(len(matches), 3)
        self.assertEqual({h["to"] for h in matches}, {"tshow", "tshow x"})

    def test_other_pack_functions_are_not_text(self):
        hints = self.hints("""import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as L
f x = B.pack (show x)
g x = L.pack (show x)
""")
        self.assertNotIn("Use tshow", [h["hint"] for h in hints])

    def test_tshow_definition_is_exempt(self):
        hints = self.hints("""import qualified Data.Text as TXT
tshow = TXT.pack ∘ show
other x = TXT.pack (show x)
""", module="UPrelude")
        matches = [h for h in hints if h["hint"] == "Use tshow"]
        self.assertEqual(len(matches), 1)
        self.assertEqual(matches[0]["decl"], ["other"])

    def test_unsafe_exception_is_binding_scoped(self):
        hints = self.hints("""import qualified System.IO.Unsafe as U
processLibraryLock = U.unsafePerformIO (pure 1)
newBinding = U.unsafePerformIO (pure 2)
dup = U.unsafeDupablePerformIO (pure 3)
lazyIO = U.unsafeInterleaveIO (pure 4)
""", module="World.GeneratedLibrary.Lock")
        matches = [h for h in hints if h["hint"] == "Avoid restricted function"]
        self.assertEqual({h["decl"][0] for h in matches},
                         {"newBinding", "dup", "lazyIO"})

    def test_tshow_reference_test_is_exempt(self):
        hints = self.hints("""import qualified Data.Text as T
spec = tshow 42 == T.pack (show 42)
""", module="Test.Headless.UPrelude")
        self.assertNotIn("Use tshow", [h["hint"] for h in hints])

    def test_trace_import_is_restricted(self):
        hints = self.hints('import qualified Debug.Trace as D\nf = D.trace "debug" 1')
        self.assertIn("Avoid restricted module", [h["hint"] for h in hints])

    def test_default_hints_remain_enabled(self):
        hints = self.hints("f g xs = concat (map g xs)")
        self.assertIn("Use concatMap", [h["hint"] for h in hints])


if __name__ == "__main__":
    unittest.main()
