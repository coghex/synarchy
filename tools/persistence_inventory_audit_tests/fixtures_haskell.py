#!/usr/bin/env python3
"""Synthetic Haskell sources two or more families feed the audit (#2138).

`SYNTHETIC_ENGINE_ENV` and `FAKE_ROOT_RECORDS` are the pair every
end-to-end group builds its `audit()` call from: the record text and the
`ROOT_RECORDS` entry that points at it. Five of the six families use
them, which is why they live here rather than with any one owner. A
fixture only the Haskell family reads stays in `haskell.py`.

This module imports no case owner (#2138 requirement 16).
"""
from __future__ import annotations



# ----- Fixtures --------------------------------------------------------

SYNTHETIC_ENGINE_ENV = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
    -- ^ a documented field, with a stray brace in prose: {not real}
  , fieldTwo   ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)

data SomethingElse = SomethingElse { unrelated ∷ Int }
"""


FAKE_ROOT_RECORDS = [("EngineEnv", "Fake.hs", r"^data EngineEnv = EngineEnv\b")]
