"""The persistence-inventory self-test's case owners and fixtures (#2138).

`tools/test_persistence_inventory_audit.py` stays the aggregate command
CI and `tools/ci-local.sh` invoke; this package holds the test bodies and
fixtures it composes, divided into six families:

  `haskell`      Haskell record parsing and its audit mutations;
  `lua_parser`   the Lua scanners, asserted on extracted names;
  `lua_audit`    the same Lua fixtures, asserted on audit verdicts;
  `inventory`    inventory-document scope, taxonomy and the real repo;
  `references`   typed Haskell and Lua persistent references (#764);
  `topology`     component registration, the coverage map, registry
                 derivation, and #2124's ownership-split structure.

alongside `support` and three fixture modules. Importing this package
runs no test and imports no case owner: the façade imports the six
families itself, so `--family` can select one without paying for the
other five, and nothing here can register a group by side effect.
"""
