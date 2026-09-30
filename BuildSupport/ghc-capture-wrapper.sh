#!/bin/sh
# Stands in for the configured ghc while Setup builds the package (#2648;
# BuildSupport/GhcCapture.hs installs it and promotes what it records).
#
# It records the exact command Cabal ran -- the raw arguments, NUL-separated,
# and a byte copy of every @response file, which Cabal deletes afterwards --
# then runs the real compiler with those arguments unchanged: RTS sections,
# response files and the environment pass straight through. The record is
# published only when the compiler succeeds. If it cannot be written the
# compile fails (exit 70), so a build can never succeed without its record.
real=${SYNARCHY_CAPTURE_REAL_GHC:?SYNARCHY_CAPTURE_REAL_GHC is not set}
dir=${SYNARCHY_CAPTURE_DIR:?SYNARCHY_CAPTURE_DIR is not set}
fail() {
    echo "ghc-capture-wrapper: $1" >&2
    exit 70
}
work=$(mktemp -d "$dir/.inv.XXXXXX") || fail "cannot create a record in $dir"
: > "$work/argv" || fail "cannot write $work/argv"
i=0
for arg in "$@"; do
    printf '%s\0' "$arg" >> "$work/argv" || fail "cannot write $work/argv"
    case $arg in
        @*) cp "${arg#@}" "$work/rsp.$i" || fail "cannot copy response file ${arg#@}" ;;
    esac
    i=$((i + 1))
done
"$real" "$@"
status=$?
if [ "$status" -eq 0 ]; then
    mv "$work" "$dir/inv.${work##*/.inv.}" || fail "cannot publish $work"
fi
exit "$status"
