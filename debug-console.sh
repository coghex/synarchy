#!/usr/bin/env bash
# Interactive debug console for the synarchy engine.
# Connects to the Lua debug server on localhost:8008.
#
# Usage:
#   ./debug-console.sh              # interactive REPL
#   ./debug-console.sh -c 'cmd'     # run a single command and exit
#
# Requires: nc (netcat). Install rlwrap for readline support (history, editing).

PORT="${SYNARCHY_DEBUG_PORT:-8008}"
HOST="localhost"

# Single-command mode
if [[ "$1" == "-c" && -n "$2" ]]; then
    printf '%s\n' "$2" | nc -w 2 "$HOST" "$PORT" 2>/dev/null | tail -n +2 | head -n -1
    exit $?
fi

# Interactive mode
if ! nc -z "$HOST" "$PORT" 2>/dev/null; then
    echo "Cannot connect to $HOST:$PORT. Is the engine running?"
    exit 1
fi

if command -v rlwrap &>/dev/null; then
    echo "Connected (rlwrap enabled — use arrow keys for history)"
    rlwrap nc "$HOST" "$PORT"
else
    echo "Connected (install rlwrap for readline support)"
    nc "$HOST" "$PORT"
fi
