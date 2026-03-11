#!/usr/bin/env python3
"""Cross-platform client for the synarchy Lua debug socket."""

from __future__ import annotations

import argparse
import socket
import sys
from dataclasses import dataclass

PROMPT = b"> "


@dataclass
class DebugClient:
    host: str
    port: int
    timeout: float
    _sock: socket.socket | None = None

    def connect(self) -> None:
        self._sock = socket.create_connection((self.host, self.port), timeout=self.timeout)
        self._sock.settimeout(self.timeout)
        self._read_until_prompt()

    def close(self) -> None:
        if self._sock is not None:
            self._sock.close()
            self._sock = None

    def run_command(self, command: str) -> str:
        if self._sock is None:
            raise RuntimeError("Debug client is not connected")

        line = command.rstrip("\r\n")
        if "\n" in line:
            raise ValueError("Debug server commands must be a single line")

        self._sock.sendall(line.encode("utf-8") + b"\n")
        return self._strip_prompt(self._read_until_prompt())

    def _read_until_prompt(self) -> bytes:
        if self._sock is None:
            raise RuntimeError("Debug client is not connected")

        chunks: list[bytes] = []
        while True:
            chunk = self._sock.recv(4096)
            if not chunk:
                break
            chunks.append(chunk)
            if b"".join(chunks).endswith(PROMPT):
                break
        return b"".join(chunks)

    @staticmethod
    def _strip_prompt(payload: bytes) -> str:
        if payload.endswith(PROMPT):
            payload = payload[: -len(PROMPT)]
        return payload.decode("utf-8", errors="replace")


def print_response(response: str) -> None:
    if response:
        sys.stdout.write(response)
        if not response.endswith("\n"):
            sys.stdout.write("\n")
    sys.stdout.flush()


def run_single_command(args: argparse.Namespace) -> int:
    client = DebugClient(args.host, args.port, args.timeout)
    try:
        client.connect()
        print_response(client.run_command(args.command))
    finally:
        client.close()
    return 0


def run_stdin_commands(args: argparse.Namespace) -> int:
    client = DebugClient(args.host, args.port, args.timeout)
    try:
        client.connect()
        for raw_line in sys.stdin:
            line = raw_line.rstrip("\r\n")
            if not line:
                continue
            print_response(client.run_command(line))
    finally:
        client.close()
    return 0


def run_repl(args: argparse.Namespace) -> int:
    client = DebugClient(args.host, args.port, args.timeout)
    try:
        client.connect()
        print(f"Connected to {args.host}:{args.port}. Type 'quit' or Ctrl-D to exit.")
        while True:
            try:
                line = input("> ")
            except EOFError:
                sys.stdout.write("\n")
                break
            if line.strip() in {"quit", "exit"}:
                break
            print_response(client.run_command(line))
    finally:
        client.close()
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-c", "--command", help="Run one Lua command and exit")
    parser.add_argument("--host", default="localhost", help="Debug server host")
    parser.add_argument("--port", type=int, default=8008, help="Debug server port")
    parser.add_argument(
        "--timeout",
        type=float,
        default=2.0,
        help="Socket timeout in seconds",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        if args.command is not None:
            return run_single_command(args)
        if not sys.stdin.isatty():
            return run_stdin_commands(args)
        return run_repl(args)
    except (OSError, ValueError, RuntimeError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
