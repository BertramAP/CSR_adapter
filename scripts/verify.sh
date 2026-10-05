#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

for tool in java sbt verilator make g++; do
  command -v "$tool" > /dev/null || { echo "Missing verification tool: $tool" >&2; exit 1; }
done
java -version
verilator --version
# Explicit selection forces every suite to run even with sbt 2's incremental test task.
CSR_TEST_BACKEND=verilator sbt clean 'testOnly *' 'runMain CsrAdapter'
verilator --lint-only --top-module CsrAdapter generated/CsrAdapter.sv
