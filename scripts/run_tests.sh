#!/bin/bash
set -euo pipefail
trap 'echo "Script error: $(basename "$BASH_SOURCE"):$LINENO $BASH_COMMAND" >&2' ERR

root_dir="$(dirname "$0")/.."
input_dir="$root_dir/examples"
output_dir="$root_dir/test-out"

tinycc="$root_dir/tinycc"
t86_cli="$root_dir/t86/build/t86-cli/t86-cli"

fancy_diff () {
#  diff --side-by-side -s "$1" "$2" | colordiff

  diff -us "$1" "$2" | colordiff
}

run_test () {
  local file="$1"
  local name="$(basename $1)"
  local out_base="$output_dir/$name"

  echo "== $name =="

  local expected="$out_base.expected"
  awk 'match($0, /^\/\/ > (.+)/, m) { print m[1] }' "$file" >"$expected"

  local cfile="$out_base.gcc"
  local cbin="$cfile.bin"
  local cout="$cfile.out"

  "$tinycc" transpile-to-c --prefix='#include "gcc_runtime.h"' -o "$cfile" "$file"
  gcc -x c -Wall --std=c99 -I "$root_dir" "$cfile" -o "$cbin"
  "$cbin" >"$cout"

  fancy_diff "$expected" "$cout"

  local asmfile="$out_base.t86"
  local asmerr="$asmfile.err"
  local asmout="$asmfile.out"

  if ! "$tinycc" compile -o "$asmfile" "$file" &>"$asmerr"; then
    cat "$asmerr" >&2
    return 1
  fi

  "$t86_cli" run "$asmfile" -registerCnt=32 -floatRegisterCnt=8 >"$asmout"

  fancy_diff "$expected" "$asmout"
}

mkdir -p "$output_dir"

if [ $# -gt 0 ]; then
  for file in "$@"; do
    run_test "$input_dir/$(basename "$file")"
  done
else
  test_files="$(find "$input_dir" -iname '*.c' -print | sort)"
  for file in $test_files; do
    run_test "$file"
  done
fi
