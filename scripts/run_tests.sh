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

extract_template () {
  ( cd "$(dirname "$2")" && awk '
match($0, /^\/\/ '$1' (.+)/, m) { print m[1] }
match($0, /^\/\/ '$1'! (.+)/, m) {
  while((getline < m[1]) == 1) {
    print
  }
}' "$(basename "$2")" )
}


run_test () {
  local file="$1"
  local name="$(basename $1)"
  local out_base="$output_dir/$name"

  echo "== $name =="

  local in="$out_base.in"
  local ref="$out_base.ref"

  extract_template "<" "$file" >"$in"
  extract_template ">" "$file" >"$ref"

  local cfile="$out_base.transpiled.c"
  local cbin="$out_base.transpiled"
  local cout="$out_base.transpiled.out"

  (
    "$tinycc" transpile-to-c --prefix='#include "gcc_runtime.h"' -o "$cfile" "$file" &&
    gcc -Wall --std=c99 -I "$root_dir" -fno-builtin -fsigned-char "$cfile" "$output_dir/gcc_runtime.o" -o "$cbin" &&
    "$cbin" <"$in" >"$cout" &&
    fancy_diff "$ref" "$cout"
  ) || true

  local asmfile="$out_base.t86"
  local asmerr="$asmfile.err"
  local asmout="$asmfile.out"

  if ! "$tinycc" compile --verbose --profile --optimize -o "$asmfile" "$file" &>"$asmerr"; then
    cat "$asmerr" >&2
    return 1
  fi

  echo "Running t86-cli..." >>"$asmerr"
  if ! "$t86_cli" run "$asmfile" -registerCnt=32 -floatRegisterCnt=8 <"$in" >"$asmout" 2>>"$asmerr"; then
    cat "$asmerr" >&2
    fancy_diff "$ref" "$asmout"
    return 1
  fi

  fancy_diff "$ref" "$asmout"
}

mkdir -p "$output_dir"

gcc -fsigned-char -c gcc_runtime.c -o "$output_dir/gcc_runtime.o"

if [ $# -gt 0 ]; then
  for file in "$@"; do
    if [ -f "$file" ]; then
      run_test "$file"
    else
      run_test "$input_dir/$(basename "$file")"
    fi
  done
else
  test_files="$(find "$input_dir" -iname '*.c' -print | sort)"
  for file in $test_files; do
    run_test "$file"
  done
fi
