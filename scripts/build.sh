#!/bin/bash
set -euo pipefail
trap 'echo "script error: $(basename "$BASH_SOURCE"):$LINENO $BASH_COMMAND" >&2' ERR
script_dir="$(dirname -- "$(readlink -f -- "$0")")"

root_dir="$script_dir/.."

which asdf &>/dev/null || . "$HOME/.asdf/asdf.sh" || echo "asdf is not installed, using local sbt binary" >&2

cd "$root_dir"
sbt assembly || exit $?
