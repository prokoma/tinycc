#!/bin/bash
set -euo pipefail
trap 'echo "Script error: $(basename "$BASH_SOURCE"):$LINENO $BASH_COMMAND" >&2' ERR

root_dir="$(dirname "$0")/.."

which asdf &>/dev/null || . "$HOME/.asdf/asdf.sh"

cd "$root_dir"
sbt assembly
