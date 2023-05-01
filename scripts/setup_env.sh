#!/bin/bash
set -euo pipefail
trap 'echo "script error: $(basename "$BASH_SOURCE"):$LINENO $BASH_COMMAND" >&2' ERR
script_dir="$(dirname -- "$(readlink -f -- "$0")")"

root_dir="$script_dir/.."
cd "$root_dir"

[ -f "$HOME/.asdf/asdf.sh" ] || git clone https://github.com/asdf-vm/asdf.git "$HOME/.asdf" --branch v0.11.2

which asdf &>/dev/null || . "$HOME/.asdf/asdf.sh"

asdf plugin add java || true
asdf plugin add scala || true
asdf plugin add sbt || true

asdf install
