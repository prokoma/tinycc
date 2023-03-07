#!/bin/bash
set -euo pipefail
trap 'echo "Script error: $(basename "$BASH_SOURCE"):$LINENO $BASH_COMMAND" >&2' ERR

root_dir="$(dirname "$0")/.."

[ -f "$HOME/.asdf/asdf.sh" ] || git clone https://github.com/asdf-vm/asdf.git "$HOME/.asdf" --branch v0.11.2

which asdf &>/dev/null || . "$HOME/.asdf/asdf.sh"

asdf plugin add java || true
asdf plugin add scala || true
asdf plugin add sbt || true

asdf install