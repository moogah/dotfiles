#!/usr/bin/env bash
# get-machine-id.sh - Get stable machine identifier from ~/.machine-id
#
# This script reads the machine ID from ~/.machine-id file.
# The machine ID is used throughout the dotfiles for machine-specific configuration.
#
# Available machine IDs:
#   - apploi-mac: Work MacBook Air
#   - personal-mac: Personal MacBook Pro
#   - personal-mac-air: Personal MacBook Air
#
# Setup:
#   echo "apploi-mac" > ~/.machine-id

set -euo pipefail

MACHINE_ID_FILE="$HOME/.machine-id"

if [[ ! -f "$MACHINE_ID_FILE" ]]; then
    cat >&2 <<EOF
ERROR: Machine ID file not found: $MACHINE_ID_FILE

To set up your machine ID, create the file with one of the following values:
  - apploi-mac (Work MacBook Air)
  - personal-mac (Personal MacBook Pro)
  - personal-mac-air (Personal MacBook Air)

Example:
  echo "apploi-mac" > ~/.machine-id

This file ensures stable machine identification even when hostname changes.
EOF
    exit 1
fi

# Read the machine ID, removing any trailing whitespace
MACHINE_ID=$(cat "$MACHINE_ID_FILE" | tr -d '[:space:]')

if [[ -z "$MACHINE_ID" ]]; then
    echo "ERROR: Machine ID file is empty: $MACHINE_ID_FILE" >&2
    exit 1
fi

echo "$MACHINE_ID"
