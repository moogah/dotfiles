#!/bin/bash
# Generic script to tangle Emacs org files
# Usage: ./tangle-org.sh path/to/file.org

# Check for argument
if [ -z "$1" ]; then
    echo "Error: No org file specified"
    echo "Usage: $(basename $0) path/to/file.org"
    exit 1
fi

ORG_FILE="$1"

# Check if file exists
if [ ! -f "$ORG_FILE" ]; then
    echo "Error: File '$ORG_FILE' not found"
    exit 1
fi

# Check if file is an org file
if [[ "$ORG_FILE" != *.org ]]; then
    echo "Warning: '$ORG_FILE' doesn't have an .org extension"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Path to Emacs on macOS
EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"

# Check if Emacs exists at the expected location
if [ ! -f "$EMACS" ]; then
    # Try alternate locations or use command which
    if command -v emacs &> /dev/null; then
        EMACS=$(command -v emacs)
    else
        echo "Error: Emacs not found at '$EMACS' or in PATH"
        exit 1
    fi
fi

echo "Tangling $ORG_FILE..."

# Tangle the org file
"$EMACS" --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$ORG_FILE\")"

# Check the exit status
if [ $? -eq 0 ]; then
    echo "Successfully tangled $ORG_FILE"
else
    echo "Error tangling $ORG_FILE"
    exit 1
fi