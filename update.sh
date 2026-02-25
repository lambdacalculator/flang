#!/bin/bash
# update.sh - Script to easily update the FLang library

echo "Pulling latest changes from GitHub..."
git pull

echo "Re-installing the library globally..."
cabal install --lib .

echo "Update complete! You can now use 'import FLang' in GHCi from any directory."
