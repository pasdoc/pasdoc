#!/bin/bash
set -euo pipefail

# ------------------------------------------------------------------------
# Downloads Bootstrap CSS and JS bundle.
# Usage: ./update_bootstrap.sh [VERSION]
# VERSION is optional -- by default it downloads the version we tested last.
# ------------------------------------------------------------------------

BOOTSTRAP_VERSION="${1:-5.3.8}"
BASE_URL="https://cdn.jsdelivr.net/npm/bootstrap@${BOOTSTRAP_VERSION}/dist"

cd "$(dirname "$0")"

echo "Downloading Bootstrap ${BOOTSTRAP_VERSION} to $(pwd)..."
curl -sL "${BASE_URL}/css/bootstrap.min.css" -o bootstrap.min.css
curl -sL "${BASE_URL}/js/bootstrap.bundle.min.js" -o bootstrap.bundle.min.js
echo "Done. Run 'make' in this directory to regenerate .inc files."
