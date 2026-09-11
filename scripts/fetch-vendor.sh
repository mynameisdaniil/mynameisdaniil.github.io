#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

mkdir -p assets/vendor

cp node_modules/js-yaml/dist/js-yaml.min.js assets/vendor/js-yaml.min.js
cp node_modules/marked/lib/marked.umd.js assets/vendor/marked.min.js

echo "vendor assets written (fonts are built by 'npm run build:fonts')"
