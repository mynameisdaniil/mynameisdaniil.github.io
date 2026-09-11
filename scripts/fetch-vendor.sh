#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

mkdir -p assets/vendor assets/fonts

cp node_modules/js-yaml/dist/js-yaml.min.js assets/vendor/js-yaml.min.js
cp node_modules/marked/lib/marked.umd.js assets/vendor/marked.min.js

LM=node_modules/@typopro/web-latin-modern
cp "$LM/TypoPRO-LatinModernRoman-Regular.woff" assets/fonts/latin-modern-roman-regular.woff
cp "$LM/TypoPRO-LatinModernRoman-Italic.woff" assets/fonts/latin-modern-roman-italic.woff
cp "$LM/TypoPRO-LatinModernRoman-Bold.woff" assets/fonts/latin-modern-roman-bold.woff
cp "$LM/TypoPRO-LatinModernRoman-BoldItalic.woff" assets/fonts/latin-modern-roman-bolditalic.woff
cp "$LM/TypoPRO-LatinModernMono-Regular.woff" assets/fonts/latin-modern-mono-regular.woff
cp "$LM/TypoPRO-LatinModernMono-Italic.woff" assets/fonts/latin-modern-mono-italic.woff

if [ ! -f assets/fonts/gost-type-a-regular.woff ]; then
  curl -sSL -o /tmp/gost-fonts.zip \
    https://github.com/Metrolog/Font.GOST2.304-81/releases/download/v0.7.6/fonts.zip
  unzip -j -o /tmp/gost-fonts.zip 'woff/GOST2.304-81TypeA-Regular.woff' -d assets/fonts/
  mv assets/fonts/GOST2.304-81TypeA-Regular.woff assets/fonts/gost-type-a-regular.woff
fi

echo "vendor assets written"
