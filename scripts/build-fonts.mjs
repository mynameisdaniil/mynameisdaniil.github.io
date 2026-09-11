import { readFile, writeFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import subsetFont from "subset-font";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const fontsDir = join(root, "assets", "fonts");

function charsFor(ranges) {
  return ranges
    .flatMap(([first, last]) =>
      Array.from({ length: last - first + 1 }, (_, index) => first + index),
    )
    .map((codePoint) => String.fromCodePoint(codePoint))
    .join("");
}

const LATIN = charsFor([
  [0x20, 0x7e],
  [0xa0, 0x17f],
  [0x2010, 0x2027],
  [0x2030, 0x205e],
  [0x20ac, 0x20ac],
]);
const CYRILLIC = charsFor([
  [0x400, 0x4ff],
  [0x2116, 0x2116],
]);

const localFonts = {
  "latin-modern-roman-regular.woff2": [
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-Regular.ttf",
    LATIN,
  ],
  "latin-modern-roman-italic.woff2": [
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-Italic.ttf",
    LATIN,
  ],
  "latin-modern-roman-bold.woff2": [
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-Bold.ttf",
    LATIN,
  ],
  "latin-modern-roman-bolditalic.woff2": [
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-BoldItalic.ttf",
    LATIN,
  ],
  "latin-modern-mono-regular.woff2": [
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernMono-Regular.ttf",
    LATIN,
  ],
  "latin-modern-mono-italic.woff2": [
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernMono-Italic.ttf",
    LATIN,
  ],
};

const gostUrl =
  "https://github.com/Metrolog/Font.GOST2.304-81/releases/download/v0.7.6/GOST2.304-81TypeA-Regular.ttf";

async function build(output, ttf, characters) {
  const subset = Buffer.from(
    await subsetFont(ttf, characters, { targetFormat: "woff2" }),
  );
  await writeFile(join(fontsDir, output), subset);
  console.log(`${output}: ${subset.length} bytes`);
}

await mkdir(fontsDir, { recursive: true });

for (const [output, [source, characters]] of Object.entries(localFonts)) {
  await build(output, await readFile(join(root, source)), characters);
}

const response = await fetch(gostUrl);
if (!response.ok) {
  throw new Error(`Failed to download ${gostUrl}: HTTP ${response.status}`);
}
await build("gost-type-a-regular.woff2", Buffer.from(await response.arrayBuffer()), LATIN + CYRILLIC);
