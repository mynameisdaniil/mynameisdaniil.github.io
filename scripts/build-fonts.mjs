import { readFile, writeFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { compress } from "wawoff2";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const fontsDir = join(root, "assets", "fonts");

const localFonts = {
  "latin-modern-roman-regular.woff2":
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-Regular.ttf",
  "latin-modern-roman-italic.woff2":
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-Italic.ttf",
  "latin-modern-roman-bold.woff2":
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-Bold.ttf",
  "latin-modern-roman-bolditalic.woff2":
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernRoman-BoldItalic.ttf",
  "latin-modern-mono-regular.woff2":
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernMono-Regular.ttf",
  "latin-modern-mono-italic.woff2":
    "node_modules/@typopro/web-latin-modern/TypoPRO-LatinModernMono-Italic.ttf",
};

const gostUrl =
  "https://github.com/Metrolog/Font.GOST2.304-81/releases/download/v0.7.6/GOST2.304-81TypeA-Regular.ttf";

await mkdir(fontsDir, { recursive: true });

for (const [output, source] of Object.entries(localFonts)) {
  const ttf = await readFile(join(root, source));
  const woff2 = Buffer.from(await compress(ttf));
  await writeFile(join(fontsDir, output), woff2);
  console.log(`${output}: ${woff2.length} bytes`);
}

const response = await fetch(gostUrl);
if (!response.ok) {
  throw new Error(`Failed to download ${gostUrl}: HTTP ${response.status}`);
}
const gostTtf = Buffer.from(await response.arrayBuffer());
const gostWoff2 = Buffer.from(await compress(gostTtf));
await writeFile(join(fontsDir, "gost-type-a-regular.woff2"), gostWoff2);
console.log(`gost-type-a-regular.woff2: ${gostWoff2.length} bytes`);
