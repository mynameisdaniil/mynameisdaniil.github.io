import { mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { Jimp } from "jimp";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const outputDir = join(root, "assets");
const source = join(root, "favicon.png");

await mkdir(outputDir, { recursive: true });

const image = await Jimp.read(source);

async function write(size, name) {
  const resized = image.clone().resize({ w: size, h: size });
  await resized.write(join(outputDir, name));
  console.log(`${name}: ${size}x${size}`);
}

await write(32, "favicon-32.png");
await write(180, "apple-touch-icon.png");
