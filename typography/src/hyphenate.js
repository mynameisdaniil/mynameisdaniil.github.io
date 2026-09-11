import * as texLinebreak from "tex-linebreak";
import enUs from "hyphenation.en-us";

const { createHyphenator } = texLinebreak.default || texLinebreak;

export const patterns = { "en-us": enUs };

export function registerPatterns(extra) {
  Object.assign(patterns, extra);
}

export function hyphenateFor(lang) {
  return createHyphenator(patterns[lang] || patterns["en-us"]);
}

export { createHyphenator };
