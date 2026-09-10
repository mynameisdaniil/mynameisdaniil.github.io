import {
  createHyphenator,
  justifyContent,
  unjustifyContent,
} from "tex-linebreak";
import enUs from "hyphenation.en-us";
import ru from "hyphenation.ru";

const patterns = { "en-us": enUs, ru };

export function hyphenateFor(lang) {
  return createHyphenator(patterns[lang] || patterns["en-us"]);
}

export { patterns, justifyContent, unjustifyContent };
