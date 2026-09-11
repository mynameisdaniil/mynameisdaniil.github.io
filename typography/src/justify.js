import * as texLinebreak from "tex-linebreak";

const { justifyContent, unjustifyContent } = texLinebreak.default || texLinebreak;

export function justify(elements, hyphenate) {
  justifyContent(elements, hyphenate);
}

export function unjustify(elements) {
  const list = Array.isArray(elements) ? elements : [elements];
  for (const el of list) {
    unjustifyContent(el);
    el.style.whiteSpace = "";
  }
}
