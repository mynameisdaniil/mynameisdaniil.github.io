import texLinebreak from "tex-linebreak";

const { justifyContent, unjustifyContent } = texLinebreak;

export function justify(elements, hyphenate) {
  justifyContent(elements, hyphenate);
}

export function unjustify(elements) {
  unjustifyContent(elements);
}
