const LEFT = {
  '"': 0.1,
  "'": 0.1,
  "\u201C": 0.15,
  "\u2018": 0.1,
  "«": 0.15,
  "»": 0.15,
  "‹": 0.15,
  "(": 0.1,
  "[": 0.1,
  "-": 0.2,
  "\u2013": 0.15,
  "\u2014": 0.15,
};

const RIGHT = {
  ",": 0.1,
  ".": 0.15,
  "!": 0.1,
  "?": 0.15,
  ":": 0.1,
  ";": 0.1,
  '"': 0.1,
  "'": 0.1,
  "\u201D": 0.15,
  "\u2019": 0.1,
  "»": 0.15,
  "›": 0.15,
  ")": 0.1,
  "]": 0.1,
  "-": 0.2,
  "\u2013": 0.1,
  "\u2014": 0.15,
  "\u2026": 0.15,
};

export function protrusionFor(char, side) {
  const table = side === "left" ? LEFT : RIGHT;
  return table[char] ?? 0;
}

function wrapRange(textNode, start, length, className) {
  const node = textNode.splitText(start);
  node.splitText(length);
  const span = document.createElement("span");
  span.className = className;
  node.parentNode.insertBefore(span, node);
  span.appendChild(node);
  return span;
}

function wrapLeft(textNode, offset, char, amount) {
  const span = wrapRange(textNode, offset, char.length, "protrude-left");
  span.style.marginLeft = `-${amount}em`;
}

function wrapRight(textNode, offset, char, amount) {
  const span = wrapRange(textNode, offset, char.length, "protrude-right");
  span.style.marginRight = `-${amount}em`;
}

function collectLines(paragraph) {
  const lines = [];
  let nodes = [];
  const walker = document.createTreeWalker(
    paragraph,
    NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT,
  );

  let node = walker.nextNode();
  while (node) {
    if (node.nodeType === Node.ELEMENT_NODE && node.tagName === "BR") {
      if (nodes.length > 0) lines.push(nodes);
      nodes = [];
    } else if (node.nodeType === Node.TEXT_NODE && node.nodeValue.length > 0) {
      nodes.push(node);
    }
    node = walker.nextNode();
  }

  if (nodes.length > 0) lines.push(nodes);
  return lines;
}

function findLastChar(nodes) {
  for (let i = nodes.length - 1; i >= 0; i -= 1) {
    const text = nodes[i].nodeValue;
    for (let j = text.length - 1; j >= 0; j -= 1) {
      if (!/\s/.test(text[j])) return { node: nodes[i], offset: j, char: text[j] };
    }
  }
  return null;
}

function findFirstChar(nodes) {
  for (let i = 0; i < nodes.length; i += 1) {
    const text = nodes[i].nodeValue;
    for (let j = 0; j < text.length; j += 1) {
      if (!/\s/.test(text[j])) return { node: nodes[i], offset: j, char: text[j] };
    }
  }
  return null;
}

export function applyProtrusion(root) {
  const paragraphs = root.matches?.("p") ? [root] : root.querySelectorAll("p");
  for (const paragraph of paragraphs) {
    for (const nodes of collectLines(paragraph)) {
      const first = findFirstChar(nodes);
      if (!first) continue;

      const last = findLastChar(nodes);
      if (last) {
        const rightAmount = protrusionFor(last.char, "right");
        if (rightAmount > 0) wrapRight(last.node, last.offset, last.char, rightAmount);
      }

      const firstStillThere =
        first.node.nodeValue.length > first.offset &&
        first.node.nodeValue.charAt(first.offset) === first.char;
      const leftAmount = protrusionFor(first.char, "left");
      if (leftAmount > 0 && firstStillThere) {
        wrapLeft(first.node, first.offset, first.char, leftAmount);
      }
    }
  }
}

export function clearProtrusion(root) {
  for (const span of root.querySelectorAll(".protrude-left, .protrude-right")) {
    const parent = span.parentNode;
    while (span.firstChild) parent.insertBefore(span.firstChild, span);
    parent.removeChild(span);
  }
  root.normalize();
}
