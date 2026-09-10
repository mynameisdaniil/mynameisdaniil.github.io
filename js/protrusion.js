const LEFT = {
  '"': 0.1,
  "'": 0.1,
  "«": 0.15,
  "»": 0.15,
  "(": 0.1,
  "[": 0.1,
  "-": 0.2,
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
  "»": 0.15,
  ")": 0.1,
  "]": 0.1,
  "-": 0.2,
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
  let current = null;
  const walker = document.createTreeWalker(paragraph, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT);

  let node = walker.nextNode();
  while (node) {
    if (node.nodeType === Node.ELEMENT_NODE && node.tagName === "BR") {
      if (current) lines.push(current);
      current = null;
    } else if (node.nodeType === Node.TEXT_NODE && node.nodeValue.length > 0) {
      if (!current) current = { first: { node, offset: 0 }, last: null };
      current.last = { node, offset: node.nodeValue.length - 1 };
    }
    node = walker.nextNode();
  }

  if (current) lines.push(current);
  return lines;
}

export function applyProtrusion(root) {
  const paragraphs = root.matches?.("p") ? [root] : root.querySelectorAll("p");
  for (const paragraph of paragraphs) {
    for (const line of collectLines(paragraph)) {
      const firstChar = line.first.node.nodeValue.charAt(line.first.offset);

      let last = line.last;
      let lastChar = last.node.nodeValue.charAt(last.offset);
      while (lastChar !== "" && /\s/.test(lastChar)) {
        if (last.offset > 0) {
          last = { node: last.node, offset: last.offset - 1 };
        } else {
          const previous = previousTextNode(last.node);
          if (!previous) break;
          last = { node: previous, offset: previous.nodeValue.length - 1 };
        }
        lastChar = last.node.nodeValue.charAt(last.offset);
      }

      const rightAmount = protrusionFor(lastChar, "right");
      if (rightAmount > 0) wrapRight(last.node, last.offset, lastChar, rightAmount);

      const firstStillThere =
        line.first.node.nodeValue.length > line.first.offset &&
        line.first.node.nodeValue.charAt(line.first.offset) === firstChar;
      const leftAmount = protrusionFor(firstChar, "left");
      if (leftAmount > 0 && firstStillThere) {
        wrapLeft(line.first.node, line.first.offset, firstChar, leftAmount);
      }
    }
  }
}

function previousTextNode(node) {
  let current = node.previousSibling;
  while (current) {
    if (current.nodeType === Node.TEXT_NODE) return current;
    if (current.lastChild) {
      let inner = current.lastChild;
      while (inner.lastChild) inner = inner.lastChild;
      if (inner.nodeType === Node.TEXT_NODE) return inner;
    }
    current = current.previousSibling;
  }
  return null;
}

export function clearProtrusion(root) {
  for (const span of root.querySelectorAll(".protrude-left, .protrude-right")) {
    const parent = span.parentNode;
    while (span.firstChild) parent.insertBefore(span.firstChild, span);
    parent.removeChild(span);
  }
  root.normalize();
}
