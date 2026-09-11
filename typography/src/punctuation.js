export function smartPunctuation(text) {
  return text
    .replace(/---/g, "\u2014")
    .replace(/--/g, "\u2013")
    .replace(/\.\.\./g, "\u2026")
    .replace(/'/g, "\u2019")
    .replace(/"([^"]*)"/g, "\u201C$1\u201D");
}

export function applySmartPunctuation(root) {
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  const nodes = [];
  let node = walker.nextNode();
  while (node) {
    const parent = node.parentElement;
    if (parent && !parent.closest("code, pre")) {
      nodes.push(node);
    }
    node = walker.nextNode();
  }

  for (const textNode of nodes) {
    const next = smartPunctuation(textNode.nodeValue);
    if (next !== textNode.nodeValue) textNode.nodeValue = next;
  }
}
