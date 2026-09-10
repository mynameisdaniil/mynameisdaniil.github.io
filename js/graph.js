import { setCardState } from "./card.js";

function forceBounds(padding) {
  let nodes = [];
  let width = 0;
  let height = 0;

  function force(alpha) {
    for (const node of nodes) {
      const r = node.radius + padding;
      if (node.x < r) node.vx += (r - node.x) * alpha * 0.4;
      if (node.x > width - r) node.vx -= (node.x - (width - r)) * alpha * 0.4;
      if (node.y < r) node.vy += (r - node.y) * alpha * 0.4;
      if (node.y > height - r) node.vy -= (node.y - (height - r)) * alpha * 0.4;
    }
  }

  force.initialize = (n) => {
    nodes = n;
  };

  force.size = ([w, h]) => {
    width = w;
    height = h;
    return force;
  };

  return force;
}

export function createGraph({ stage, svg, cardsById, edges, win = window }) {
  const nodeById = new Map();
  const nodes = [...cardsById.entries()].map(([id, el]) => {
    const node = { id, el, x: 0, y: 0, w: 0, h: 0, radius: 60, focused: false };
    nodeById.set(id, node);
    return node;
  });

  const linkData = edges.map((edge) => ({ source: edge.source, target: edge.target }));

  let width = stage.clientWidth || win.innerWidth;
  let height = stage.clientHeight || win.innerHeight;

  const svgNS = "http://www.w3.org/2000/svg";
  const lineById = new Map();
  for (const edge of linkData) {
    const line = document.createElementNS(svgNS, "line");
    line.setAttribute("class", "edge");
    line.dataset.source = edge.source;
    line.dataset.target = edge.target;
    svg.appendChild(line);
    lineById.set(`${edge.source}->${edge.target}`, line);
  }

  const reducedMotion = win.matchMedia?.("(prefers-reduced-motion: reduce)")?.matches ?? false;
  const boundary = forceBounds(48);

  function linkDistance(link) {
    return link.source.focused || link.target.focused ? 240 : 170;
  }

  const simulation = d3
    .forceSimulation(nodes)
    .force(
      "link",
      d3
        .forceLink(linkData)
        .id((d) => d.id)
        .distance(linkDistance)
        .strength(0.25),
    )
    .force("charge", d3.forceManyBody().strength(-420))
    .force(
      "collide",
      d3
        .forceCollide()
        .radius((d) => d.radius + 12)
        .strength(0.9),
    )
    .force("x", d3.forceX(() => width / 2).strength((d) => (d.focused ? 0.14 : 0.015)))
    .force("y", d3.forceY(() => height / 2).strength((d) => (d.focused ? 0.14 : 0.015)))
    .force("boundary", boundary.size([width, height]))
    .alphaDecay(0.05)
    .on("tick", tick);

  function measure(node) {
    node.w = node.el.offsetWidth || 200;
    node.h = node.el.offsetHeight || 80;
    const scale = node.focused ? 1 : 0.6;
    node.radius = (Math.max(node.w, node.h) / 2) * scale;
  }

  function tick() {
    for (const node of nodes) {
      node.el.style.transform = `translate(${node.x - node.w / 2}px, ${node.y - node.h / 2}px)`;
    }
    for (const link of linkData) {
      const line = lineById.get(`${link.source.id}->${link.target.id}`);
      if (!line) continue;
      line.setAttribute("x1", link.source.x);
      line.setAttribute("y1", link.source.y);
      line.setAttribute("x2", link.target.x);
      line.setAttribute("y2", link.target.y);
    }
  }

  function focusCard(id) {
    const target = nodeById.get(id);
    if (!target) return;

    for (const node of nodes) {
      node.focused = node.id === id;
      setCardState(node.el, node.focused ? "focused" : "background");
    }
    for (const node of nodes) measure(node);

    for (const line of lineById.values()) {
      const active = line.dataset.source === id || line.dataset.target === id;
      line.classList.toggle("edge--active", active);
    }

    if (reducedMotion) {
      for (let i = 0; i < 120; i += 1) simulation.tick();
      tick();
    } else {
      simulation.alpha(0.6).restart();
    }
  }

  function resize() {
    width = stage.clientWidth || win.innerWidth;
    height = stage.clientHeight || win.innerHeight;
    boundary.size([width, height]);
    simulation.force("x").x(() => width / 2);
    simulation.force("y").y(() => height / 2);
    for (const node of nodes) measure(node);
    if (reducedMotion) {
      tick();
    } else {
      simulation.alpha(0.2).restart();
    }
  }

  const observer = new ResizeObserver(() => resize());
  observer.observe(stage);

  return {
    focusCard,
    resize,
    destroy() {
      observer.disconnect();
      simulation.stop();
    },
  };
}
