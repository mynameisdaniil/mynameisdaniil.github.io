export function resolveTheme(stored, systemDark) {
  if (stored === "light" || stored === "dark") return stored;
  return systemDark ? "dark" : "light";
}

function safeStorage() {
  try {
    return globalThis.localStorage ?? null;
  } catch (err) {
    return null;
  }
}

function safeMatchMedia() {
  try {
    return globalThis.matchMedia("(prefers-color-scheme: dark)");
  } catch (err) {
    return { matches: false, addEventListener() {} };
  }
}

export function initTheme({ root = document.documentElement, button = null, storage, mql } = {}) {
  const store = storage !== undefined ? storage : safeStorage();
  const media = mql !== undefined ? mql : safeMatchMedia();

  function storedValue() {
    try {
      return store ? store.getItem("theme") : null;
    } catch (err) {
      return null;
    }
  }

  function apply() {
    const mode = resolveTheme(storedValue(), media.matches);
    root.classList.toggle("dark", mode === "dark");
    root.style.colorScheme = mode;
    if (button) {
      const isDark = mode === "dark";
      button.setAttribute("aria-pressed", String(isDark));
      button.setAttribute("aria-label", isDark ? "Switch to light theme" : "Switch to dark theme");
    }
  }

  function onToggle() {
    const next = root.classList.contains("dark") ? "light" : "dark";
    try {
      store?.setItem("theme", next);
    } catch (err) {}
    apply();
  }

  button?.addEventListener("click", onToggle);
  media.addEventListener?.("change", () => {
    if (!storedValue()) apply();
  });
  apply();

  return { apply };
}
