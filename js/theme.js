export function resolveTheme(stored, systemDark) {
  if (stored === "light" || stored === "dark") return stored;
  return systemDark ? "dark" : "light";
}

export function initTheme({
  root = document.documentElement,
  button = null,
  storage = globalThis.localStorage,
  mql = globalThis.matchMedia("(prefers-color-scheme: dark)"),
} = {}) {
  function storedValue() {
    try {
      return storage ? storage.getItem("theme") : null;
    } catch (err) {
      return null;
    }
  }

  function apply() {
    const mode = resolveTheme(storedValue(), mql.matches);
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
      storage?.setItem("theme", next);
    } catch (err) {}
    apply();
  }

  button?.addEventListener("click", onToggle);
  mql.addEventListener?.("change", () => {
    if (!storedValue()) apply();
  });
  apply();

  return { apply };
}
