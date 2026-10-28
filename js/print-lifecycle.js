export function installPrintLifecycle({ win, root, clearProtrusion, unjustify, typesetter }) {
  win.addEventListener("beforeprint", () => {
    clearProtrusion(root);
    unjustify([...root.querySelectorAll("p")]);
  });

  win.addEventListener("afterprint", () => {
    const printMedia = win.matchMedia("print");
    if (printMedia.matches) {
      printMedia.addEventListener(
        "change",
        (event) => {
          if (!event.matches) typesetter?.refresh();
        },
        { once: true },
      );
    } else {
      typesetter?.refresh();
    }
  });
}
