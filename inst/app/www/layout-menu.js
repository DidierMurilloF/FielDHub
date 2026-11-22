/* Prototype: the "Layout options" menu on the plot toolbar
   (app_layout_toolbar()). Shiny re-renders the toolbar with each new
   design, so every handler is delegated from the document. */
(function() {
  "use strict";

  function panelOf(toggle) {
    return document.getElementById(toggle.getAttribute("aria-controls"));
  }
  function setOpen(toggle, open, focus) {
    var panel = panelOf(toggle);
    if (!panel) return;
    panel.hidden = !open;
    toggle.setAttribute("aria-expanded", String(open));
    if (!focus) return;
    if (open) {
      var first = panel.querySelector("select, input, button");
      if (first) first.focus();
    } else toggle.focus();
  }
  function closeOthers(keep) {
    document.querySelectorAll('.fh-view-menu-toggle[aria-expanded="true"]').forEach(function(toggle) {
      if (toggle.parentElement !== keep) setOpen(toggle, false, false);
    });
  }
  function menuOf(target) {
    return target && target.closest ? target.closest(".fh-view-menu") : null;
  }

  document.addEventListener("click", function(event) {
    var toggle = event.target.closest ? event.target.closest(".fh-view-menu-toggle") : null;
    if (toggle) {
      var open = toggle.getAttribute("aria-expanded") !== "true";
      closeOthers(toggle.parentElement);
      // Opened with the keyboard (no pointer position): move into the menu.
      setOpen(toggle, open, open && event.detail === 0);
      return;
    }
    // A click anywhere else closes the menu; a click inside keeps it.
    closeOthers(menuOf(event.target));
  });

  document.addEventListener("keydown", function(event) {
    var menu = menuOf(event.target);
    if (!menu) return;
    var toggle = menu.querySelector(".fh-view-menu-toggle");
    if (event.key === "Escape" && toggle.getAttribute("aria-expanded") === "true") {
      event.preventDefault();
      event.stopPropagation();
      setOpen(toggle, false, true);
    } else if (event.key === "ArrowDown" && event.target === toggle) {
      event.preventDefault();
      setOpen(toggle, true, true);
    }
  });

  // Tabbing out of the menu closes it.
  document.addEventListener("focusin", function(event) {
    closeOthers(menuOf(event.target));
  });
}());
