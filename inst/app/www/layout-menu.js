/* Prototype: the menus on the plot toolbar: "Layout options"
   (app_layout_toolbar()) and "Check spacing" (app_check_spacing()). Shiny re-renders the toolbar with each new
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
    syncShift();
    if (!focus) return;
    if (open) {
      var first = panel.querySelector("select, input, button");
      if (first) first.focus();
    } else toggle.focus();
  }
  // The plot moves down by the height of the open layout options, so they
  // never cover its title. The image is moved, not resized (layout-images.js
  // measures it where it rests), so the plot is not drawn again.
  function syncShift() {
    document.querySelectorAll("#fieldhub-app .fieldhub-plot-panel").forEach(function(plot) {
      var open = plot.querySelector(".fh-view-bar .fh-view-menu-panel:not(.fh-spacing):not([hidden])");
      if (open && open.offsetHeight) plot.style.setProperty("--fh-menu-shift", (open.offsetHeight + 10) + "px");
      else plot.style.removeProperty("--fh-menu-shift");
    });
  }
  // Shiny draws the toolbar again with each design: a menu that was open is gone.
  if (window.jQuery) window.jQuery(document).on("shiny:value", function() { window.setTimeout(syncShift, 0); });
  window.addEventListener("resize", syncShift);

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

  // A select in a panel that shows one of its sections (the location of the
  // check spacing).
  document.addEventListener("change", function(event) {
    var choice = event.target;
    if (!choice.hasAttribute || !choice.hasAttribute("data-fh-switch")) return;
    var panel = choice.closest(".fh-view-menu-panel");
    if (!panel) return;
    panel.querySelectorAll("[data-fh-section]").forEach(function(section) {
      section.hidden = section.getAttribute("data-fh-section") !== choice.value;
    });
  });

  // Tabbing out of the menu closes it.
  document.addEventListener("focusin", function(event) {
    closeOthers(menuOf(event.target));
  });
}());
