(function() {
  "use strict";

  var timer;
  var sent = Object.create(null);
  var pending = Object.create(null);

  function plotState(event) {
    var image = event.target;
    if (!image.classList || !image.classList.contains("shiny-image-output")) return;
    var panel = image.closest("#fieldhub-app .fieldhub-plot-panel");
    if (!panel) return;
    var ready = event.type === "shiny:value" && Boolean(event.value && event.value.src);
    var toolbar = panel.querySelector(".fieldhub-plot-toolbar");
    panel.classList.toggle("has-plot", ready);
    toolbar.inert = !ready;
    toolbar.setAttribute("aria-hidden", String(!ready));
  }

  function fitImages() {
    var viewport = document.documentElement.clientHeight;
    var settling = false;
    document.querySelectorAll("#fieldhub-app .fieldhub-layout-image").forEach(function(panel) {
      if (!panel.getClientRects().length) return;
      var image = panel.querySelector(".shiny-image-output");
      var main = panel.closest('[role="main"]');
      var sidebar = main && main.parentElement.querySelector('[role="complementary"]');
      if (!image || !main || !sidebar) return;
      var rect = panel.getBoundingClientRect();
      var side = sidebar.getBoundingClientRect();
      var width = Math.max(1, Math.floor(rect.width));
      var top = rect.top + window.scrollY;
      var available = viewport - top - 24;
      // A stacked mobile sidebar may put the result below the fold.
      if (available <= 0) available = viewport - 48;
      // Keep a landscape preview instead of filling a tall sidebar or monitor.
      available = Math.min(available, width * 0.625, 700);
      if (side.right <= main.getBoundingClientRect().left + 1) {
        available = Math.min(available, side.bottom - rect.top - 4);
      }
      var height = Math.max(1, Math.floor(available));
      panel.style.setProperty("--fieldhub-preview-height", height + "px");
      var key = width + "x" + height;
      if (window.Shiny && Shiny.setInputValue && sent[image.id] !== key) {
        // Publish only after two quiet measurements agree. Showing a tab or
        // changing controls can take more than one browser layout pass.
        if (pending[image.id] !== key) {
          pending[image.id] = key;
          settling = true;
          return;
        }
        sent[image.id] = key;
        delete pending[image.id];
        // Re-draw the plot at the panel's shape; do not stretch an old bitmap.
        Shiny.setInputValue(image.id + "_size", {width: width, height: height});
      } else delete pending[image.id];
    });
    if (settling) scheduleFit();
  }

  function scheduleFit() {
    window.clearTimeout(timer);
    timer = window.setTimeout(fitImages, 100);
  }

  window.addEventListener("resize", scheduleFit);
  $(document).on("shown.bs.tab shown.bs.collapse hidden.bs.collapse", scheduleFit);
  // Image delivery changes readiness, never the measurement of that same image.
  $(document).on("shiny:value shiny:error shiny:recalculating", plotState);
  $(document).on("shiny:bound", function(event) {
    if ($(event.target).is("#fieldhub-app .shiny-image-output")) scheduleFit();
  });
  $(document).on("shiny:connected", function() {
    sent = Object.create(null);
    pending = Object.create(null);
    scheduleFit();
  });
  $(function() {
    if (!document.getElementById("fieldhub-app")) return;
    // A page scrollbar must not change every plot's width during recalculation.
    document.documentElement.classList.add("fieldhub-layout-page");
    if (window.ResizeObserver) {
      var observer = new ResizeObserver(scheduleFit);
      document.querySelectorAll('#fieldhub-app [role="complementary"], #fieldhub-app .fieldhub-task-content').forEach(function(element) {
        observer.observe(element);
      });
    }
    scheduleFit();
  });
  if (document.fonts) document.fonts.ready.then(scheduleFit);
}());
