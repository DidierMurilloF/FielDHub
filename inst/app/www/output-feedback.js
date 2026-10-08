(function() {
  "use strict";

  var states = new WeakMap();

  function stateFor(loader) {
    if (!states.has(loader)) states.set(loader, { generation: 0, output: false, table: false });
    return states.get(loader);
  }

  function setBusy(loader, busy) {
    loader.classList.toggle("is-loading", busy);
    loader.setAttribute("aria-busy", String(busy));
    var content = loader.querySelector(".fieldhub-output-content");
    var region = loader.closest(".fieldhub-task-region");
    // Inside a result tab, one region owns both task and output feedback.
    // Keep fast output updates visible until that region's debounce expires.
    content.inert = busy && !region;
    if (busy && !region) content.setAttribute("aria-hidden", "true");
    else content.removeAttribute("aria-hidden");
    loader.querySelector(".fieldhub-output-feedback").hidden = !busy || Boolean(region);
    if (region) $(loader).trigger("fieldhub:output-feedback");
  }

  $(document).on("shiny:outputinvalidated shiny:recalculating shiny:value shiny:error", function(event) {
    var output = event.target;
    if (!output.closest) return;
    var loader = output.closest(".fieldhub-output-loader");
    if (!loader) return;
    var state = stateFor(loader);
    var generation = ++state.generation;
    var busy = event.type === "shiny:outputinvalidated" || event.type === "shiny:recalculating";
    if (busy) {
      state.output = true;
      setBusy(loader, true);
      return;
    }
    // Shiny emits value/error before updating the DOM. Keep the loading state
    // through that update and, for images, until the new pixels are available.
    window.requestAnimationFrame(function() {
      if (state.generation !== generation) return;
      function finish() {
        if (state.generation === generation) {
          state.output = false;
          setBusy(loader, state.table);
        }
      }
      var image = output.querySelector("img");
      if (event.type === "shiny:value" && image && !image.complete) {
        image.addEventListener("load", finish, { once: true });
        image.addEventListener("error", finish, { once: true });
      } else finish();
    });
  });

  // Server-side table paging/filtering does not invalidate the Shiny output.
  // Its AJAX lifecycle must share the same indicator without clearing an
  // independently pending output render.
  $(document).on("processing.dt", function(event, settings, processing) {
    var loader = event.target.closest(".fieldhub-output-loader");
    if (!loader) return;
    var state = stateFor(loader);
    state.table = Boolean(processing);
    setBusy(loader, state.output || state.table);
  });

  $(document).on("shiny:disconnected", function() {
    document.querySelectorAll(".fieldhub-output-loader").forEach(function(loader) {
      var state = stateFor(loader);
      state.generation++;
      state.output = state.table = false;
      setBusy(loader, false);
    });
  });
}());
