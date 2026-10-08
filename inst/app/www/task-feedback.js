(function() {
  "use strict";

  var tasks = Object.create(null);
  var states = new WeakMap();
  var disconnected = false;

  function stateFor(region) {
    if (!states.has(region)) states.set(region, {
      visible: false, showTimer: null, hideTimer: null, message: "Loading results..."
    });
    return states.get(region);
  }

  function paintRegion(region, state, busy) {
    state.visible = busy;
    var content = region.querySelector(".fieldhub-task-content");
    var feedback = region.querySelector(".fieldhub-task-feedback");
    region.classList.toggle("is-busy", busy);
    content.inert = busy;
    content.setAttribute("aria-busy", String(busy));
    if (busy) content.setAttribute("aria-hidden", "true");
    else content.removeAttribute("aria-hidden");
    feedback.querySelector(".fieldhub-task-message").textContent = busy ? state.message : "";
    feedback.hidden = !busy;
    if (!busy) state.message = "Loading results...";
  }

  function updateRegion(region) {
    var state = stateFor(region);
    var active = region.getAttribute("data-fieldhub-tasks").split(" ").find(function(id) {
      return Object.prototype.hasOwnProperty.call(tasks, id);
    });
    var busy = !disconnected && Boolean(active || region.querySelector(".fieldhub-output-loader.is-loading"));
    if (busy) {
      window.clearTimeout(state.hideTimer);
      state.hideTimer = null;
      if (active) state.message = tasks[active];
      if (active || state.visible) {
        window.clearTimeout(state.showTimer);
        state.showTimer = null;
        paintRegion(region, state, true);
      } else if (state.showTimer === null) {
        // Fast output-only updates keep their existing content without flashing.
        state.showTimer = window.setTimeout(function() {
          state.showTimer = null;
          paintRegion(region, state, true);
        }, 120);
      }
    } else {
      window.clearTimeout(state.showTimer);
      state.showTimer = null;
      if (disconnected) {
        window.clearTimeout(state.hideTimer);
        state.hideTimer = null;
        paintRegion(region, state, false);
      } else if (state.visible && state.hideTimer === null) {
        // Bridge task completion, Shiny delivery and image/table painting with
        // the same indicator, rather than handing off to another spinner.
        state.hideTimer = window.setTimeout(function() {
          state.hideTimer = null;
          paintRegion(region, state, false);
        }, 160);
      }
    }
  }

  function showFeedback(id, busy, message) {
    if (busy) tasks[id] = message || "Preparing your results...";
    else delete tasks[id];
    document.querySelectorAll(".fieldhub-task-region").forEach(updateRegion);
  }

  // Paint before even synchronous R work starts. Output-recalculation events
  // alone cannot describe an asynchronous job waiting for a result or worker.
  $(document).on("click", "[data-fieldhub-task]", function() {
    if (this.disabled || this.classList.contains("disabled")) return;
    showFeedback(this.getAttribute("data-fieldhub-task"), true,
      this.getAttribute("data-fieldhub-message") || "Preparing your results...");
  });

  $(function() {
    Shiny.addCustomMessageHandler("fieldhub-task-feedback", function(data) {
      showFeedback(data.id, data.busy === true, data.message || "");
    });
  });

  $(document).on("fieldhub:output-feedback", function(event) {
    var region = event.target.closest(".fieldhub-task-region");
    if (region) updateRegion(region);
  });

  $(document).on("shiny:disconnected", function() {
    disconnected = true;
    tasks = Object.create(null);
    document.querySelectorAll(".fieldhub-task-region").forEach(updateRegion);
    // The app-wide connection notice explains why work is no longer proceeding.
  });
  $(document).on("shiny:connected", function() {
    disconnected = false;
  });
}());
