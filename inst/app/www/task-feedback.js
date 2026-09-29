(function() {
  "use strict";

  var tasks = Object.create(null);

  function updateRegion(region) {
    var active = region.getAttribute("data-fieldhub-tasks").split(" ").find(function(id) {
      return Object.prototype.hasOwnProperty.call(tasks, id);
    });
    var busy = Boolean(active);
    var content = region.querySelector(".fieldhub-task-content");
    var feedback = region.querySelector(".fieldhub-task-feedback");
    region.classList.toggle("is-busy", busy);
    content.inert = busy;
    content.setAttribute("aria-busy", String(busy));
    if (busy) content.setAttribute("aria-hidden", "true");
    else content.removeAttribute("aria-hidden");
    feedback.querySelector(".fieldhub-task-message").textContent = busy ? tasks[active] : "";
    feedback.hidden = !busy;
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

  $(document).on("shiny:disconnected", function() {
    tasks = Object.create(null);
    document.querySelectorAll(".fieldhub-task-region").forEach(updateRegion);
    // The app-wide connection notice explains why work is no longer proceeding.
  });
}());
