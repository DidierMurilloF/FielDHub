(function() {
  "use strict";

  function showFeedback(id, busy, message) {
    var panel = document.getElementById(id + "_feedback");
    var label = document.getElementById(id + "_status");
    if (!panel || !label) return;
    label.textContent = busy ? message : "";
    panel.hidden = !busy;
    return panel;
  }

  // Paint before even synchronous R work starts. Output-recalculation events
  // alone cannot describe an asynchronous job waiting for a result or worker.
  $(document).on("click", "[data-fieldhub-task]", function() {
    if (this.disabled || this.classList.contains("disabled")) return;
    var panel = showFeedback(this.getAttribute("data-fieldhub-task"), true,
      this.getAttribute("data-fieldhub-message") || "Preparing your results...");
    // Run can be below the fold, especially when the sidebar stacks on mobile.
    if (panel) panel.scrollIntoView({block: "nearest"});
  });

  $(function() {
    Shiny.addCustomMessageHandler("fieldhub-task-feedback", function(data) {
      showFeedback(data.id, data.busy === true, data.message || "");
    });
  });

  $(document).on("shiny:disconnected", function() {
    document.querySelectorAll(".fieldhub-task-feedback").forEach(function(panel) {
      panel.hidden = true;
    });
    // The app-wide connection notice explains why work is no longer proceeding.
  });
}());
