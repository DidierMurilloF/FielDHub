(function() {
  "use strict";

  // Report activity without changing states owned by individual controls.
  function report(busy, message) {
    var root = document.getElementById("fieldhub-app");
    var status = document.getElementById("fieldhub-status");
    if (root) root.setAttribute("aria-busy", busy ? "true" : "false");
    if (status) status.textContent = message;
  }

  $(document).on("shiny:busy", function() {
    report(true, "Working…");
  });
  $(document).on("shiny:idle", function() {
    report(false, "");
  });
  $(document).on("shiny:disconnected", function() {
    report(false, "Connection lost. Reconnect before continuing.");
  });
  $(document).on("shiny:connected", function() {
    report(false, "");
  });
}());
