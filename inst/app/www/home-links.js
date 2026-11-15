// Links on the Welcome page that open another tab of the app's navbar:
// <a data-fieldhub-tab="Augmented RCBD"> shows the tab whose data-value
// matches, as if it were picked from its menu.
(function() {
  "use strict";

  document.addEventListener("click", function(event) {
    var link = event.target.closest && event.target.closest("#fieldhub-app [data-fieldhub-tab]");
    if (!link || !window.jQuery) return;
    var value = link.getAttribute("data-fieldhub-tab");
    var tabs = document.querySelectorAll('#fieldhub-app .navbar a[data-toggle="tab"]');
    var tab = null;
    for (var i = 0; i < tabs.length; i++) {
      if (tabs[i].getAttribute("data-value") === value) { tab = tabs[i]; break; }
    }
    if (!tab) return;
    event.preventDefault();
    window.jQuery(tab).tab("show");
    window.scrollTo(0, 0);
  });
})();
