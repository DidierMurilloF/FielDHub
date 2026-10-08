$(document).ready(function() {
  "use strict";
  var navbar = $("#fieldhub-app .navbar .container-fluid").first();
  if (navbar.length && !navbar.find(".fieldhub-navbar-logo").length) {
    navbar.append('<img class="fieldhub-navbar-logo" src="www/ndsulogo.jpg" alt="North Dakota State University" height="62" width="340">');
  }
  // Help/About footers must account for a wrapped or collapsed navigation bar.
  var root = document.getElementById("fieldhub-app");
  var bar = navbar.closest(".navbar")[0];
  if (root && bar && window.ResizeObserver) {
    var observer = new ResizeObserver(function() {
      root.style.setProperty("--fieldhub-navbar-height", bar.getBoundingClientRect().height + "px");
    });
    observer.observe(bar);
  }
});
