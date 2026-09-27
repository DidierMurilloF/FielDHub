$(document).ready(function() {
  "use strict";
  var navbar = $("#fieldhub-app .navbar .container-fluid").first();
  if (navbar.length && !navbar.find(".fieldhub-navbar-logo").length) {
    navbar.append('<img class="fieldhub-navbar-logo" src="www/ndsulogo.jpg" alt="North Dakota State University" height="62" width="340">');
  }
});
