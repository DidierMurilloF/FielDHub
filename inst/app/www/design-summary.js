/* Prototype: the Summary tab (app_design_summary()): tooltips, copying the
   seed, and opening another result tab from a button. */
(function() {
  "use strict";

  function announce(text) {
    var status = document.getElementById("fieldhub-status");
    if (status) status.textContent = text;
  }
  // Without the clipboard API (an app served over plain http).
  function legacyCopy(text) {
    return new Promise(function(resolve, reject) {
      var field = document.createElement("textarea");
      field.value = text;
      field.setAttribute("readonly", "");
      field.className = "fh-clipboard";
      document.body.appendChild(field);
      field.select();
      var done = false;
      try { done = document.execCommand("copy"); } catch (e) {}
      document.body.removeChild(field);
      if (done) resolve(); else reject();
    });
  }
  function copy(text) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text).catch(function() { return legacyCopy(text); });
    }
    return legacyCopy(text);
  }
  document.addEventListener("click", function(event) {
    if (!event.target.closest) return;
    var button = event.target.closest("[data-fh-copy]");
    if (button) {
      copy(button.getAttribute("data-fh-copy")).then(function() {
        button.classList.add("is-copied");
        announce("Seed copied");
        window.setTimeout(function() {
          button.classList.remove("is-copied");
          announce("");
        }, 1800);
      }, function() {
        announce("Copy is not available here. Select the seed and copy it.");
      });
      return;
    }
    // Open a tab of this page's own result tabs, named by their id.
    var open = event.target.closest("[data-fh-tabset]");
    if (open) {
      var tabs = document.getElementById(open.getAttribute("data-fh-tabset"));
      var name = open.getAttribute("data-fh-tab");
      var link = tabs && [].find.call(tabs.querySelectorAll("a[data-value]"), function(a) {
        return a.getAttribute("data-value") === name;
      });
      if (link) { $(link).tab("show"); link.focus(); }
    }
  });

  $(function() {
    // Hover and keyboard focus both open a tooltip; Shiny inserts the terms later.
    if ($.fn.tooltip) {
      $(document.body).tooltip({selector: "[data-fh-tip]", container: "body", placement: "top",
                                trigger: "hover focus"});
    }
  });
}());
