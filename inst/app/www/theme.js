/* Prototype: light/dark theme toggle in the navbar. The theme itself is set
   before first paint by the inline script in app_add_external_resources(). */
(function() {
  "use strict";
  var KEY = "fieldhub-theme";
  var root = document.documentElement;
  var SUN = '<svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M2 12h2M20 12h2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/></svg>';
  var MOON = '<svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M20 14.5A8 8 0 0 1 9.5 4a8 8 0 1 0 10.5 10.5z"/></svg>';

  function stored() {
    try { return window.localStorage.getItem(KEY); } catch (e) { return null; }
  }
  function current() {
    return root.getAttribute("data-theme") === "dark" ? "dark" : "light";
  }
  function paint(button) {
    var dark = current() === "dark";
    // The icon shows the theme a click switches to.
    button.innerHTML = dark ? SUN : MOON;
    button.setAttribute("aria-label", dark ? "Switch to light theme" : "Switch to dark theme");
    button.setAttribute("title", dark ? "Light theme" : "Dark theme");
    button.setAttribute("aria-pressed", String(dark));
  }
  // A saved choice replaces a ?theme= or ?look= in the address.
  function forget(name) {
    if (!window.history.replaceState || !window.URL) return;
    var url = new URL(window.location.href);
    if (!url.searchParams.has(name)) return;
    url.searchParams.delete(name);
    window.history.replaceState(null, "", url);
  }
  function set(theme, button, save) {
    root.setAttribute("data-theme", theme);
    if (save) {
      try { window.localStorage.setItem(KEY, theme); } catch (e) {}
      forget("theme");
    }
    if (button) paint(button);
  }
  // Prototype only: switch between the looks (design.css, look-b.css, look-c.css, look-d.css).
  function lookSwitch() {
    var names = {a: "Option A: neutral", b: "Option B: cards", c: "Option C: compact",
                 d: "Option D: A with a slim navbar"};
    var group = document.createElement("div");
    group.className = "fh-look-switch";
    group.setAttribute("role", "group");
    group.setAttribute("aria-label", "Prototype look");
    var paintLooks = function() {
      var look = root.getAttribute("data-look") || "a";
      group.querySelectorAll("button").forEach(function(button) {
        button.setAttribute("aria-pressed", String(button.value === look));
      });
    };
    Object.keys(names).forEach(function(look) {
      var button = document.createElement("button");
      button.type = "button";
      button.value = look;
      button.textContent = look.toUpperCase();
      button.title = names[look];
      button.setAttribute("aria-label", names[look]);
      button.addEventListener("click", function() {
        root.setAttribute("data-look", look);
        try { window.localStorage.setItem("fieldhub-look", look); } catch (e) {}
        forget("look");
        paintLooks();
        // Plots are sized to their panel, which each look frames differently.
        window.dispatchEvent(new Event("resize"));
      });
      group.appendChild(button);
    });
    paintLooks();
    return group;
  }

  $(function() {
    var navbar = document.querySelector("#fieldhub-app .navbar .container-fluid");
    if (!navbar || navbar.querySelector(".fh-theme-toggle")) return;
    var button = document.createElement("button");
    button.type = "button";
    button.className = "fh-theme-toggle";
    paint(button);
    button.addEventListener("click", function() {
      set(current() === "dark" ? "light" : "dark", button, true);
    });
    navbar.appendChild(button);
    navbar.appendChild(lookSwitch());
    // Follow the system only until the user has chosen.
    if (window.matchMedia) {
      var media = window.matchMedia("(prefers-color-scheme: dark)");
      var follow = function(event) {
        if (!stored()) set(event.matches ? "dark" : "light", button, false);
      };
      if (media.addEventListener) media.addEventListener("change", follow);
    }
  });
}());
