// Run with: node --test tests/js/*.test.cjs
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function harness() {
  const handlers = {}, messages = {}, frames = [], timers = new Map(), regions = [];
  let now = 0, timerID = 0;
  function element() {
    const classes = new Set();
    return {
      attributes: {}, inert: false, hidden: true,
      classList: {
        toggle(name, value) { if (value) classes.add(name); else classes.delete(name); },
        contains(name) { return classes.has(name); }
      },
      setAttribute(key, value) { this.attributes[key] = value; },
      getAttribute(key) { return this.attributes[key]; },
      removeAttribute(key) { delete this.attributes[key]; }
    };
  }
  function emit(type, target, ...args) {
    for (const callback of handlers[type] || []) callback.call(target, { type, target }, ...args);
  }
  const document = {
    querySelectorAll(selector) {
      return selector === ".fieldhub-task-region" ? regions : regions.flatMap(region => region.loaders);
    }
  };
  const context = vm.createContext({
    document,
    window: {
      setTimeout(callback, delay) { timers.set(++timerID, { callback, at: now + delay }); return timerID; },
      clearTimeout(id) { timers.delete(id); },
      requestAnimationFrame(callback) { frames.push(callback); }
    },
    Shiny: { addCustomMessageHandler(type, callback) { messages[type] = callback; } },
    $(target) {
      if (typeof target === "function") { target(); return; }
      return {
        on(events, selector, callback) {
          const fn = callback || selector;
          for (const type of events.split(" ")) (handlers[type] ||= []).push(fn);
        },
        trigger(type) { emit(type, target); }
      };
    }
  });
  for (const file of ["task-feedback.js", "output-feedback.js"]) {
    vm.runInContext(fs.readFileSync(path.join(__dirname, "../../inst/app/www", file), "utf8"), context);
  }
  return {
    region(tasks, count = 1) {
      const region = element(), content = element(), feedback = element(), message = { textContent: "" };
      region.setAttribute("data-fieldhub-tasks", tasks);
      feedback.querySelector = () => message;
      region.content = content;
      region.feedback = feedback;
      region.message = message;
      region.loaders = Array.from({ length: count }, () => {
        const loader = element(), outputContent = element(), outputFeedback = element();
        loader.closest = () => region;
        loader.querySelector = selector => selector === ".fieldhub-output-content" ? outputContent : outputFeedback;
        loader.feedback = outputFeedback;
        loader.output = { closest: () => loader, querySelector: () => null };
        return loader;
      });
      region.querySelector = selector => {
        if (selector === ".fieldhub-task-content") return content;
        if (selector === ".fieldhub-task-feedback") return feedback;
        return region.loaders.find(loader => loader.classList.contains("is-loading")) || null;
      };
      regions.push(region);
      return region;
    },
    task(id, busy, message = "Randomizing design...") { messages["fieldhub-task-feedback"]({ id, busy, message }); },
    click(id, disabled = false) {
      const button = element();
      button.disabled = disabled;
      button.setAttribute("data-fieldhub-task", id);
      emit("click", button);
    },
    event(type, loader) { emit(type, loader.output); },
    table(loader, busy) { emit("processing.dt", loader.output, {}, busy); },
    disconnect() { emit("shiny:disconnected", document); },
    reconnect() { emit("shiny:connected", document); },
    paint() { while (frames.length) frames.shift()(); },
    advance(delay) {
      const end = now + delay;
      while (true) {
        const next = [...timers].filter(([, timer]) => timer.at <= end).sort((a, b) => a[1].at - b[1].at)[0];
        if (!next) break;
        now = next[1].at;
        timers.delete(next[0]);
        next[1].callback();
      }
      now = end;
    }
  };
}

test("Run shows one tab indicator immediately, without a second output spinner", () => {
  const h = harness(), region = h.region("prep-run prep-randomize");
  h.click("prep-run");
  assert.equal(region.feedback.hidden, false);
  assert.equal(region.content.inert, true);
  h.event("shiny:recalculating", region.loaders[0]);
  assert.equal(region.loaders[0].feedback.hidden, true);
  h.task("prep-run", true, "Optimizing allocation...");
  assert.equal(region.message.textContent, "Optimizing allocation...");
});

test("task completion and late output delivery never swap or blink the indicator", () => {
  const h = harness(), region = h.region("prep-run"), loader = region.loaders[0];
  h.task("prep-run", true);
  h.task("prep-run", false);
  h.advance(100);
  h.event("shiny:recalculating", loader);
  h.advance(200);
  assert.equal(region.feedback.hidden, false);
  assert.equal(region.message.textContent, "Randomizing design...");
  h.event("shiny:value", loader);
  h.paint();
  assert.equal(region.feedback.hidden, false);
  h.advance(160);
  assert.equal(region.feedback.hidden, true);
  assert.equal(region.content.inert, false);
});

test("fast output/table updates do not hide content or flash a spinner", () => {
  const h = harness(), region = h.region("prep-run"), loader = region.loaders[0];
  h.event("shiny:recalculating", loader);
  h.advance(60);
  assert.equal(region.content.inert, false);
  h.event("shiny:value", loader);
  h.paint();
  h.table(loader, true);
  h.table(loader, false);
  h.advance(300);
  assert.equal(region.feedback.hidden, true);
  assert.equal(region.content.attributes["aria-hidden"], undefined);
});

test("side-by-side outputs share one indicator until both finish", () => {
  const h = harness(), region = h.region("prep-run", 2);
  h.event("shiny:recalculating", region.loaders[0]);
  h.table(region.loaders[1], true);
  h.advance(120);
  assert.equal(region.feedback.hidden, false);
  assert.equal(region.message.textContent, "Loading results...");
  h.event("shiny:error", region.loaders[0]);
  h.paint();
  h.advance(200);
  assert.equal(region.feedback.hidden, false);
  h.table(region.loaders[1], false);
  h.advance(160);
  assert.equal(region.feedback.hidden, true);
});

test("Get Random remains usable when only dependent tabs are randomizing", () => {
  const h = harness(), setup = h.region("prep-run"), plot = h.region("prep-run prep-randomize");
  h.task("prep-randomize", true);
  assert.equal(setup.feedback.hidden, true);
  assert.equal(setup.content.inert, false);
  assert.equal(plot.feedback.hidden, false);
});

test("a newer request cancels pending clearance and errors settle the region", () => {
  const h = harness(), region = h.region("prep-run");
  h.task("prep-run", true);
  h.task("prep-run", false);
  h.advance(100);
  h.task("prep-run", true);
  h.advance(200);
  assert.equal(region.feedback.hidden, false);
  h.task("prep-run", false);
  h.event("shiny:error", region.loaders[0]);
  h.paint();
  h.advance(160);
  assert.equal(region.feedback.hidden, true);
});

test("disconnect clears feedback and timers immediately; reconnect can run again", () => {
  const h = harness(), shown = h.region("prep-run"), waiting = h.region("other-run");
  h.task("prep-run", true);
  h.event("shiny:recalculating", waiting.loaders[0]);
  h.disconnect();
  h.advance(500);
  assert.equal(shown.feedback.hidden, true);
  assert.equal(waiting.feedback.hidden, true);
  h.reconnect();
  h.click("prep-run", true);
  assert.equal(shown.feedback.hidden, true);
  h.click("prep-run");
  assert.equal(shown.feedback.hidden, false);
});
