// Run with: node --test tests/js/output-feedback.test.cjs
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function harness() {
  const handlers = {}, frames = [];
  const content = {
    inert: false, attributes: {},
    setAttribute(key, value) { this.attributes[key] = value; },
    removeAttribute(key) { delete this.attributes[key]; }
  };
  const feedback = { hidden: true };
  const loader = {
    attributes: {}, busy: false,
    classList: { toggle(name, value) { loader.busy = value; } },
    closest() { return null; },
    setAttribute(key, value) { this.attributes[key] = value; },
    querySelector(selector) { return selector === ".fieldhub-output-content" ? content : feedback; }
  };
  const output = { image: null, closest() { return loader; }, querySelector() { return this.image; } };
  const document = { querySelectorAll() { return [loader]; } };
  vm.runInNewContext(fs.readFileSync(path.join(__dirname, "../../inst/app/www/output-feedback.js"), "utf8"), {
    document,
    window: { requestAnimationFrame(callback) { frames.push(callback); } },
    $() { return { on(events, callback) { events.split(" ").forEach(event => { handlers[event] = callback; }); } }; }
  });
  return {
    loader, content, feedback, output,
    event(type, target = output) { handlers[type]({ type, target }); },
    table(processing) { handlers["processing.dt"]({ target: output }, {}, processing); },
    paint() { while (frames.length) frames.shift()(); }
  };
}

test("invalidated and recalculating outputs show the same accessible busy state", () => {
  for (const event of ["shiny:outputinvalidated", "shiny:recalculating"]) {
    const h = harness();
    h.event(event);
    assert.equal(h.loader.busy, true);
    assert.equal(h.loader.attributes["aria-busy"], "true");
    assert.equal(h.content.inert, true);
    assert.equal(h.content.attributes["aria-hidden"], "true");
    assert.equal(h.feedback.hidden, false);
  }
});

test("values and errors clear loading after Shiny updates the output", () => {
  for (const event of ["shiny:value", "shiny:error"]) {
    const h = harness();
    h.event("shiny:recalculating");
    h.event(event);
    assert.equal(h.loader.busy, true);
    h.paint();
    assert.equal(h.loader.busy, false);
    assert.equal(h.content.inert, false);
    assert.equal(h.content.attributes["aria-hidden"], undefined);
    assert.equal(h.feedback.hidden, true);
  }
});

test("an older completion cannot hide a newer request's spinner", () => {
  const h = harness();
  h.event("shiny:value");
  h.event("shiny:recalculating");
  h.paint();
  assert.equal(h.loader.busy, true);
  h.event("shiny:error");
  h.paint();
  assert.equal(h.loader.busy, false);
});

test("images keep feedback until their pixels load or fail", () => {
  for (const event of ["load", "error"]) {
    const h = harness(), listeners = {};
    h.output.image = { complete: false, addEventListener(type, callback) { listeners[type] = callback; } };
    h.event("shiny:recalculating");
    h.event("shiny:value");
    h.paint();
    assert.equal(h.loader.busy, true);
    listeners[event]();
    assert.equal(h.loader.busy, false);
  }
});

test("late image events cannot clear the next request", () => {
  const h = harness(), listeners = {};
  h.output.image = { complete: false, addEventListener(type, callback) { listeners[type] = callback; } };
  h.event("shiny:recalculating");
  h.event("shiny:value");
  h.paint();
  h.event("shiny:outputinvalidated");
  listeners.load();
  assert.equal(h.loader.busy, true);
});

test("unwrapped outputs are ignored and disconnect clears pending feedback", () => {
  const h = harness();
  h.event("shiny:recalculating", { closest() { return null; } });
  assert.equal(h.loader.busy, false);
  h.event("shiny:recalculating");
  h.event("shiny:value");
  h.event("shiny:disconnected");
  h.paint();
  assert.equal(h.loader.busy, false);
  assert.equal(h.feedback.hidden, true);
});

test("table paging uses the same indicator without hiding a pending output render", () => {
  const h = harness();
  h.table(true);
  assert.equal(h.loader.busy, true);
  h.table(false);
  assert.equal(h.loader.busy, false);
  h.event("shiny:recalculating");
  h.table(true);
  h.table(false);
  assert.equal(h.loader.busy, true);
  h.event("shiny:value");
  h.table(true);
  h.paint();
  assert.equal(h.loader.busy, true);
  h.table(false);
  assert.equal(h.loader.busy, false);
});

test("global busy messages are screen-reader-only while connection loss stays visible", () => {
  const handlers = {}, root = { setAttribute(key, value) { this[key] = value; } };
  const status = { textContent: "", classList: { toggle(name, value) { status.hiddenVisually = value; } } };
  const document = { getElementById(id) { return id === "fieldhub-app" ? root : status; } };
  vm.runInNewContext(fs.readFileSync(path.join(__dirname, "../../inst/app/www/shinybusy.js"), "utf8"), {
    document, $() { return { on(event, callback) { handlers[event] = callback; } }; }
  });
  handlers["shiny:busy"]();
  assert.equal(root["aria-busy"], "true");
  assert.equal(status.hiddenVisually, true);
  assert.equal(status.textContent, "Working…");
  handlers["shiny:disconnected"]();
  assert.equal(root["aria-busy"], "false");
  assert.equal(status.hiddenVisually, false);
  assert.match(status.textContent, /Connection lost/);
  handlers["shiny:connected"]();
  assert.equal(status.textContent, "");
});
