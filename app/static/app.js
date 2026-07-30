/* Kill Curve Studio — front end.
   Settings live on the server (one source of truth for screen and export), so
   every control POSTs its change and asks for a fresh figure. */
(() => {
"use strict";

const $  = (s, r = document) => r.querySelector(s);
const $$ = (s, r = document) => Array.from(r.querySelectorAll(s));

const S = {
  cfg: {}, styles: {}, samples: [], defaults: {},
  palettes: {}, shapes: [], dashes: [],
  hasData: false, ready: false, fitTraces: [], caps: {}, busy: false,
};

/* ── utilities ─────────────────────────────────────────────── */
let toastT;
function toast(msg, isErr) {
  const el = $("#toast");
  el.textContent = msg;
  el.classList.toggle("err", !!isErr);
  el.hidden = false;
  clearTimeout(toastT);
  toastT = setTimeout(() => { el.hidden = true; }, isErr ? 6000 : 2600);
}

async function api(url, opts) {
  const r = await fetch(url, opts);
  const ct = r.headers.get("content-type") || "";
  if (!ct.includes("application/json")) {
    if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
    return r;
  }
  const j = await r.json();
  if (!r.ok || j.ok === false) throw new Error(j.error || `${r.status} ${r.statusText}`);
  return j;
}

const debounce = (fn, ms = 220) => {
  let t; return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); };
};

/** Format a number for a metrics table: significant, compact, never "NaN". */
function fmt(v, digits = 3) {
  if (v === null || v === undefined || v === "" || Number.isNaN(v)) return "—";
  const n = Number(v);
  if (!Number.isFinite(n)) return "—";
  if (n === 0) return "0";
  const a = Math.abs(n);
  if (a >= 1e5 || a < 1e-3) return n.toExponential(2).replace("e", "×10^");
  return String(Number(n.toPrecision(digits)));
}

/* ── config plumbing ───────────────────────────────────────── */
function readControl(el) {
  if (el.type === "checkbox") return el.checked;
  if (el.type === "number" || el.type === "range") {
    if (el.value === "") return null;
    const n = parseFloat(el.value);
    return Number.isFinite(n) ? n : null;
  }
  return el.value === "" ? (el.dataset.emptyNull === "0" ? "" : null) : el.value;
}

function writeControl(el, v) {
  if (el.type === "checkbox") el.checked = !!v;
  else el.value = (v === null || v === undefined) ? "" : v;
  const lbl = $(`#${el.id}Val`);
  if (lbl) lbl.textContent = el.value;
}

async function pushCfg(patch, opts = {}) {
  Object.assign(S.cfg, patch);
  await api("/api/config", {
    method: "POST", headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ cfg: patch }),
  });
  if (!opts.silent) refresh();
}

async function pushStyle(name, patch) {
  S.styles[name] = Object.assign(S.styles[name] || {}, patch);
  await api("/api/config", {
    method: "POST", headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ styles: { [name]: patch } }),
  });
  refresh();
}

/* ── rendering ─────────────────────────────────────────────── */
const PLOT_CONFIG = {
  responsive: true,
  displaylogo: false,
  // Screen download uses the SAME renderer as the on-screen plot, so it is
  // WYSIWYG and needs no server round-trip or headless browser. Its size comes
  // from the Export pane (see syncCameraButton) — left to itself the camera
  // exports 3x whatever the browser window happens to be, so the same figure
  // came out a different size, and a different ASPECT RATIO, on every window.
  toImageButtonOptions: { format: "png", filename: "kill-curve", scale: 3 },
  modeBarButtonsToAdd: ["hoverclosest", "hovercompare"],
};

/** Point the modebar camera at the configured export size. */
function syncCameraButton() {
  const g = exportGeometry("png");
  PLOT_CONFIG.toImageButtonOptions = {
    format: "png", filename: exportStem(),
    width: g.width, height: g.height, scale: g.scale,
  };
}

async function drawPlot() {
  // Deliberately does NOT gate on a client-side "hasData" flag: that flag could
  // go stale against the server (e.g. a click racing app start-up) and leave a
  // blank canvas with data loaded. The server response is the single source of
  // truth — if it returns traces, we draw them.
  try {
    const r = await fetch("/api/figure");
    const fig = await r.json();
    if (fig.ok === false) throw new Error(fig.error);
    // The server figure already carries any fit overlay, so do not concat again.
    const traces = (fig.data || []);
    const empty = traces.length === 0;
    $("#plotEmpty").hidden = !empty;
    $("#plot").style.display = empty ? "none" : "";
    S.hasData = !empty;
    // Data the figure cannot show must be stated, not just omitted.
    const warn = $("#dataWarn");
    if (warn) {
      const notes = fig.warnings || [];
      warn.hidden = notes.length === 0;
      warn.textContent = notes.join("  ");
    }
    if (empty) { Plotly.purge("plot"); return; }
    syncCameraButton();          // keep the camera on the configured export size
    await Plotly.react("plot", traces, fig.layout, PLOT_CONFIG);
  } catch (e) {
    toast("Could not draw the plot: " + e.message, true);
  }
}

async function drawAnalysis() {
  const tbl = $("#analysisTable"), empty = $("#analysisEmpty");
  if (!S.hasData) { tbl.innerHTML = ""; empty.hidden = false; return; }
  try {
    const j = await api("/api/analysis");
    if (!j.rows.length) { tbl.innerHTML = ""; empty.hidden = false; return; }
    empty.hidden = true;
    const unit = S.cfg.x_axis_label || "time";
    const cols = [
      ["sample", "Sample", 0],
      ["mu_max", "μmax (/t)", 4],
      ["doubling_time", "Doubling", 3],
      ["lag_time", "Lag", 3],
      ["y_max", "Peak", 3],
      ["t_at_y_max", "t @ peak", 3],
      ["lysis_onset", "Lysis onset", 3],
      ["max_lysis_rate", "Max lysis rate", 3],
      ["auc", "AUC", 4],
      ["time_to_log_drop", `t to ${fmt(S.cfg.log_drop ?? 3, 2)}-log`, 3],
      ["max_log_reduction", "Max log red.", 3],
      ["final_log_reduction", "Final log red.", 3],
      ["n_points", "n pts", 0],
    ];
    const has = k => j.rows.some(r => r[k] !== null && r[k] !== undefined);
    const use = cols.filter(c => c[0] === "sample" || has(c[0]));
    tbl.innerHTML =
      "<thead><tr>" + use.map(c => `<th>${esc(c[1])}</th>`).join("") + "</tr></thead><tbody>" +
      j.rows.map(r => "<tr>" + use.map(c => {
        if (c[0] === "sample") return `<td>${esc(r.sample)}</td>`;
        const v = r[c[0]];
        const cls = (v === null || v === undefined) ? "num na" : "num";
        return `<td class="${cls}">${fmt(v, c[2] || 3)}</td>`;
      }).join("") + "</tr>").join("") + "</tbody>";
    $("#metricHelp").innerHTML =
      `μmax is the steepest slope of ln(signal) over a ${S.cfg.growth_window || 4}-point window; ` +
      `doubling time is ln2/μmax. Lysis onset is the first fall of ` +
      `${Math.round((S.cfg.lysis_drop_frac || 0.05) * 100)}% below the running peak. ` +
      (j.control ? `Log reduction is relative to <strong>${esc(j.control)}</strong>.`
                 : `Pick a control sample under Analysis settings to get log reduction.`) +
      ` Times are in the units of your time column (${esc(unit)}). “—” means not applicable.`;
  } catch (e) {
    tbl.innerHTML = ""; empty.hidden = false;
    toast("Analysis failed: " + e.message, true);
  }
}

async function drawData() {
  const tbl = $("#dataTable"), empty = $("#dataEmpty");
  if (!S.hasData) { tbl.innerHTML = ""; empty.hidden = false; return; }
  const j = await api("/api/table");
  if (!j.rows.length) { tbl.innerHTML = ""; empty.hidden = false; return; }
  empty.hidden = true;
  const cols = ["sample", "time", "mean", "sd", "sem", "ci95", "n"].filter(c => j.columns.includes(c));
  const MAX = 800;
  tbl.innerHTML =
    "<thead><tr>" + cols.map(c => `<th>${esc(c)}</th>`).join("") + "</tr></thead><tbody>" +
    j.rows.slice(0, MAX).map(r => "<tr>" + cols.map(c =>
      c === "sample" ? `<td>${esc(r[c])}</td>` : `<td class="num">${fmt(r[c], 4)}</td>`
    ).join("") + "</tr>").join("") + "</tbody>";
  if (j.rows.length > MAX) {
    tbl.insertAdjacentHTML("beforeend",
      `<tfoot><tr><td colspan="${cols.length}">Showing the first ${MAX} of ${j.rows.length} rows — download the CSV for all of them.</td></tr></tfoot>`);
  }
}

const esc = s => String(s ?? "").replace(/[&<>"']/g,
  c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));

/** A finite number as a string, or "" — never arbitrary text in a value=. */
const num = v => (v === null || v === undefined || v === "" || !Number.isFinite(Number(v)))
  ? "" : String(Number(v));
/** Validate a CSS colour's SHAPE; escaping alone is not enough inside style/value. */
const safeColor = (c, fb) => /^#[0-9a-fA-F]{3,8}$/.test(String(c || "").trim())
  ? String(c).trim() : (fb || "#888888");

/* Mirror of plotting.transform_note(). Kept deliberately in sync: the figure
   says it on the Y axis, the header says it on screen. */
const NORM_LABEL = { t0: "normalised to t₀", max: "normalised to max",
                     zscore: "z-score", control: "relative to control" };
function transformNote() {
  const bits = [];
  if (S.cfg.blank_sample) bits.push("blank-subtracted");
  const n = S.cfg.normalize;
  if (n && n !== "none") bits.push(NORM_LABEL[n] || String(n));
  const w = parseInt(S.cfg.smooth_window, 10);
  if (Number.isFinite(w) && w > 1) bits.push(`smoothed (${w}-pt)`);
  return bits.join(" · ");
}
function showTransformState() {
  const note = transformNote();
  const b = $("#xformBadge");
  if (b) { b.hidden = !note; b.textContent = note ? `⚠ ${note}` : ""; }
  // A transform hidden inside a collapsed panel is how someone ends up
  // reporting a log reduction computed on z-scores. Open the panel for them.
  const card = $("#prepCard");
  if (card && note) card.open = true;
}

function refresh() {
  drawPlot();
  showTransformState();
  if ($("#pane-analysis").classList.contains("active")) drawAnalysis();
  if ($("#pane-data").classList.contains("active")) drawData();
  saveSession();
}
const saveSession = debounce(() => {
  fetch("/api/session/save", { method: "POST" }).catch(() => {});
}, 1500);

/* ── sample list ───────────────────────────────────────────── */
function renderSamples() {
  const host = $("#sampleList");
  host.innerHTML = "";
  $("#sampleHint").hidden = S.samples.length > 0;
  if (!S.samples.length) return;

  const pal = S.palettes[S.cfg.color_palette] || S.palettes.colorblind || [];
  S.samples.forEach((name, i) => {
    const st = S.styles[name] || {};
    const color = st.color || pal[i % (pal.length || 1)] || "#666666";
    const visible = st.visible !== false;
    const row = document.createElement("div");
    row.className = "sample" + (visible ? "" : " hidden-s");
    row.innerHTML = `
      <input type="checkbox" ${visible ? "checked" : ""} title="Show this sample"
             aria-label="Show ${esc(name)}">
      <input type="color" value="${esc(color)}" title="Colour" aria-label="Colour for ${esc(name)}">
      <input type="text" value="${esc(st.label || name)}" title="Legend label"
             aria-label="Legend label for ${esc(name)}">
      <select title="Point shape" aria-label="Point shape for ${esc(name)}">
        ${S.shapes.map(sh => `<option value="${esc(sh)}"${(st.shape || "") === sh ? " selected" : ""}>${esc(sh.replace("-open", " ○").replace("triangle-up", "triangle"))}</option>`).join("")}
      </select>`;
    const [chk, col, lab, shp] = row.children;
    chk.onchange = () => pushStyle(name, { visible: chk.checked }).then(renderSamples);
    col.oninput = debounce(() => pushStyle(name, { color: col.value }), 120);
    lab.onchange = () => pushStyle(name, { label: lab.value.trim() || name });
    shp.onchange = () => pushStyle(name, { shape: shp.value });
    host.appendChild(row);
  });

  // Sample-referencing dropdowns
  for (const id of ["blank_sample", "control_sample"]) {
    const sel = $("#" + id);
    const cur = S.cfg[id] || "";
    sel.innerHTML = `<option value="">None</option>` +
      S.samples.map(s => `<option value="${esc(s)}"${cur === s ? " selected" : ""}>${esc(s)}</option>`).join("");
  }
}

function renderSwatches() {
  const pal = S.palettes[S.cfg.color_palette] || [];
  $("#swatches").innerHTML = pal.slice(0, 10)
    .map(c => `<i style="background:${esc(c)}"></i>`).join("");
}

/* ── regions / markers ─────────────────────────────────────── */
function renderAnnotations() {
  const rl = $("#regionList");
  rl.innerHTML = "";
  (S.cfg.regions || []).forEach((r, i) => {
    const d = document.createElement("div");
    d.className = "mini";
    // Every interpolated value is escaped AND colour is shape-validated: these
    // come from an imported settings .json, which is untrusted input.
    d.innerHTML = `<input type="number" step="any" value="${esc(num(r.x0))}" placeholder="from" aria-label="Region ${i + 1} start">
                   <input type="number" step="any" value="${esc(num(r.x1))}" placeholder="to" aria-label="Region ${i + 1} end">
                   <input type="color" value="${esc(safeColor(r.color, "#ffd54f"))}" aria-label="Region ${i + 1} colour">
                   <button class="icon-btn" title="Remove" aria-label="Remove region ${i + 1}">✕</button>`;
    const [x0, x1, col, del] = d.children;
    const upd = debounce(() => {
      S.cfg.regions[i] = { x0: parseFloat(x0.value), x1: parseFloat(x1.value),
                           color: col.value, alpha: r.alpha ?? 0.2, label: r.label || "" };
      pushCfg({ regions: S.cfg.regions });
    }, 250);
    [x0, x1, col].forEach(el => el.oninput = upd);
    del.onclick = () => { S.cfg.regions.splice(i, 1); pushCfg({ regions: S.cfg.regions }).then(renderAnnotations); };
    rl.appendChild(d);
  });

  const ml = $("#markerList");
  ml.innerHTML = "";
  (S.cfg.markers || []).forEach((m, i) => {
    const d = document.createElement("div");
    d.className = "mini";
    d.innerHTML = `<input type="number" step="any" value="${esc(num(m.x))}" placeholder="time" aria-label="Marker ${i + 1} time">
                   <input type="text" value="${esc(m.label || "")}" placeholder="label" aria-label="Marker ${i + 1} label">
                   <input type="color" value="${esc(safeColor(m.color, "#666666"))}" aria-label="Marker ${i + 1} colour">
                   <button class="icon-btn" title="Remove" aria-label="Remove marker ${i + 1}">✕</button>`;
    const [x, lab, col, del] = d.children;
    const upd = debounce(() => {
      S.cfg.markers[i] = { x: parseFloat(x.value), label: lab.value, color: col.value,
                           dash: m.dash || "dash", width: m.width ?? 1 };
      pushCfg({ markers: S.cfg.markers });
    }, 250);
    [x, lab, col].forEach(el => el.oninput = upd);
    del.onclick = () => { S.cfg.markers.splice(i, 1); pushCfg({ markers: S.cfg.markers }).then(renderAnnotations); };
    ml.appendChild(d);
  });
}

/* ── data loading ──────────────────────────────────────────── */
function showDataInfo(j) {
  const el = $("#dataInfo");
  el.hidden = false;
  el.classList.remove("err");
  el.innerHTML =
    `<strong>${esc(j.filename)}</strong><br>` +
    `${j.samples.length} sample${j.samples.length === 1 ? "" : "s"} · ` +
    `${j.n_points} timepoint${j.n_points === 1 ? "" : "s"} · ` +
    `up to ${j.max_replicates} replicate${j.max_replicates === 1 ? "" : "s"}<br>` +
    `<span class="hint">${esc(j.layout)} format · time ${fmt(j.time_range[0])}–${fmt(j.time_range[1])}</span>`;
  const b = $("#fileBadge");
  b.hidden = false; b.textContent = j.filename;
}

async function afterLoad(j) {
  S.samples = j.samples;
  S.hasData = true;
  await loadConfig();          // server may have changed control/blank defaults
  renderSamples(); renderSwatches(); renderAnnotations();
  showDataInfo(j);
  S.fitTraces = [];
  refresh();
  toast(`Loaded ${j.samples.length} samples`);
}

async function uploadFile(file) {
  if (!file) return;
  const fd = new FormData();
  fd.append("file", file);
  try {
    const j = await api("/api/upload", { method: "POST", body: fd });
    await afterLoad(j);
  } catch (e) {
    const el = $("#dataInfo");
    el.hidden = false; el.classList.add("err");
    el.textContent = e.message;
    toast(e.message, true);
  }
}

/* ── export ────────────────────────────────────────────────── */
/** Pixel canvas and scale for one export, matching the server's rules.

    DPI is meaningless for SVG: plotly multiplies the root width/height by
    scale while leaving the viewBox alone, so scaling only inflates the
    DECLARED page size. At the default 300 DPI that shipped a "10 x 8 in" SVG
    reporting 31 x 25 in, next to a PDF button giving a correct 10 x 8.
    render_images() pins scale=1 for vector for exactly this reason. */
function exportGeometry(fmt) {
  const w = Number(S.cfg.export_width || 10), h = Number(S.cfg.export_height || 8);
  const dpi = Number(S.cfg.export_dpi || 300);
  return {
    width: Math.round(w * 96), height: Math.round(h * 96),
    scale: fmt === "svg" ? 1 : Math.max(dpi / 96, 0.1),
    dpi,
  };
}

/** Filename stem, so a series of conditions doesn't become kill-curve (3).png. */
function exportStem() {
  const raw = S.filename || "";
  const stem = raw.replace(/\.[^.]*$/, "").replace(/[^A-Za-z0-9_-]+/g, "_");
  return stem || "kill-curve";
}

/** Splice a pHYs chunk into a PNG so it declares its real physical size.

    Chrome's canvas encoder writes no resolution at all, so a 3000x2400 PNG
    (10 x 8 in at 300 DPI) is read as 96 DPI and placed 31 in wide in Word.
    The server does this in Python; the PNG/JPEG buttons never reach it. */
function pngWithDpi(buf, dpi) {
  const b = new Uint8Array(buf);
  const sig = [137, 80, 78, 71, 13, 10, 26, 10];
  if (b.length < 8 || !sig.every((v, i) => b[i] === v)) return buf;

  const ppu = Math.round(dpi / 0.0254);            // pixels per metre
  const data = new Uint8Array(9);
  new DataView(data.buffer).setUint32(0, ppu);
  new DataView(data.buffer).setUint32(4, ppu);
  data[8] = 1;                                     // unit 1 = the metre

  const typed = new Uint8Array(13);
  typed.set([0x70, 0x48, 0x59, 0x73]);             // "pHYs"
  typed.set(data, 4);
  const chunk = new Uint8Array(21);
  new DataView(chunk.buffer).setUint32(0, 9);
  chunk.set(typed, 4);
  new DataView(chunk.buffer).setUint32(17, crc32(typed));

  // Walk chunks: replace an existing pHYs, else insert straight after IHDR.
  let pos = 8, afterIhdr = null, existing = null;
  const dv = new DataView(b.buffer, b.byteOffset, b.byteLength);
  while (pos + 8 <= b.length) {
    const len = dv.getUint32(pos);
    const type = String.fromCharCode(b[pos + 4], b[pos + 5], b[pos + 6], b[pos + 7]);
    const next = pos + 12 + len;
    if (next > b.length) return buf;                // truncated: leave alone
    if (type === "IHDR") afterIhdr = next;
    else if (type === "pHYs") { existing = [pos, next]; break; }
    else if (type === "IDAT") break;
    pos = next;
  }
  const cut = existing ? existing : (afterIhdr === null ? null : [afterIhdr, afterIhdr]);
  if (!cut) return buf;
  const out = new Uint8Array(b.length - (cut[1] - cut[0]) + chunk.length);
  out.set(b.subarray(0, cut[0]), 0);
  out.set(chunk, cut[0]);
  out.set(b.subarray(cut[1]), cut[0] + chunk.length);
  return out.buffer;
}

let CRC_TABLE = null;
function crc32(bytes) {
  if (!CRC_TABLE) {
    CRC_TABLE = new Uint32Array(256);
    for (let n = 0; n < 256; n++) {
      let c = n;
      for (let k = 0; k < 8; k++) c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1;
      CRC_TABLE[n] = c >>> 0;
    }
  }
  let c = 0xFFFFFFFF;
  for (let i = 0; i < bytes.length; i++) c = CRC_TABLE[(c ^ bytes[i]) & 0xFF] ^ (c >>> 8);
  return (c ^ 0xFFFFFFFF) >>> 0;
}

/** Client-side export for formats Plotly.js can render itself.
    Guarantees the file matches the screen and needs no headless browser. */
async function clientExport(fmt) {
  const g = exportGeometry(fmt);
  const url = await Plotly.toImage($("#plot"), {
    format: fmt, width: g.width, height: g.height, scale: g.scale,
  });
  let href = url;
  if (fmt === "png") {
    // Re-tag the raster so it lands at the requested physical size.
    const buf = await (await fetch(url)).arrayBuffer();
    href = URL.createObjectURL(new Blob([pngWithDpi(buf, g.dpi)], { type: "image/png" }));
  }
  const a = document.createElement("a");
  a.href = href;
  a.download = `${exportStem()}.${fmt === "jpeg" ? "jpg" : fmt}`;
  document.body.appendChild(a); a.click(); a.remove();
  if (href !== url) setTimeout(() => URL.revokeObjectURL(href), 2000);
}

async function doExport(fmt) {
  if (!S.hasData) { toast("Load data first", true); return; }
  const btn = $(`.btn.exp[data-exp="${fmt}"]`);
  const old = btn ? btn.textContent : "";
  if (btn) { btn.disabled = true; btn.textContent = "Working…"; }
  const msg = $("#exportMsg"); msg.hidden = true; msg.classList.remove("err");
  try {
    if (["png", "jpeg", "svg"].includes(fmt)) {
      await clientExport(fmt);                 // WYSIWYG, always available
    } else {
      const p = new URLSearchParams({
        width: S.cfg.export_width ?? 10, height: S.cfg.export_height ?? 8,
        dpi: S.cfg.export_dpi ?? 300,
      });
      const r = await fetch(`/api/export/${fmt}?${p}`);
      if (!r.ok) {
        let m = `${r.status} ${r.statusText}`;
        try { m = (await r.json()).error || m; } catch {}
        throw new Error(m);
      }
      const blob = await r.blob();
      const cd = r.headers.get("content-disposition") || "";
      const nm = (cd.match(/filename="?([^"]+)"?/) || [, `kill-curve.${fmt}`])[1];
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url; a.download = nm;
      document.body.appendChild(a); a.click(); a.remove();
      setTimeout(() => URL.revokeObjectURL(url), 2000);
    }
    toast(`Exported ${fmt.toUpperCase()}`);
  } catch (e) {
    msg.hidden = false; msg.classList.add("err"); msg.textContent = e.message;
    toast("Export failed: " + e.message, true);
  } finally {
    if (btn) { btn.disabled = false; btn.textContent = old; }
  }
}

/* ── curve fits ────────────────────────────────────────────── */
async function runFit() {
  if (!S.hasData) { toast("Load data first", true); return; }
  const btn = $("#runFit"); btn.disabled = true; btn.textContent = "Fitting…";
  try {
    const model = $("#fitModel").value;
    const j = await api("/api/fit", {
      method: "POST", headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model }),
    });
    const tbl = $("#fitTable");
    const ok = j.fits.filter(f => f.ok);
    $("#fitEmpty").hidden = j.fits.length > 0;
    const keys = [...new Set(ok.flatMap(f => Object.keys(f.params || {})))];
    tbl.innerHTML =
      "<thead><tr><th>Sample</th><th>R²</th>" + keys.map(k => `<th>${esc(k)}</th>`).join("") +
      "</tr></thead><tbody>" +
      j.fits.map(f => `<tr><td>${esc(f.sample)}</td>` +
        `<td class="num${f.ok ? "" : " na"}">${f.ok ? fmt(f.r2, 4) : "no fit"}</td>` +
        keys.map(k => `<td class="num">${fmt((f.params || {})[k], 4)}</td>`).join("") +
        "</tr>").join("") + "</tbody>";

    S.fitTraces = ok.filter(f => f.t && f.fitted).map(f => ({
      x: f.t, y: f.fitted, mode: "lines", name: `${f.sample} (fit)`,
      line: { dash: "dot", width: 2,
              color: (S.styles[f.sample] || {}).color || "#888" },
      hoverinfo: "skip", showlegend: true,
    }));
    // Push the overlay to the server too, or server-rendered exports
    // (PDF/TIFF/PPTX/GIF) would silently omit the fitted curves that are
    // visible on screen and in the client-side PNG/SVG.
    await api("/api/overlays", {
      method: "POST", headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ traces: S.fitTraces }),
    }).catch(() => {});
    drawPlot();
    toast(ok.length ? `Fitted ${ok.length}/${j.fits.length} curves` : "No curve could be fitted", !ok.length);
  } catch (e) {
    toast("Fit failed: " + e.message, true);
  } finally {
    btn.disabled = false; btn.textContent = "Fit models";
  }
}

/* ── wiring ────────────────────────────────────────────────── */
async function loadConfig() {
  const j = await api("/api/config");
  S.cfg = j.cfg; S.styles = j.styles; S.defaults = j.defaults;
  S.palettes = j.palettes; S.shapes = j.shapes; S.dashes = j.dashes;
  S.filename = j.filename || "";   // names exports after the data, as the server does
  if (j.samples && j.samples.length) S.samples = j.samples;

  const psel = $("#color_palette");
  if (!psel.options.length) {
    const nice = { custom: "Custom", colorblind: "Colourblind-safe (Okabe-Ito)",
      viridis: "Viridis", plasma: "Plasma", publication: "Publication",
      rainbow: "Rainbow", gray: "Greyscale", warm: "Warm", cool: "Cool" };
    psel.innerHTML = Object.keys(S.palettes)
      .filter(k => k !== "custom")
      .map(k => `<option value="${esc(k)}">${esc(nice[k] || k)}</option>`).join("");
  }
  $$("[data-cfg]").forEach(el => writeControl(el, S.cfg[el.dataset.cfg]));
  renderSwatches();
}

function bindControls() {
  $$("[data-cfg]").forEach(el => {
    const key = el.dataset.cfg;
    const handler = () => {
      const lbl = $(`#${el.id}Val`);
      if (lbl) lbl.textContent = el.value;
      pushCfg({ [key]: readControl(el) });
      if (key === "color_palette") { renderSwatches(); renderSamples(); }
    };
    const ev = (el.type === "range" || el.type === "text") ? "input" : "change";
    el.addEventListener(ev, ev === "input" ? debounce(handler, 260) : handler);
  });

  // tabs
  $$(".tab").forEach(t => t.onclick = () => {
    $$(".tab").forEach(x => { x.classList.remove("active"); x.setAttribute("aria-selected", "false"); });
    t.classList.add("active"); t.setAttribute("aria-selected", "true");
    $$(".pane").forEach(p => p.classList.remove("active"));
    $(`#pane-${t.dataset.tab}`).classList.add("active");
    if (t.dataset.tab === "analysis") drawAnalysis();
    if (t.dataset.tab === "data") drawData();
    if (t.dataset.tab === "plot") Plotly.Plots.resize($("#plot"));
  });

  // file input + drag/drop
  $("#fileInput").onchange = e => uploadFile(e.target.files[0]);
  const drop = $("#drop");
  ["dragenter", "dragover"].forEach(ev => drop.addEventListener(ev, e => {
    e.preventDefault(); drop.classList.add("over");
  }));
  ["dragleave", "drop"].forEach(ev => drop.addEventListener(ev, e => {
    e.preventDefault(); drop.classList.remove("over");
  }));
  drop.addEventListener("drop", e => uploadFile(e.dataTransfer.files[0]));

  const demo = async () => {
    try { await afterLoad(await api("/api/demo", { method: "POST" })); }
    catch (e) { toast(e.message, true); }
  };
  $("#demoBtn").onclick = demo;
  $("#demoBtn2").onclick = demo;

  $("#addRegion").onclick = () => {
    S.cfg.regions = (S.cfg.regions || []).concat(
      [{ x0: 0, x1: 10, color: "#ffd54f", alpha: 0.2, label: "" }]);
    pushCfg({ regions: S.cfg.regions }).then(renderAnnotations);
  };
  $("#addMarker").onclick = () => {
    S.cfg.markers = (S.cfg.markers || []).concat(
      [{ x: 0, label: "", color: "#666666", dash: "dash", width: 1 }]);
    pushCfg({ markers: S.cfg.markers }).then(renderAnnotations);
  };

  $$(".btn.exp").forEach(b => b.onclick = () => doExport(b.dataset.exp));
  $("#runFit").onclick = runFit;
  $("#clearFit").onclick = async () => {
    S.fitTraces = [];
    await api("/api/overlays", { method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ traces: [] }) }).catch(() => {});
    drawPlot();
  };

  $("#resetBtn").onclick = async () => {
    if (!confirm("Reset every setting back to defaults? Your loaded data stays.")) return;
    await api("/api/config/reset", { method: "POST" });
    await loadConfig();
    renderSamples(); renderAnnotations();
    refresh();
    toast("Settings reset");
  };

  $("#settingsInput").onchange = async e => {
    const f = e.target.files[0]; if (!f) return;
    const fd = new FormData(); fd.append("file", f);
    try {
      const j = await api("/api/settings/import", { method: "POST", body: fd });
      await loadConfig();
      renderSamples(); renderAnnotations(); refresh();
      const el = $("#settingsMsg"); el.hidden = false;
      el.innerHTML = `Applied ${j.applied} settings.` +
        (j.matched.length ? `<br>Matched: ${j.matched.map(esc).join(", ")}` : "") +
        (j.unmatched.length ? `<br><span class="hint">Not in this dataset: ${j.unmatched.map(esc).join(", ")}</span>` : "");
      toast("Settings loaded");
    } catch (err) { toast(err.message, true); }
    e.target.value = "";
  };

  $("#helpBtn").onclick = () => $("#helpDlg").showModal();

  const themeBtn = $("#themeBtn");
  const setTheme = t => {
    document.body.dataset.theme = t;
    themeBtn.textContent = t === "dark" ? "☀️" : "🌙";
    try { localStorage.setItem("kc_theme", t); } catch {}
    if (S.hasData) drawPlot();
  };
  themeBtn.onclick = () => setTheme(document.body.dataset.theme === "dark" ? "light" : "dark");
  let saved = null;
  try { saved = localStorage.getItem("kc_theme"); } catch {}
  setTheme(saved || (matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light"));

  // Ctrl/Cmd+S exports a PNG rather than saving the web page.
  addEventListener("keydown", e => {
    if (!(e.ctrlKey || e.metaKey) || e.key.toLowerCase() !== "s") return;
    const t = e.target;
    // Don't steal Ctrl+S from a field the user is editing.
    if (t && /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName)) return;
    e.preventDefault(); doExport("png");
  });
}

function setReady(ready) {
  // Until the first config load completes, the controls would act on empty
  // state. Disable them and say so rather than silently doing nothing.
  S.ready = ready;
  ["demoBtn", "demoBtn2", "fileInput"].forEach(id => {
    const el = $("#" + id); if (el) el.disabled = !ready;
  });
  const d = $("#drop");
  if (d) d.style.opacity = ready ? "" : ".5";
}

(async function init() {
  setReady(false);
  bindControls();
  try {
    const h = await api("/api/health");
    S.caps = h.capabilities || {};
    if (!S.caps.kaleido) {
      // PNG/JPEG/SVG still work (client-side); only these need the engine.
      ["pdf", "tiff", "pptx", "gif"].forEach(f => {
        const b = $(`.btn.exp[data-exp="${f}"]`);
        if (b) { b.disabled = true; b.title = "Needs the kaleido engine — re-run the installer"; }
      });
    }
  } catch {}
  await loadConfig();
  renderSamples(); renderAnnotations();
  try { await api("/api/session/restore", { method: "POST" }); await loadConfig(); } catch {}
  $$("[data-cfg]").forEach(el => writeControl(el, S.cfg[el.dataset.cfg]));
  setReady(true);
  // The server may already hold data (a restored session, or a reload after
  // uploading). Draw whatever it has instead of assuming an empty canvas.
  drawPlot();
})();
})();
