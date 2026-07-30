"""
Kill Curve Studio — local Flask server.

Binds to 127.0.0.1 only. Your data is read, plotted and exported entirely on
this machine; nothing is uploaded anywhere.
"""
from __future__ import annotations

import io
import json
import os
import re
import time
import uuid
import webbrowser

import numpy as np
import pandas as pd
from flask import Flask, jsonify, request, send_file, send_from_directory

from . import analysis as A
from . import data as D
from . import plotting as P

HERE = os.path.dirname(os.path.abspath(__file__))
STATIC = os.path.join(HERE, "static")
SESSION_DIR = os.path.join(os.path.expanduser("~"), ".killcurve_studio")
os.makedirs(SESSION_DIR, exist_ok=True)

MAX_UPLOAD_MB = 64

app = Flask(__name__, static_folder=None)
app.config["MAX_CONTENT_LENGTH"] = MAX_UPLOAD_MB * 1024 * 1024

# Everything the UI can set. Kept flat so save/load settings is a plain dict
# round-trip and old files stay loadable as new keys are added.
DEFAULTS = {
    "plot_title": "", "plot_subtitle": "", "title_font_size": 18, "title_x": 0.5,
    "x_axis_label": "Time (minutes)", "y_axis_label": "OD (A550)",
    "x_scale_type": "linear", "y_scale_type": "log",
    "x_min": None, "x_max": None, "y_min": None, "y_max": None,
    "x_tick_interval": None, "y_tick_interval": None,
    "x_minor_ticks": False, "y_minor_ticks": False,
    "axis_label_font_size": 16, "axis_text_font_size": 14, "axis_text_angle": 0,
    "axis_color": "#000000", "axis_line_width": 1.2, "panel_border": True,
    "show_major_gridlines": False, "show_minor_gridlines": False,
    "major_gridline_color": "#E6E6E6", "minor_gridline_color": "#F2F2F2",
    "major_gridline_size": 0.5,
    "font_family": "Arial", "template": "simple_white",
    "plot_bgcolor": "#FFFFFF", "paper_bgcolor": "#FFFFFF",
    "color_palette": "colorblind", "vary_shape": True, "vary_linetype": False,
    "line_thickness": 2, "show_lines": True,
    "show_points": True, "shape_size": 8, "point_stroke": 1,
    "show_end_labels": False, "label_font_size": 12, "label_offset": 8,
    "error_type": "sem", "error_display_mode": "bar", "error_multiplier": 1,
    "error_bar_width": 4, "error_bar_thickness": 1, "shadow_alpha": 0.2,
    "show_error_caption": False,
    "show_legend": True, "legend_orientation": "v", "legend_x": 1.02,
    "legend_y": 1.0, "legend_font_size": 13,
    "blank_sample": None, "normalize": "none", "smooth_window": 1,
    "time_min": None, "time_max": None, "exclude_timepoints": "",
    "regions": [], "markers": [],
    "export_width": 10.0, "export_height": 8.0, "export_dpi": 300,
    "gif_fps": 1.0, "control_sample": None, "log_drop": 3.0,
    "growth_window": 4, "lysis_drop_frac": 0.05,
}

# Server-side session state. Single-user local app, so one process-wide store.
STATE = {
    "overlays": [],     # extra traces (curve fits) so exports match the screen
    "raw": None,        # original DataFrame
    "long": None,       # tidy DataFrame
    "layout": None,
    "samples": [],
    "filename": "",
    "cfg": dict(DEFAULTS),
    "styles": {},
    "loaded_at": None,
}


@app.before_request
def _guard_local_only():
    """Reject cross-origin and non-local requests.

    This process holds the user's data behind an unauthenticated API. Without
    this, any website the scientist happens to visit could POST to
    http://127.0.0.1:8765 in the background (or reach it via DNS rebinding with
    a hostname that resolves to 127.0.0.1) and read or overwrite their session.
    Browsers send Origin on cross-site requests, so checking it costs nothing.
    """
    # The Host header must be a loopback literal — blocks DNS rebinding.
    host = (request.host or "").split(":")[0].strip("[]").lower()
    if host not in ("127.0.0.1", "localhost", "::1", ""):
        return jsonify({"ok": False, "error": "This app only serves localhost."}), 403

    origin = request.headers.get("Origin")
    if origin:
        try:
            from urllib.parse import urlparse
            oh = (urlparse(origin).hostname or "").lower()
        except Exception:
            oh = "?"
        if oh not in ("127.0.0.1", "localhost", "::1"):
            return jsonify({"ok": False, "error": "Cross-origin requests are not allowed."}), 403

    # Any state-changing request must come from our own page, not a form post
    # on someone else's site (which would not carry an Origin in older browsers).
    if request.method not in ("GET", "HEAD", "OPTIONS") and not origin:
        sec = request.headers.get("Sec-Fetch-Site")
        if sec and sec not in ("same-origin", "none"):
            return jsonify({"ok": False, "error": "Cross-site request blocked."}), 403
    return None


# ── helpers ─────────────────────────────────────────────────────────────────
def _csv_cell(v):
    """Quote a CSV cell and defuse spreadsheet formula injection.

    Excel/Sheets execute a cell beginning with = + - @ tab or CR. Sample names
    come from an uploaded file, so they are untrusted. Plain numbers are left
    alone so "-0.5" stays numeric.
    """
    t = "" if v is None else str(v)
    if t[:1] in ("=", "+", "-", "@", chr(9), chr(13)):
        try:
            float(t)
        except ValueError:
            t = "'" + t
    return t


def _safe_frame_csv(df):
    """DataFrame -> CSV bytes with every text cell defused.

    Selects columns by "is not numeric" rather than "dtype == object": pandas 3
    reports text columns as dtype 'str', so an object-only check silently skips
    them and the escaping never runs.
    """
    import pandas.api.types as pdt

    out = df.copy()
    for c in out.columns:
        if not pdt.is_numeric_dtype(out[c]) and not pdt.is_bool_dtype(out[c]):
            out[c] = out[c].map(_csv_cell)
    out.columns = [_csv_cell(c) for c in out.columns]
    return out.to_csv(index=False).encode("utf-8")


def _clean_for_json(obj):
    """NaN/Inf are not valid JSON; convert to None so the browser gets nulls."""
    if isinstance(obj, dict):
        return {k: _clean_for_json(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_clean_for_json(v) for v in obj]
    if isinstance(obj, (np.integer,)):
        return int(obj)
    if isinstance(obj, (np.floating, float)):
        f = float(obj)
        return None if (np.isnan(f) or np.isinf(f)) else f
    if isinstance(obj, (np.bool_,)):
        return bool(obj)
    if obj is pd.NaT:
        return None
    return obj


def _parse_excludes(spec):
    """'0, 15, 30-45' -> a matcher for excluded timepoints."""
    vals, ranges = set(), []
    for part in re.split(r"[,;\s]+", str(spec or "").strip()):
        if not part:
            continue
        m = re.match(r"^(-?[\d.]+)\s*-\s*(-?[\d.]+)$", part)
        if m:
            try:
                ranges.append((float(m.group(1)), float(m.group(2))))
            except ValueError:
                pass
            continue
        try:
            vals.add(float(part))
        except ValueError:
            pass
    return vals, ranges


def _filtered_long():
    """Tidy data with the time filter and exclusions applied."""
    lg = STATE["long"]
    if lg is None or lg.empty:
        return lg
    cfg = STATE["cfg"]
    out = lg
    tmin, tmax = cfg.get("time_min"), cfg.get("time_max")
    if tmin not in (None, ""):
        out = out[out["time"] >= float(tmin)]
    if tmax not in (None, ""):
        out = out[out["time"] <= float(tmax)]
    vals, ranges = _parse_excludes(cfg.get("exclude_timepoints"))
    if vals:
        out = out[~out["time"].isin(vals)]
    for lo, hi in ranges:
        lo, hi = min(lo, hi), max(lo, hi)
        out = out[~((out["time"] >= lo) & (out["time"] <= hi))]
    return out


def _agg():
    cfg = STATE["cfg"]
    return D.aggregate(
        _filtered_long(),
        error_type=cfg.get("error_type", "sem"),
        blank_sample=cfg.get("blank_sample"),
        normalize=cfg.get("normalize"),
        smooth_window=int(cfg.get("smooth_window", 1) or 1),
    )


def _visible_samples(agg):
    """Samples in declared order, minus any hidden in styles."""
    present = list(dict.fromkeys(agg["sample"].tolist())) if agg is not None and len(agg) else []
    ordered = [s for s in STATE["samples"] if s in present] or present
    return [s for s in ordered if STATE["styles"].get(s, {}).get("visible", True)]


def _fig():
    agg = _agg()
    return P.build_figure(agg, STATE["cfg"], STATE["styles"],
                          samples=_visible_samples(agg),
                          analysis_overlays=STATE.get("overlays") or []), agg


@app.route("/api/overlays", methods=["POST"])
def overlays():
    """Store fitted-curve traces so server-rendered exports include them too."""
    body = request.get_json(silent=True) or {}
    tr = body.get("traces")
    STATE["overlays"] = tr if isinstance(tr, list) else []
    return jsonify({"ok": True, "n": len(STATE["overlays"])})


# ── routes: static ──────────────────────────────────────────────────────────
@app.route("/")
def index():
    return send_from_directory(STATIC, "index.html")


@app.route("/static/<path:fn>")
def static_files(fn):
    return send_from_directory(STATIC, fn)


@app.route("/api/health")
def health():
    caps = {}
    for mod, key in [("plotly", "plotly"), ("kaleido", "kaleido"), ("pptx", "pptx"),
                     ("PIL", "pillow"), ("scipy", "scipy")]:
        try:
            __import__(mod)
            caps[key] = True
        except Exception:
            caps[key] = False
    return jsonify({"ok": True, "capabilities": caps,
                    "has_data": STATE["long"] is not None and not STATE["long"].empty})


# ── routes: data ────────────────────────────────────────────────────────────
@app.route("/api/upload", methods=["POST"])
def upload():
    f = request.files.get("file")
    if f is None:
        return jsonify({"ok": False, "error": "No file was sent."}), 400
    content = f.read()
    if not content:
        return jsonify({"ok": False, "error": "That file is empty."}), 400
    try:
        df = D.load_table(content, f.filename or "")
    except Exception as e:
        return jsonify({"ok": False, "error": f"Could not read the file: {e}"}), 400
    if df is None or df.empty or not len(df.columns):
        return jsonify({"ok": False,
                        "error": "No readable columns. Expected a time column plus one "
                                 "column per sample (or time/sample/value in long format)."}), 400

    layout = D.detect_layout(df)
    lg = D.to_long(df, layout)
    if lg.empty or lg["value"].notna().sum() == 0:
        return jsonify({"ok": False,
                        "error": "No numeric measurements found. Check that your value "
                                 "columns contain numbers."}), 400

    STATE.update(raw=df, long=lg, layout=layout,
                 samples=list(dict.fromkeys(lg["sample"].tolist())),
                 filename=f.filename or "data.csv", loaded_at=time.time())
    # Reset per-sample styling; keep global look so a user can re-upload and
    # keep their theme.
    STATE["styles"] = {}
    STATE["overlays"] = []          # fits belong to the previous dataset
    cfg = STATE["cfg"]
    cfg["time_min"] = cfg["time_max"] = None
    cfg["exclude_timepoints"] = ""
    cfg["blank_sample"] = None
    cfg["control_sample"] = None
    # Normalisation and smoothing are transforms OF THE DATA, so they must not
    # outlive the dataset they were chosen for. They used to persist across both
    # a new upload and an app restart (the session file replays cfg on boot),
    # inside a collapsed panel, with nothing on the figure to say so — so a
    # z-scored, 7-point-smoothed plot could be read as raw OD, and the log
    # reduction in the Analysis tab was arithmetic on z-scores.
    cfg["normalize"] = DEFAULTS["normalize"]
    cfg["smooth_window"] = DEFAULTS["smooth_window"]

    # Adopt the file's own column names as axis labels unless the user has
    # already typed their own — "Time (minutes)" on an hours dataset is wrong.
    if layout.get("time") and cfg.get("x_axis_label") in (DEFAULTS["x_axis_label"], "", None):
        cfg["x_axis_label"] = str(layout["time"])
    if layout["layout"] == "long" and layout.get("value") and             cfg.get("y_axis_label") in (DEFAULTS["y_axis_label"], "", None):
        cfg["y_axis_label"] = str(layout["value"])

    tmin = float(lg["time"].min()) if len(lg) else 0.0
    tmax = float(lg["time"].max()) if len(lg) else 0.0
    reps = int(lg.groupby(["sample", "time"]).size().max()) if len(lg) else 0
    return jsonify(_clean_for_json({
        "ok": True, "filename": STATE["filename"], "layout": layout["layout"],
        "time_column": layout["time"], "samples": STATE["samples"],
        "n_rows": int(len(df)), "n_points": int(lg["time"].nunique()),
        "max_replicates": reps, "time_range": [tmin, tmax],
        "palette": P.palette_for(cfg.get("color_palette", "colorblind"), len(STATE["samples"])),
    }))


@app.route("/api/demo", methods=["POST"])
def demo():
    """Synthetic phage-lysis dataset so the app is usable with zero setup."""
    rng = np.random.default_rng(7)
    t = np.arange(0, 185, 10, dtype=float)
    cols = {"Time": t}

    def growth(k, mu, lag):
        return k / (1 + np.exp(-mu * (t - lag)))

    for rep in range(3):                                   # uninfected control
        cols.setdefault("Uninfected", []).append(
            np.clip(growth(1.15, 0.055, 75) + rng.normal(0, 0.012, t.size), 1e-3, None))
    for rep in range(3):                                   # infected, MOI 0.1
        base = growth(1.0, 0.05, 70)
        lysed = np.where(t > 90, base * np.exp(-0.035 * (t - 90)), base)
        cols.setdefault("Phage MOI 0.1", []).append(
            np.clip(lysed + rng.normal(0, 0.012, t.size), 1e-3, None))
    for rep in range(3):                                   # infected, MOI 1
        base = growth(0.8, 0.05, 60)
        lysed = np.where(t > 60, base * np.exp(-0.06 * (t - 60)), base)
        cols.setdefault("Phage MOI 1", []).append(
            np.clip(lysed + rng.normal(0, 0.01, t.size), 1e-3, None))
    for rep in range(2):                                   # antibiotic kill
        cols.setdefault("Antibiotic", []).append(
            np.clip(0.35 * np.exp(-0.05 * t) + 0.004 + rng.normal(0, 0.004, t.size), 1e-4, None))

    # Build with duplicate headers, exactly like a real replicate CSV.
    arrays = [t]
    headers = ["Time"]
    for name, reps in cols.items():
        if name == "Time":
            continue
        for r in reps:
            headers.append(name)
            arrays.append(r)
    df = pd.DataFrame(np.column_stack(arrays), columns=headers)

    layout = D.detect_layout(df)
    lg = D.to_long(df, layout)
    STATE.update(raw=df, long=lg, layout=layout,
                 samples=list(dict.fromkeys(lg["sample"].tolist())),
                 filename="demo_lysis_curve.csv", loaded_at=time.time())
    STATE["styles"] = {}
    STATE["overlays"] = []
    cfg = STATE["cfg"]
    cfg.update(time_min=None, time_max=None, exclude_timepoints="",
               blank_sample=None, control_sample="Uninfected",
               plot_title="Phage lysis curves", y_axis_label="OD (A550)",
               x_axis_label="Time (minutes)")
    return jsonify(_clean_for_json({
        "ok": True, "filename": STATE["filename"], "layout": layout["layout"],
        "samples": STATE["samples"], "n_rows": int(len(df)),
        "n_points": int(lg["time"].nunique()),
        "max_replicates": int(lg.groupby(["sample", "time"]).size().max()),
        "time_range": [float(t.min()), float(t.max())],
    }))


@app.route("/api/table")
def table():
    """The tidy data behind the plot, for the Data tab."""
    agg = _agg()
    if agg is None or agg.empty:
        return jsonify({"ok": True, "rows": [], "columns": []})
    out = agg.copy()
    for c in ("mean", "sd", "sem", "ci95", "err"):
        if c in out:
            out[c] = out[c].round(6)
    return jsonify(_clean_for_json({
        "ok": True, "columns": list(out.columns),
        "rows": out.to_dict(orient="records")}))


# ── routes: settings ────────────────────────────────────────────────────────
@app.route("/api/config", methods=["GET", "POST"])
def config():
    if request.method == "POST":
        body = request.get_json(silent=True) or {}
        cfg = body.get("cfg")
        if isinstance(cfg, dict):
            # Only accept known keys so a stale settings file can't inject junk.
            for k, v in cfg.items():
                if k in DEFAULTS:
                    STATE["cfg"][k] = v
        st = body.get("styles")
        if isinstance(st, dict):
            for k, v in st.items():
                if isinstance(v, dict):
                    STATE["styles"].setdefault(k, {}).update(v)
        order = body.get("sample_order")
        if isinstance(order, list) and order:
            known = set(STATE["samples"])
            STATE["samples"] = [s for s in order if s in known] + \
                               [s for s in STATE["samples"] if s not in set(order)]
    return jsonify(_clean_for_json({
        "ok": True, "cfg": STATE["cfg"], "styles": STATE["styles"],
        "samples": STATE["samples"], "defaults": DEFAULTS,
        "palettes": {k: v for k, v in P.PALETTES.items()},
        "shapes": P.SHAPES, "dashes": P.DASHES,
        "filename": STATE["filename"],
    }))


@app.route("/api/config/reset", methods=["POST"])
def config_reset():
    STATE["cfg"] = dict(DEFAULTS)
    STATE["styles"] = {}
    return jsonify({"ok": True})


@app.route("/api/settings/export")
def settings_export():
    payload = {"app": "Kill Curve Studio", "version": 1,
               "cfg": STATE["cfg"], "styles": STATE["styles"],
               "sample_order": STATE["samples"]}
    buf = io.BytesIO(json.dumps(_clean_for_json(payload), indent=2).encode("utf-8"))
    return send_file(buf, mimetype="application/json", as_attachment=True,
                     download_name="killcurve-settings.json")


@app.route("/api/settings/import", methods=["POST"])
def settings_import():
    f = request.files.get("file")
    raw = f.read() if f is not None else (request.data or b"")
    try:
        payload = json.loads(raw.decode("utf-8"))
    except Exception as e:
        return jsonify({"ok": False, "error": f"Not a valid settings file: {e}"}), 400
    cfg = payload.get("cfg", {})
    applied = 0
    for k, v in (cfg.items() if isinstance(cfg, dict) else []):
        if k in DEFAULTS:
            STATE["cfg"][k] = v
            applied += 1
    styles = payload.get("styles", {})
    matched, unmatched = [], []
    if isinstance(styles, dict):
        current = set(STATE["samples"])
        for name, st in styles.items():
            if not isinstance(st, dict):
                continue
            STATE["styles"].setdefault(name, {}).update(st)
            (matched if name in current else unmatched).append(name)
    return jsonify(_clean_for_json({
        "ok": True, "applied": applied, "matched": matched, "unmatched": unmatched}))


# ── routes: figure + analysis ───────────────────────────────────────────────
@app.route("/api/figure")
def figure():
    try:
        fig, _ = _fig()
        return app.response_class(fig.to_json(), mimetype="application/json")
    except Exception as e:
        return jsonify({"ok": False, "error": str(e)}), 500


def _analysis_rows():
    """Per-sample metrics + optional control comparison. Shared by the JSON
    route and the CSV export so the two can never disagree."""
    cfg = STATE["cfg"]
    agg = _agg()
    if agg is None or agg.empty:
        return []

    ctrl = cfg.get("control_sample")
    ctrl_map = {}
    if ctrl and ctrl in set(agg["sample"]):
        c = agg[agg["sample"] == ctrl].sort_values("time")
        ctrl_map = dict(zip(c["time"].tolist(), c["mean"].tolist()))

    rows = []
    for name in _visible_samples(agg):
        sub = agg[agg["sample"] == name].sort_values("time")
        t = sub["time"].to_numpy(dtype=float)
        y = sub["mean"].to_numpy(dtype=float)
        rec = {"sample": name}
        rec.update(A.summarize_curve(
            t, y,
            window=int(cfg.get("growth_window", 4) or 4),
            drop_frac=float(cfg.get("lysis_drop_frac", 0.05) or 0.05),
            log_drop=float(cfg.get("log_drop", 3.0) or 3.0)))
        if ctrl_map and name != ctrl:
            yc = np.array([ctrl_map.get(tt, np.nan) for tt in t], dtype=float)
            cmp = A.compare_to_control(t, y, yc, log_drop=float(cfg.get("log_drop", 3.0) or 3.0))
            rec["max_log_reduction"] = cmp["max_log_reduction"]
            rec["final_log_reduction"] = cmp["final_log_reduction"]
            rec["t_at_max_log_reduction"] = cmp["t_at_max_log_reduction"]
        rows.append(rec)
    return rows


@app.route("/api/analysis")
def analysis_route():
    return jsonify(_clean_for_json({
        "ok": True, "rows": _analysis_rows(),
        "control": STATE["cfg"].get("control_sample")}))


@app.route("/api/fit", methods=["POST"])
def fit():
    """Fit a growth model per sample and return the fitted curves for overlay."""
    body = request.get_json(silent=True) or {}
    model = (body.get("model") or "logistic").lower()
    agg = _agg()
    if agg is None or agg.empty:
        return jsonify({"ok": True, "fits": []})
    fn = {"logistic": A.fit_logistic, "gompertz": A.fit_gompertz,
          "baranyi": A.fit_baranyi}.get(model, A.fit_logistic)
    fits = []
    for name in _visible_samples(agg):
        sub = agg[agg["sample"] == name].sort_values("time")
        res = fn(sub["time"].to_numpy(dtype=float), sub["mean"].to_numpy(dtype=float))
        fits.append({"sample": name, "model": model, "ok": bool(res.get("ok")),
                     "r2": res.get("r2"), "params": res.get("params", {}),
                     "t": res.get("t"), "fitted": res.get("fitted")})
    return jsonify(_clean_for_json({"ok": True, "fits": fits}))


# ── routes: export ──────────────────────────────────────────────────────────
@app.route("/api/export/<fmt>")
def export(fmt):
    cfg = STATE["cfg"]
    fmt = (fmt or "png").lower()
    w = float(request.args.get("width", cfg.get("export_width", 10)))
    h = float(request.args.get("height", cfg.get("export_height", 8)))
    dpi = float(request.args.get("dpi", cfg.get("export_dpi", 300)))
    stem = re.sub(r"[^A-Za-z0-9_-]+", "_", os.path.splitext(STATE["filename"] or "plot")[0]) or "plot"

    try:
        agg = _agg()
        samples = _visible_samples(agg)
        if fmt == "pptx":
            blob = P.export_pptx(agg, cfg, STATE["styles"], samples, w, h, min(dpi, 200))
            mime = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
        elif fmt == "gif":
            blob = P.export_gif(agg, cfg, STATE["styles"], samples, w, h,
                                min(dpi, 120), float(cfg.get("gif_fps", 1) or 1))
            mime = "image/gif"
        elif fmt == "csv":
            blob = _safe_frame_csv(agg)
            mime = "text/csv"
        else:
            fig, _ = _fig()
            blob = P.figure_to_bytes(fig, fmt, w, h, dpi)
            mime = {"png": "image/png", "jpeg": "image/jpeg", "jpg": "image/jpeg",
                    "pdf": "application/pdf", "svg": "image/svg+xml",
                    "tiff": "image/tiff", "html": "text/html",
                    "json": "application/json"}.get(fmt, "application/octet-stream")
    except Exception as e:
        msg = str(e)
        if "kaleido" in msg.lower() or "chrome" in msg.lower():
            msg = ("Static image export needs the 'kaleido' engine. Use the "
                   "Interactive HTML export, or reinstall to fetch kaleido.")
        return jsonify({"ok": False, "error": msg}), 500

    ext = {"jpg": "jpeg"}.get(fmt, fmt)
    return send_file(io.BytesIO(blob), mimetype=mime, as_attachment=True,
                     download_name=f"{stem}.{ext}")


@app.route("/api/analysis.csv")
def export_analysis():
    """Analysis table as CSV. Registered off /api/export/ so it cannot be
    shadowed by the /api/export/<fmt> converter."""
    rows = _analysis_rows()
    if not rows:
        return jsonify({"ok": False, "error": "No analysis rows to export."}), 400
    buf = io.BytesIO(_safe_frame_csv(pd.DataFrame(rows)))
    return send_file(buf, mimetype="text/csv", as_attachment=True,
                     download_name="killcurve-analysis.csv")


# ── session autosave ────────────────────────────────────────────────────────
@app.route("/api/session/save", methods=["POST"])
def session_save():
    """Persist settings (not data) so a crash or restart doesn't lose the look."""
    path = os.path.join(SESSION_DIR, "session.json")
    try:
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(_clean_for_json({"cfg": STATE["cfg"], "styles": STATE["styles"],
                                       "samples": STATE["samples"],
                                       "saved_at": time.time()}), fh, indent=2)
        return jsonify({"ok": True})   # path withheld: it contains the user name
    except Exception as e:
        return jsonify({"ok": False, "error": str(e)}), 500


@app.route("/api/session/restore", methods=["POST"])
def session_restore():
    path = os.path.join(SESSION_DIR, "session.json")
    if not os.path.exists(path):
        return jsonify({"ok": False, "error": "No saved session."}), 404
    try:
        with open(path, encoding="utf-8") as fh:
            payload = json.load(fh)
        for k, v in (payload.get("cfg") or {}).items():
            if k in DEFAULTS:
                STATE["cfg"][k] = v
        st = payload.get("styles") or {}
        if isinstance(st, dict):
            STATE["styles"].update(st)
        return jsonify({"ok": True})
    except Exception as e:
        return jsonify({"ok": False, "error": str(e)}), 500


@app.route("/api/sample-csv")
def sample_csv():
    """Download a correctly-shaped template so users can match the format."""
    txt = ("Time,Uninfected,Uninfected,Uninfected,Infected,Infected,Infected\n"
           "0,0.05,0.06,0.05,0.05,0.05,0.06\n"
           "20,0.11,0.12,0.10,0.11,0.10,0.11\n"
           "40,0.24,0.26,0.23,0.23,0.22,0.24\n"
           "60,0.52,0.55,0.50,0.48,0.46,0.49\n"
           "80,0.88,0.92,0.86,0.61,0.58,0.60\n"
           "100,1.10,1.14,1.08,0.31,0.29,0.32\n"
           "120,1.18,1.21,1.16,0.14,0.13,0.15\n"
           "140,1.20,1.23,1.19,0.08,0.07,0.09\n")
    return send_file(io.BytesIO(txt.encode()), mimetype="text/csv",
                     as_attachment=True, download_name="killcurve-template.csv")


@app.errorhandler(413)
def too_large(_):
    return jsonify({"ok": False,
                    "error": f"That file is larger than {MAX_UPLOAD_MB} MB."}), 413


def run(host="127.0.0.1", port=8765, open_browser=True):
    if host not in ("127.0.0.1", "localhost", "::1"):
        # The API is unauthenticated and holds the user's data. Binding it to a
        # reachable interface would expose it to the whole network.
        raise SystemExit(
            f"Refusing to bind to {host}: this app is unauthenticated and is "
            "designed for 127.0.0.1 only.")
    if open_browser:
        import threading
        threading.Timer(1.2, lambda: webbrowser.open(f"http://{host}:{port}")).start()
    print(f"\n  Kill Curve Studio  ->  http://{host}:{port}")
    print("  Everything runs on this machine. Close this window to stop.\n")
    app.run(host=host, port=port, debug=False, threaded=True, use_reloader=False)
