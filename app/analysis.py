"""
Kill / lysis / growth curve analytics.

Pure functions over numpy arrays — no plotting, no Flask, no globals — so every
number here is unit-testable without a browser.

The R app this replaces only *drew* curves. Everything in this module is the
part a bench scientist actually has to compute by hand afterwards: growth rate,
doubling time, lag, lysis onset, log reduction, AUC, and model fits.

Conventions
-----------
* ``t`` is time (any unit; minutes by default) and must be increasing.
* ``y`` is the measured signal (OD/A550, CFU/mL, RLU...). NaNs are allowed and
  are dropped pairwise.
* Growth-rate maths is done on natural-log OD. mu is per time-unit.
"""
from __future__ import annotations

import math
import numpy as np

__all__ = [
    "clean_xy", "max_growth_rate", "doubling_time", "lag_time", "carrying_capacity",
    "auc", "lysis_onset", "max_lysis_rate", "log_reduction", "time_to_log_drop",
    "percent_survival", "smooth", "fit_logistic", "fit_gompertz", "fit_baranyi",
    "summarize_curve", "compare_to_control",
]

_EPS = 1e-12


# ─────────────────────────── helpers ───────────────────────────
def clean_xy(t, y):
    """Drop NaN/inf pairs and sort by time. Returns (t, y) float arrays."""
    t = np.asarray(t, dtype=float)
    y = np.asarray(y, dtype=float)
    n = min(len(t), len(y))
    t, y = t[:n], y[:n]
    ok = np.isfinite(t) & np.isfinite(y)
    t, y = t[ok], y[ok]
    if len(t) > 1:
        order = np.argsort(t, kind="stable")
        t, y = t[order], y[order]
    return t, y


def _safe_log(y):
    """ln of a signal, with non-positive readings marked missing (NaN).

    A blanked well can read 0 or slightly negative. Flooring those to a tiny
    number (an earlier approach here) put them ~14-18 ln-units below their
    neighbours, which manufactured an enormous artificial slope in any window
    that touched them. NaN keeps the time base intact — the array is the same
    length — while making such windows unusable rather than wrong.
    """
    y = np.asarray(y, dtype=float)
    out = np.full(y.shape, np.nan)
    ok = np.isfinite(y) & (y > 0)
    out[ok] = np.log(y[ok])
    return out


def smooth(y, window=3, method="rolling"):
    """Rolling mean/median smoother, centred and symmetric at the edges.

    Two things an earlier version got wrong:

    * **Edge bias.** Clipping the window at the array bounds made it one-sided
      near the ends (the last point averaged only itself and its predecessors),
      which pulls the baseline and the endpoint toward the interior. Kill curves
      are read at exactly those two places — t0 baseline and final value — so the
      window is now shrunk *symmetrically*: at distance d from an edge it uses
      min(half, d) on BOTH sides. Less smoothing at the edges, but unbiased.
    * **Speed.** It was a per-point Python loop, which stalled the UI for seconds
      on large plates. The mean path is now a cumulative-sum sweep.
    """
    y = np.asarray(y, dtype=float)
    w = int(window)
    if w <= 1 or y.size == 0:
        return y.copy()
    n = y.size
    w = min(w, n)
    if w % 2 == 0:                      # force odd so the window is centred
        w -= 1
    if w <= 1:
        return y.copy()
    half = w // 2

    idx = np.arange(n)
    # Symmetric half-width: never reach past either end.
    h = np.minimum(half, np.minimum(idx, n - 1 - idx))
    lo, hi = idx - h, idx + h + 1       # hi is exclusive

    if method == "median":
        out = np.empty(n, dtype=float)
        for i in range(n):
            seg = y[lo[i]:hi[i]]
            seg = seg[np.isfinite(seg)]
            out[i] = np.median(seg) if seg.size else np.nan
        return out

    finite = np.isfinite(y)
    vals = np.where(finite, y, 0.0)
    cs = np.concatenate(([0.0], np.cumsum(vals)))
    cn = np.concatenate(([0.0], np.cumsum(finite.astype(float))))
    total = cs[hi] - cs[lo]
    count = cn[hi] - cn[lo]
    with np.errstate(divide="ignore", invalid="ignore"):
        out = np.where(count > 0, total / count, np.nan)
    return out


# ───────────────────── growth-phase metrics ─────────────────────
def _rolling_slopes(t, y, w):
    """Slope, intercept and R^2 of every length-w window, vectorised.

    Closed-form OLS from cumulative sums instead of one polyfit per window:
    for 30 samples x 300 points this is the difference between ~7 s and ~0.1 s.
    Windows whose time span is zero (duplicate timestamps) come back as NaN.
    """
    n = t.size
    k = n - w + 1
    if k <= 0:
        return (np.empty(0),) * 4

    def _wsum(a):
        c = np.concatenate(([0.0], np.cumsum(a)))
        return c[w:] - c[:-w]

    # Cumulative sums propagate a single NaN to every later window, so count the
    # finite points per window and invalidate any window that is not complete.
    finite = np.isfinite(y) & np.isfinite(t)
    n_ok = _wsum(finite.astype(float))
    y = np.where(finite, y, 0.0)
    t_ = np.where(finite, t, 0.0)

    Sx, Sy = _wsum(t_), _wsum(y)
    Sxx, Sxy, Syy = _wsum(t_ * t_), _wsum(t_ * y), _wsum(y * y)

    den = w * Sxx - Sx * Sx
    with np.errstate(divide="ignore", invalid="ignore"):
        slope = np.where(np.abs(den) > _EPS, (w * Sxy - Sx * Sy) / den, np.nan)
        icpt = (Sy - slope * Sx) / w
        # R^2 = (explained sum of squares) / (total sum of squares)
        ss_tot = Syy - Sy * Sy / w
        ss_res = Syy - icpt * Sy - slope * Sxy
        # A window with zero variance has no defined R^2. Reporting 1.0 there
        # (an earlier choice) told the caller a flat line was a perfect
        # exponential fit — exactly backwards for a "is this real growth?" test.
        r2 = np.where(np.abs(ss_tot) > _EPS, 1.0 - ss_res / ss_tot, np.nan)
    incomplete = n_ok < w
    slope = np.where(incomplete, np.nan, slope)
    r2 = np.where(incomplete, np.nan, r2)
    t_mid = np.where(incomplete, np.nan, Sx / w)
    return slope, icpt, r2, t_mid


def max_growth_rate(t, y, window=4, log_base="e"):
    """Maximum specific growth rate mu_max from a sliding log-linear fit.

    Returns dict(mu, t_mid, r2, i0, i1) — mu in ln-units per time-unit, or
    NaNs when there are too few points to fit any window.
    """
    t, y = clean_xy(t, y)
    out = {"mu": float("nan"), "t_mid": float("nan"), "r2": float("nan"),
           "i0": -1, "i1": -1}
    if t.size < 2:
        return out
    w = max(2, min(int(window), t.size))
    ly = _safe_log(y)
    slope, _icpt, r2, t_mid = _rolling_slopes(t, ly, w)
    if slope.size == 0 or not np.any(np.isfinite(slope)):
        return out
    i = int(np.nanargmax(slope))
    mu = float(slope[i])
    r2i = float(np.clip(r2[i], -1e9, 1.0))

    # Is this a real exponential phase, or the best-fitting slice of flat noise?
    # A dead culture sitting on the detection limit will always have SOME
    # steepest window; reporting a doubling time for it is a fabricated number.
    # Require both a decent linear fit and a meaningful rise across the window.
    span = float(mu) * (float(t[min(i + w - 1, t.size - 1)]) - float(t[i]))
    reliable = bool(np.isfinite(mu) and mu > 0 and r2i >= 0.80 and span >= 0.10)

    if log_base == "10":
        mu /= math.log(10)
    elif log_base == "2":
        mu /= math.log(2)
    return {"mu": mu, "t_mid": float(t_mid[i]), "r2": r2i,
            "i0": i, "i1": i + w - 1, "reliable": reliable}


def doubling_time(mu):
    """Doubling time from mu (ln-units/time). NaN when the culture isn't growing."""
    if mu is None or not np.isfinite(mu) or mu <= 0:
        return float("nan")
    return math.log(2) / mu


def lag_time(t, y, window=4):
    """Lag = where the max-growth tangent crosses the initial ln(OD).

    The classic geometric definition (Buchanan/Zwietering): extrapolate the
    exponential-phase tangent back to the starting density.
    """
    t, y = clean_xy(t, y)
    if t.size < 3:
        return float("nan")
    g = max_growth_rate(t, y, window=window)
    mu = g["mu"]
    if not np.isfinite(mu) or mu <= _EPS:
        return float("nan")
    ly = _safe_log(y)
    i0, i1 = g["i0"], g["i1"]
    tt, yy = t[i0:i1 + 1], ly[i0:i1 + 1]
    icpt = yy.mean() - mu * tt.mean()
    lam = (ly[0] - icpt) / mu
    if not np.isfinite(lam):
        return float("nan")
    return float(min(max(lam, 0.0), float(t[-1])))   # clamp into the observed window


def carrying_capacity(t, y):
    """Peak signal and when it occurs."""
    t, y = clean_xy(t, y)
    if y.size == 0:
        return {"ymax": float("nan"), "t_at_ymax": float("nan")}
    k = int(np.argmax(y))
    return {"ymax": float(y[k]), "t_at_ymax": float(t[k])}


def auc(t, y, log_scale=False):
    """Trapezoidal area under the curve; optionally of ln(y)."""
    t, y = clean_xy(t, y)
    if t.size < 2:
        return float("nan")
    vals = _safe_log(y) if log_scale else y
    return float(np.trapezoid(vals, t)) if hasattr(np, "trapezoid") else float(np.trapz(vals, t))


# ────────────────────── lysis / kill metrics ──────────────────────
def lysis_onset(t, y, drop_frac=0.05, min_window=2):
    """First time the signal falls ``drop_frac`` below its running peak.

    This is deliberately peak-relative rather than absolute: a culture that
    lyses from OD 1.2 and one that lyses from OD 0.3 both register at the same
    fractional drop.
    """
    t, y = clean_xy(t, y)
    if t.size < max(2, min_window):
        return {"t_onset": float("nan"), "od_peak": float("nan"), "i": -1}
    peak, peak_i = y[0], 0
    for i in range(1, y.size):
        if y[i] > peak:
            peak, peak_i = y[i], i
        elif peak > _EPS and (peak - y[i]) / peak >= drop_frac and i > peak_i:
            return {"t_onset": float(t[i]), "od_peak": float(peak), "i": i}
    return {"t_onset": float("nan"), "od_peak": float(peak), "i": -1}


def max_lysis_rate(t, y, window=3):
    """Steepest *decline* (most negative slope of ln y) and when it happens."""
    t, y = clean_xy(t, y)
    out = {"rate": float("nan"), "t_mid": float("nan")}
    if t.size < 2:
        return out
    w = max(2, min(int(window), t.size))
    slope, _i, _r2, t_mid = _rolling_slopes(t, _safe_log(y), w)
    if slope.size == 0 or not np.any(np.isfinite(slope)):
        return out
    i = int(np.nanargmin(slope))
    if not np.isfinite(slope[i]) or slope[i] >= 0:
        return out          # nothing is declining: there is no lysis rate
    return {"rate": float(slope[i]), "t_mid": float(t_mid[i])}


def log_reduction(y_treated, y_control):
    """log10(control / treated), elementwise. The standard kill-curve readout.

    Positive = kill. Non-positive inputs give NaN rather than +/-inf, so a
    single blanked well can't poison a whole series.
    """
    a = np.asarray(y_treated, dtype=float)
    b = np.asarray(y_control, dtype=float)
    n = min(a.size, b.size)
    a, b = a[:n], b[:n]
    out = np.full(n, np.nan)
    ok = np.isfinite(a) & np.isfinite(b) & (a > 0) & (b > 0)
    out[ok] = np.log10(b[ok] / a[ok])
    return out


def time_to_log_drop(t, y, drop=3.0, baseline=None, detail=False):
    """First time the signal falls ``drop`` log10 below baseline (default y[0]).

    Two distinct cases, which an earlier version of this function conflated:

    * **Observed crossing** — the signal passes the target while still
      measurable. Linearly interpolated in log10 space between the bracketing
      points, so a 3-log kill reported at t=47.5 means the crossing, not the
      next sampling time.
    * **Censored (below limit of detection)** — the well reads 0 or negative.
      That is *not* evidence the target was reached; it only says the signal is
      somewhere below the detection limit. Treating it as -inf made any
      non-positive reading satisfy any target, so a culture that had dropped
      0.1 log and then blanked reported a full 3-log kill, and one that blanked
      on the first read reported the kill at t=0 — before anything happened.
      We now report the first non-detect time and mark the result censored.

    ``detail=True`` returns a dict with the flag; the default scalar return is
    kept so existing callers and tests are unaffected.
    """
    t, y = clean_xy(t, y)
    nan = float("nan")
    res = lambda v, c=False, obs=False: ({"t": v, "censored": c, "observed": obs}
                                         if detail else v)
    if t.size == 0:
        return res(nan)
    b = float(y[0]) if baseline is None else float(baseline)
    if not np.isfinite(b) or b <= 0:
        return res(nan)
    target = math.log10(b) - float(drop)

    prev_lv, prev_t = (math.log10(y[0]) if y[0] > 0 else None), float(t[0])
    for i in range(1, y.size):
        yi = float(y[i])
        if yi <= 0:
            # Undetectable. The true crossing is at or before t[i], but it is
            # not interpolatable — report the censoring time, flagged.
            return res(float(t[i]), c=True)
        lv = math.log10(yi)
        if lv <= target:
            if prev_lv is None or abs(prev_lv - lv) <= _EPS:
                return res(float(t[i]), obs=True)
            frac = min(max((prev_lv - target) / (prev_lv - lv), 0.0), 1.0)
            return res(float(prev_t + frac * (float(t[i]) - prev_t)), obs=True)
        prev_lv, prev_t = lv, float(t[i])
    return res(nan)


def percent_survival(y_treated, y_control):
    """100 * treated/control, elementwise."""
    a = np.asarray(y_treated, dtype=float)
    b = np.asarray(y_control, dtype=float)
    n = min(a.size, b.size)
    a, b = a[:n], b[:n]
    out = np.full(n, np.nan)
    ok = np.isfinite(a) & np.isfinite(b) & (np.abs(b) > _EPS)
    out[ok] = 100.0 * a[ok] / b[ok]
    return out


# ─────────────────────────── model fits ───────────────────────────
def _fit(model, t, y, p0, bounds=None):
    """scipy curve_fit wrapper that degrades to NaNs instead of raising."""
    t, y = clean_xy(t, y)
    base = {"params": {}, "r2": float("nan"), "ok": False, "fitted": None}
    if t.size < 4:
        return base
    try:
        from scipy.optimize import curve_fit
    except Exception:
        return base
    try:
        kw = {"maxfev": 20000}
        if bounds is not None:
            kw["bounds"] = bounds
        popt, _ = curve_fit(model, t, y, p0=p0, **kw)
        pred = model(t, *popt)
        ss_res = float(np.sum((y - pred) ** 2))
        ss_tot = float(np.sum((y - y.mean()) ** 2))
        r2 = 1.0 - ss_res / ss_tot if ss_tot > _EPS else float("nan")
        return {"popt": [float(v) for v in popt], "r2": r2, "ok": True,
                "fitted": pred.tolist(), "t": t.tolist()}
    except Exception:
        return base


def _r2_signal(res, t_raw, y_raw):
    """R^2 of a completed fit measured in SIGNAL units, whatever space it fitted in.

    Each model is fitted in whichever space it is defined in — logistic on raw
    signal, Gompertz on ln(N/N0), Baranyi on ln y — so their native R^2 values
    are not comparable. Ranking them side by side made the log-space fits look
    best almost automatically (Baranyi scored a flat 1.0 against logistic's
    0.99999 on the same data). This adds one common yardstick.
    """
    if not res.get("ok") or not res.get("fitted"):
        return res
    try:
        tf = np.asarray(res.get("t", t_raw), dtype=float)
        pred = np.asarray(res["fitted"], dtype=float)
        tr, yr = clean_xy(t_raw, y_raw)
        obs = np.interp(tf, tr, yr) if tf.size != yr.size else yr
        ok = np.isfinite(obs) & np.isfinite(pred)
        if ok.sum() >= 2:
            o, p = obs[ok], pred[ok]
            sst = float(np.sum((o - o.mean()) ** 2))
            ssr = float(np.sum((o - p) ** 2))
            res["r2_signal"] = (1.0 - ssr / sst) if sst > _EPS else float("nan")
    except Exception:
        pass
    return res


def fit_logistic(t, y):
    """Logistic growth: y = K / (1 + exp(-r (t - t0)))."""
    t_, y_ = clean_xy(t, y)
    if t_.size < 4:
        return {"ok": False, "r2": float("nan"), "params": {}}
    K0 = float(max(y_.max(), _EPS)); r0 = 0.1; t00 = float(np.median(t_))

    def m(x, K, r, t0):
        return K / (1.0 + np.exp(-np.clip(r * (x - t0), -700, 700)))

    res = _fit(m, t_, y_, p0=[K0, r0, t00])
    if res.get("ok"):
        K, r, t0 = res["popt"]
        res["params"] = {"K": K, "r": r, "t_mid": t0,
                         "doubling_time": doubling_time(r)}
        res["r2_space"] = "signal"
    return _r2_signal(res, t_, y_)


def fit_gompertz(t, y):
    """Modified Gompertz (Zwietering 1990).

    The model is defined on the LOG-RELATIVE population,
    ``L(t) = ln(N(t)/N0) = A exp(-exp(mu e / A (lam - t) + 1))``,
    where ``A = ln(Nmax/N0)`` and ``mu`` is in ln-units per time.

    Fitting it to raw OD instead (an earlier version here) gave ``mu`` units of
    OD/time, so the reported doubling time ln(2)/mu was meaningless — and in
    practice the fit simply failed: on data generated from a true modified
    Gompertz it did not converge at all. We fit in log space and transform the
    prediction back to signal units for plotting.
    """
    t_, y_ = clean_xy(t, y)
    if t_.size < 4:
        return {"ok": False, "r2": float("nan"), "params": {}}
    pos = y_ > 0
    if pos.sum() < 4:
        return {"ok": False, "r2": float("nan"), "params": {}}
    t_, y_ = t_[pos], y_[pos]
    N0 = float(y_[0])
    L = np.log(y_ / N0)                      # ln(N/N0): starts at 0 by construction
    A0 = float(max(L.max(), 1e-3)); mu0 = 0.05; lam0 = float(t_.min())

    def m(x, A, mu, lam):
        z = (mu * math.e / max(A, _EPS)) * (lam - x) + 1.0
        return A * np.exp(-np.exp(np.clip(z, -700, 700)))

    res = _fit(m, t_, L, p0=[A0, mu0, lam0])
    if res.get("ok"):
        A, mu, lam = res["popt"]
        res["params"] = {"A_ln": A, "mu_max": mu, "lag": lam,
                         "N_max": float(N0 * math.exp(A)) if abs(A) < 700 else float("nan"),
                         "doubling_time": doubling_time(mu)}
        res["r2_space"] = "ln(N/N0)"
        # Back to signal units so the overlay sits on the plotted curve.
        res["fitted"] = (N0 * np.exp(np.clip(np.asarray(res["fitted"], float), -700, 700))).tolist()
        return _r2_signal(res, t_, y_)
    return res


def fit_baranyi(t, y):
    """Baranyi-Roberts, simplified (no explicit q0), fitted on ln y."""
    t_, y_ = clean_xy(t, y)
    if t_.size < 5:
        return {"ok": False, "r2": float("nan"), "params": {}}
    ly = _safe_log(y_)
    y0_, ymax_ = float(ly[0]), float(ly.max())
    mu0 = 0.05; lam0 = float(t_.min())

    def m(x, y0, ymax, mu, lam):
        A = x + (1.0 / max(mu, _EPS)) * np.log(
            np.exp(np.clip(-mu * x, -700, 700)) +
            np.exp(np.clip(-mu * lam, -700, 700)) -
            np.exp(np.clip(-mu * (x + lam), -700, 700)))
        num = np.exp(np.clip(mu * A, -700, 700)) - 1.0
        den = np.exp(np.clip(ymax - y0, -700, 700))
        return y0 + mu * A - np.log1p(np.maximum(num / np.maximum(den, _EPS), -0.999999))

    res = _fit(m, t_, ly, p0=[y0_, ymax_, mu0, lam0])
    if res.get("ok"):
        y0, ymax, mu, lam = res["popt"]
        res["params"] = {"y0": y0, "y_max": ymax, "mu_max": mu, "lag": lam,
                         "doubling_time": doubling_time(mu)}
        res["fitted"] = np.exp(np.asarray(res["fitted"], dtype=float)).tolist()
        res["r2_space"] = "ln y"
        return _r2_signal(res, t_, y_)
    return res


# ───────────────────────── orchestration ─────────────────────────
def summarize_curve(t, y, window=4, drop_frac=0.05, log_drop=3.0):
    """Every single-curve metric in one dict — what the Analysis tab renders."""
    t, y = clean_xy(t, y)
    g = max_growth_rate(t, y, window=window)
    cc = carrying_capacity(t, y)
    on = lysis_onset(t, y, drop_frac=drop_frac)
    ml = max_lysis_rate(t, y)
    reliable = bool(g.get("reliable"))
    return {
        "n_points": int(t.size),
        "growth_reliable": reliable,
        "t_start": float(t[0]) if t.size else float("nan"),
        "t_end": float(t[-1]) if t.size else float("nan"),
        "y_start": float(y[0]) if y.size else float("nan"),
        "y_end": float(y[-1]) if y.size else float("nan"),
        # Withheld rather than guessed when the fit does not support it — a
        # blank "—" is honest, an invented doubling time is not.
        "mu_max": g["mu"] if reliable else float("nan"),
        "mu_r2": g["r2"],
        "t_at_mu_max": g["t_mid"] if reliable else float("nan"),
        "doubling_time": doubling_time(g["mu"]) if reliable else float("nan"),
        "lag_time": lag_time(t, y, window=window) if reliable else float("nan"),
        "y_max": cc["ymax"], "t_at_y_max": cc["t_at_ymax"],
        "auc": auc(t, y),
        "auc_log": auc(t, y, log_scale=True),
        "lysis_onset": on["t_onset"], "od_at_lysis": on["od_peak"],
        "max_lysis_rate": ml["rate"], "t_at_max_lysis": ml["t_mid"],
        "time_to_log_drop": time_to_log_drop(t, y, drop=log_drop),
        "log_drop_target": float(log_drop),
    }


def compare_to_control(t, y, y_ctrl, log_drop=3.0):
    """Treated-vs-control readouts: log reduction, % survival, endpoint deltas."""
    t = np.asarray(t, dtype=float)
    lr = log_reduction(y, y_ctrl)
    ps = percent_survival(y, y_ctrl)
    finite = lr[np.isfinite(lr)]
    return {
        "log_reduction": lr.tolist(),
        "percent_survival": ps.tolist(),
        "max_log_reduction": float(np.nanmax(lr)) if finite.size else float("nan"),
        "t_at_max_log_reduction": (float(t[int(np.nanargmax(lr))])
                                   if finite.size and t.size == lr.size else float("nan")),
        # The value AT the final timepoint, not merely the last one that happened
        # to be finite. A complete kill makes the last point NaN (treated=0), and
        # reporting the previous timepoint there silently understated the result:
        # a well killed to zero was reported as its second-to-last, smaller value.
        "final_log_reduction": float(lr[-1]) if lr.size else float("nan"),
        "final_log_reduction_censored": bool(
            lr.size and not np.isfinite(lr[-1])
            and len(y) and np.isfinite(np.asarray(y, float)[-1])
            and float(np.asarray(y, float)[-1]) <= 0),
        "last_measurable_log_reduction": (float(finite[-1]) if finite.size else float("nan")),
        "time_to_log_drop_vs_control": time_to_log_drop(
            t, y, drop=log_drop,
            baseline=(float(y_ctrl[0]) if len(y_ctrl) and np.isfinite(y_ctrl[0]) else None)),
    }
