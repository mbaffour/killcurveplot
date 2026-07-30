"""Analytical checks for app/analysis.py.

Every expectation here is derived from the maths (or a hand-computed value),
never from running the implementation and pasting what it printed.
"""
import math
import os
import sys

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from app import analysis as A  # noqa: E402

FAIL = []


def check(name, cond, detail=""):
    if cond:
        print(f"  PASS  {name}")
    else:
        print(f"  FAIL  {name} {detail}")
        FAIL.append(f"{name} {detail}")


def close(a, b, tol=1e-6):
    return np.isfinite(a) and np.isfinite(b) and abs(a - b) <= tol * max(1.0, abs(b))


print("\n== growth rate ==")
# Pure exponential: OD = 0.01 * e^(0.02 t). mu is exactly 0.02 by construction.
t = np.arange(0, 200, 10.0)
y = 0.01 * np.exp(0.02 * t)
g = A.max_growth_rate(t, y, window=4)
check("mu_max on pure exponential == 0.02", close(g["mu"], 0.02, 1e-9), f"got {g['mu']}")
check("r2 == 1 on a perfect log-line", close(g["r2"], 1.0, 1e-9), f"got {g['r2']}")

# Doubling time must satisfy ln2/mu exactly.
check("doubling_time == ln2/mu", close(A.doubling_time(0.02), math.log(2) / 0.02),
      f"got {A.doubling_time(0.02)}")
check("doubling_time(0) is NaN", math.isnan(A.doubling_time(0.0)))
check("doubling_time(-x) is NaN", math.isnan(A.doubling_time(-0.5)))

# Base conversion: mu in log10 units is mu_ln / ln(10).
g10 = A.max_growth_rate(t, y, window=4, log_base="10")
check("log10 growth rate == mu/ln10", close(g10["mu"], 0.02 / math.log(10), 1e-9),
      f"got {g10['mu']}")

print("\n== lag time ==")
# Flat at 0.01 for 50 min, then clean exponential. Tangent hits ln(0.01) at t=50.
t2 = np.arange(0, 200, 5.0)
y2 = np.where(t2 < 50, 0.01, 0.01 * np.exp(0.03 * (t2 - 50)))
lam = A.lag_time(t2, y2, window=4)
check("lag ~= 50 for a 50-min lag", abs(lam - 50.0) <= 5.0, f"got {lam}")
# No lag: pure exponential from t=0 must give ~0.
check("lag ~= 0 with no lag phase", abs(A.lag_time(t, y, window=4)) <= 5.0,
      f"got {A.lag_time(t, y, window=4)}")

print("\n== carrying capacity / AUC ==")
cc = A.carrying_capacity([0, 1, 2, 3], [0.1, 0.9, 0.4, 0.2])
check("ymax picks the peak", close(cc["ymax"], 0.9), f"got {cc['ymax']}")
check("t_at_ymax picks its time", close(cc["t_at_ymax"], 1.0), f"got {cc['t_at_ymax']}")
# Trapezoid of a constant 2.0 over [0,10] is exactly 20.
check("auc of constant 2 over 0..10 == 20",
      close(A.auc([0, 5, 10], [2, 2, 2]), 20.0), f"got {A.auc([0,5,10],[2,2,2])}")
# Triangle 0->1 over [0,2] = 1.0
check("auc of a triangle == 1", close(A.auc([0, 1, 2], [0, 1, 0]), 1.0),
      f"got {A.auc([0,1,2],[0,1,0])}")

print("\n== lysis ==")
# Rise to 1.0 then fall. 5% below the running peak of 1.0 first occurs at 0.95.
tl = np.array([0, 10, 20, 30, 40, 50, 60], dtype=float)
yl = np.array([0.1, 0.4, 0.8, 1.0, 0.94, 0.5, 0.2])
on = A.lysis_onset(tl, yl, drop_frac=0.05)
check("lysis onset at first >=5% drop from peak", close(on["t_onset"], 40.0),
      f"got {on['t_onset']}")
check("peak OD recorded as 1.0", close(on["od_peak"], 1.0), f"got {on['od_peak']}")
# A monotonically rising culture never lyses.
check("no onset for a pure riser",
      math.isnan(A.lysis_onset(t, y)["t_onset"]))
ml = A.max_lysis_rate(tl, yl, window=3)
check("max lysis rate is negative", ml["rate"] < 0, f"got {ml['rate']}")

print("\n== kill metrics ==")
# 2-log kill: control 1.0, treated 0.01 -> exactly 2.
lr = A.log_reduction([0.01], [1.0])
check("log_reduction 1.0->0.01 == 2", close(lr[0], 2.0), f"got {lr[0]}")
check("log_reduction of equal values == 0", close(A.log_reduction([0.5], [0.5])[0], 0.0))
check("log_reduction with a zero is NaN (not inf)", math.isnan(A.log_reduction([0.0], [1.0])[0]))
check("percent_survival 0.25/1.0 == 25", close(A.percent_survival([0.25], [1.0])[0], 25.0))

# Time to 3-log drop, exact interpolation: start 1.0, hit 1e-3 partway.
tt = np.array([0.0, 10.0, 20.0])
yy = np.array([1.0, 1.0, 1e-4])          # log10 goes 0, 0, -4 between t=10 and 20
# target -3 lies 3/4 of the way from 0 to -4 => t = 10 + 0.75*10 = 17.5
check("time_to_log_drop interpolates to 17.5",
      close(A.time_to_log_drop(tt, yy, drop=3.0), 17.5), f"got {A.time_to_log_drop(tt,yy,3.0)}")
check("time_to_log_drop NaN when never reached",
      math.isnan(A.time_to_log_drop([0, 1, 2], [1.0, 0.9, 0.8], drop=3.0)))

print("\n== smoothing ==")
s = A.smooth([1, 2, 3, 4, 5], window=3)
check("smooth preserves length", len(s) == 5)
check("smooth interior is the mean of 3", close(s[2], 3.0), f"got {s[2]}")
check("smooth window=1 is identity", np.allclose(A.smooth([1, 5, 2], window=1), [1, 5, 2]))
# A constant series must survive smoothing unchanged.
check("smooth of a constant is constant", np.allclose(A.smooth([7, 7, 7, 7], 3), 7.0))

print("\n== model fits ==")
# Logistic data generated with known K=1.0, r=0.1, t0=50 must be recovered.
tf = np.arange(0, 120, 5.0)
yf = 1.0 / (1.0 + np.exp(-0.1 * (tf - 50)))
fl = A.fit_logistic(tf, yf)
check("logistic fit converges", fl.get("ok") is True)
if fl.get("ok"):
    check("logistic recovers K=1", close(fl["params"]["K"], 1.0, 1e-3), f"got {fl['params']['K']}")
    check("logistic recovers r=0.1", close(fl["params"]["r"], 0.1, 1e-3), f"got {fl['params']['r']}")
    check("logistic r2 ~ 1", fl["r2"] > 0.999, f"got {fl['r2']}")
fg = A.fit_gompertz(tf, yf)
check("gompertz returns a dict with r2 key", "r2" in fg)

print("\n== robustness (must not raise) ==")
for name, args in [
    ("empty", ([], [])),
    ("single point", ([1.0], [0.5])),
    ("all NaN", ([1, 2, 3], [float('nan')] * 3)),
    ("zeros", ([0, 1, 2], [0.0, 0.0, 0.0])),
    ("negatives", ([0, 1, 2], [-0.1, -0.2, -0.3])),
    ("duplicate times", ([1, 1, 1], [0.1, 0.2, 0.3])),
    ("unsorted", ([5, 1, 3], [0.3, 0.1, 0.2])),
    ("huge", ([0, 1, 2], [1e300, 1e300, 1e300])),
    ("tiny", ([0, 1, 2], [1e-300, 1e-300, 1e-300])),
    ("mismatched lengths", ([0, 1, 2, 3], [0.1, 0.2])),
    ("inf", ([0, 1, 2], [1.0, float('inf'), 2.0])),
]:
    try:
        A.summarize_curve(*args)
        check(f"summarize_curve survives {name}", True)
    except Exception as e:
        check(f"summarize_curve survives {name}", False, f"raised {type(e).__name__}: {e}")

# clean_xy contract
tc, yc = A.clean_xy([3, 1, 2], [0.3, 0.1, 0.2])
check("clean_xy sorts by time", list(tc) == [1, 2, 3] and list(yc) == [0.1, 0.2, 0.3])
tn, yn = A.clean_xy([1, 2, 3], [0.1, float("nan"), 0.3])
check("clean_xy drops NaN pairwise", len(tn) == 2 and list(tn) == [1, 3])

print("\n== summarize_curve shape ==")
s = A.summarize_curve(tl, yl)
for k in ["mu_max", "doubling_time", "lag_time", "y_max", "auc", "lysis_onset",
          "max_lysis_rate", "time_to_log_drop", "n_points"]:
    check(f"summary has {k}", k in s)
check("n_points is the cleaned count", s["n_points"] == 7, f"got {s['n_points']}")

print("\n" + "=" * 60)
print("\n== regressions: censoring, model space, edge bias ==")
# A well reading 0 is BELOW the limit of detection, not proof the target was
# reached. Both of these previously reported a full 3-log kill.
tz = np.array([0., 10., 20., 30., 40.])
check("shallow drop then non-detect is not a fabricated crossing",
      A.time_to_log_drop(tz, np.array([1.0, 0.9, 0.8, 0.0, 0.0]), drop=3.0) == 30.0)
check("non-detect on the first step is not reported at t=0",
      A.time_to_log_drop(tz, np.array([1.0, 0.0, 0.0, 0.0, 0.0]), drop=3.0) == 10.0)
_d = A.time_to_log_drop(tz, np.array([1.0, 0.0, 0.0, 0.0, 0.0]), drop=3.0, detail=True)
check("censoring is flagged, not silent", _d["censored"] is True and _d["observed"] is False)
# 1.0 -> 0.001 spans exactly 3 logs between t=20 and t=30, so by construction
# the interpolated crossing is t=30.
check("a real crossing still interpolates",
      A.time_to_log_drop(tz, np.array([1.0, 0.5, 0.1, 0.001, 0.0005]), drop=3.0) == 30.0)

# final_log_reduction must describe the FINAL timepoint, not the last finite one.
_c = A.compare_to_control(np.array([0., 10., 20., 30.]),
                          np.array([1.0, 0.1, 0.01, 0.0]),
                          np.array([1.0, 1.0, 1.0, 1.0]))
check("complete kill does not report the previous timepoint's value",
      not np.isfinite(_c["final_log_reduction"]))
check("the last measurable value is still available",
      abs(_c["last_measurable_log_reduction"] - 2.0) < 1e-9)

# Modified Gompertz is defined on ln(N/N0). Fitted to raw OD it gave mu the
# wrong units and did not converge at all. Recover a known mu.
_mu, _lam = 0.05, 20.0
_A = math.log(1000.0)
tg = np.linspace(0, 100, 25)
_lnrel = _A * np.exp(-np.exp((_mu * math.e / _A) * (_lam - tg) + 1.0))
_g = A.fit_gompertz(tg, 0.01 * np.exp(_lnrel))
check("gompertz converges on true-gompertz data", bool(_g.get("ok")))
check("gompertz recovers mu within 5%",
      bool(_g.get("ok")) and abs(_g["params"]["mu_max"] - _mu) / _mu < 0.05,
      f"got {_g.get('params', {}).get('mu_max')}")

# R^2 must be comparable across models, and undefined where it is undefined.
_yy = 0.05 * np.exp(0.06 * tg) / (1 + 0.05 * (np.exp(0.06 * tg) - 1) / 1.2) + 0.001
for _nm, _fn in (("logistic", A.fit_logistic), ("gompertz", A.fit_gompertz), ("baranyi", A.fit_baranyi)):
    _r = _fn(tg, _yy)
    check(f"{_nm} reports a comparable signal-space R2",
          np.isfinite(_r.get("r2_signal", float("nan"))))
_s2, _i2, _r2f, _tm2 = A._rolling_slopes(np.array([0., 1., 2., 3.]),
                                         A._safe_log(np.array([0.5, 0.5, 0.5, 0.5])), 4)
check("R2 of a zero-variance window is undefined, not 1.0", not np.isfinite(_r2f[0]))

# A symmetric smoother passes a straight line through unchanged.
check("smoothing preserves a linear ramp exactly",
      np.allclose(A.smooth(np.arange(20, dtype=float), window=5), np.arange(20, dtype=float)))
check("smoothing does not drag the endpoint inward",
      A.smooth(np.array([1., 1., 1., 1., 1., 1., 1., 10.]), window=5)[-1] == 10.0)

print("\n" + "=" * 60)

if FAIL:
    print(f"{len(FAIL)} FAILURE(S):")
    for f in FAIL:
        print("  -", f)
    sys.exit(1)
print("ALL ANALYSIS TESTS PASSED")
