"""OD-layer regressions: blank subtraction, normalisation guards, axis bounds.

Every case here is a way the app could draw a confident, tidy-looking curve that
misrepresents the measurement.
"""
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from app import data as D  # noqa: E402
from app import plotting as P  # noqa: E402
from app.server import DEFAULTS, _enforce_scale_interlock  # noqa: E402

FAIL = []


def check(name, cond, detail=""):
    if cond:
        print(f"  PASS  {name}")
    else:
        print(f"  FAIL  {name} {detail}")
        FAIL.append(f"{name} {detail}")


def long(rows):
    return pd.DataFrame([{"time": t, "sample": s, "value": v} for t, s, v in rows])


print("\n== normalising to t0 after blank subtraction ==")
# A t0 culture reading at or below the media blank is routine. Dividing by the
# resulting near-zero baseline used to invert the curve: a genuine 6x rise came
# out as -99x, and on the default log axis the sample collapsed to a single dot.
rise = [(0, "Trt", .0525), (30, "Trt", .139), (60, "Trt", .3025)]
for blank, tag in ((.055, "baseline goes negative"), (.0520, "baseline stays positive but tiny")):
    lg = long([(t, "Blank", blank) for t in (0, 30, 60)] + rise)
    a = D.aggregate(lg, blank_sample="Blank", normalize="t0")
    m = a["mean"].dropna()
    check(f"no runaway fold change when {tag}",
          m.empty or m.abs().max() < 50, f"max |fold| = {m.abs().max() if len(m) else 0}")
    check(f"user is told why ({tag})", bool(a.attrs.get("warnings")))

print("\n== a legitimate normalisation is untouched ==")
a = D.aggregate(long([(0, "S", .05), (30, "S", .20), (60, "S", .80), (90, "S", 1.00)]),
                normalize="t0")
check("starts at 1.0", abs(a["mean"].iloc[0] - 1.0) < 1e-9, f"got {a['mean'].iloc[0]}")
check("ends at 20x", abs(a["mean"].iloc[-1] - 20.0) < 1e-9, f"got {a['mean'].iloc[-1]}")
check("no spurious warning", not a.attrs.get("warnings"), f"got {a.attrs.get('warnings')}")

print("\n== a gap in the blank must not spike one timepoint ==")
# Subtracting 0.0 where the blank has no reading leaves that point carrying the
# full blank while its neighbours are corrected, so it stands ~10x above them.
rows = [(t, "Blank", .050) for t in (0, 90, 120)]
rows += [(0, "Lysed", .0550), (60, "Lysed", .0552), (90, "Lysed", .0548), (120, "Lysed", .0551)]
a = D.aggregate(long(rows), blank_sample="Blank")
v = a.set_index("time")["mean"]
check("gap timepoint is interpolated, not left unblanked",
      v[60] < 3 * max(v[0], v[90], v[120]), f"t=60 -> {v[60]:.4f} vs ~{v[0]:.4f}")
check("interpolation is disclosed", bool(a.attrs.get("warnings")))

print("\n== z-score cannot share a figure with a log axis ==")
# Z-score is mean-centred by construction, so ~half of every curve is negative
# and a log axis drops it — while the axis note still reads "— z-score".
cfg = dict(DEFAULTS, normalize="zscore", y_scale_type="log")
_enforce_scale_interlock(cfg)
check("y axis forced to linear", cfg["y_scale_type"] == "linear", f"got {cfg['y_scale_type']}")
cfg = dict(DEFAULTS, normalize="t0", y_scale_type="log")
_enforce_scale_interlock(cfg)
check("log axis left alone for unsigned transforms", cfg["y_scale_type"] == "log")

print("\n== axis bounds ==")
ax = P._axis_kwargs(dict(DEFAULTS, y_min=None, y_max=1.5), "y")
check("a y-max-only bound is honoured, not discarded", "range" in ax, "range key absent")
check("free end still autoranges", ax.get("autorange") == "max", f"got {ax.get('autorange')}")
ax = P._axis_kwargs(dict(DEFAULTS, y_min=0.001, y_max=None), "y")
check("a y-min-only bound is honoured", ax.get("range") == [-3.0, None], f"got {ax.get('range')}")
ax = P._axis_kwargs(dict(DEFAULTS, y_min=0, y_max=1.5), "y")
check("y_min=0 on log does not become a 12-decade axis",
      ax["range"][0] is None, f"got {ax['range']}")
ax = P._axis_kwargs(dict(DEFAULTS, y_min=0.001, y_max=1.0), "y")
check("positive log bounds convert to exponents", ax["range"] == [-3.0, 0.0], f"got {ax['range']}")

print("\n== the figure declares what was done to the data ==")
check("blank subtraction named on the axis",
      "blank" in P.transform_note(dict(DEFAULTS, blank_sample="Blank")))

print("\n== the plot area does not depend on sample names ==")
agg = pd.DataFrame([{"sample": s, "time": t, "mean": .1 + t / 100, "err": .01, "n": 3}
                    for s in ("A", "B") for t in range(0, 60, 10)])
def margins(label):
    fig = P.build_figure(agg, dict(DEFAULTS), {"A": {"label": "WT"}, "B": {"label": label}},
                         samples=["A", "B"])
    return fig.layout.margin.l, fig.layout.margin.r
short, long_ = margins("MOI 1"), margins("Antibiotic (ciprofloxacin, 4x MIC, pooled)")
check("margins identical for short and long legend labels", short == long_,
      f"{short} vs {long_}")

print("\n" + "=" * 60)
if FAIL:
    print(f"{len(FAIL)} FAILURE(S):")
    for f in FAIL:
        print("  -", f)
    sys.exit(1)
print("ALL OD TESTS PASSED")
