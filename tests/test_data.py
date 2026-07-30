"""Data-layer checks: layout detection, replicate handling, stats."""
import math
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from app import data as D  # noqa: E402

FAIL = []


def check(name, cond, detail=""):
    if cond:
        print(f"  PASS  {name}")
    else:
        print(f"  FAIL  {name} {detail}")
        FAIL.append(f"{name} {detail}")


print("\n== wide format with duplicate-name replicates ==")
csv = b"Time,WT,WT,WT,Mutant,Mutant\n0,0.05,0.06,0.04,0.05,0.03\n30,0.12,0.15,0.10,0.08,0.06\n"
df = D.load_table(csv, "x.csv")
check("duplicate headers preserved (not WT.1)", list(df.columns) == ["Time", "WT", "WT", "WT", "Mutant", "Mutant"],
      f"got {list(df.columns)}")
lay = D.detect_layout(df)
check("detects wide", lay["layout"] == "wide", f"got {lay['layout']}")
check("time column is Time", lay["time"] == "Time")
check("samples are WT+Mutant (deduped)", lay["samples"] == ["WT", "Mutant"], f"got {lay['samples']}")

lg = D.to_long(df, lay)
check("long has 10 rows (5 cols x 2 times)", len(lg) == 10, f"got {len(lg)}")
check("WT has 3 reps at t=0",
      len(lg[(lg["sample"] == "WT") & (lg["time"] == 0)]) == 3)
check("rep numbering starts at 1",
      sorted(lg[(lg["sample"] == "WT") & (lg["time"] == 0)]["rep"]) == [1, 2, 3])

agg = D.aggregate(lg, error_type="sd")
wt0 = agg[(agg["sample"] == "WT") & (agg["time"] == 0)].iloc[0]
check("WT t=0 mean == 0.05", abs(wt0["mean"] - 0.05) < 1e-12, f"got {wt0['mean']}")
check("WT t=0 n == 3", wt0["n"] == 3)
# sd of [0.05,0.06,0.04] = 0.01 exactly (sample sd)
check("WT t=0 sd == 0.01", abs(wt0["sd"] - 0.01) < 1e-12, f"got {wt0['sd']}")
# sem = sd/sqrt(3)
check("WT t=0 sem == sd/sqrt(3)", abs(wt0["sem"] - 0.01 / math.sqrt(3)) < 1e-12, f"got {wt0['sem']}")

print("\n== long format ==")
csv2 = b"Time,Sample,OD\n0,WT,0.05\n0,WT,0.06\n0,Mut,0.04\n30,WT,0.12\n30,Mut,0.08\n"
df2 = D.load_table(csv2, "y.csv")
lay2 = D.detect_layout(df2)
check("detects long", lay2["layout"] == "long", f"got {lay2['layout']}")
check("group col is Sample", lay2["group"] == "Sample", f"got {lay2['group']}")
check("value col is OD", lay2["value"] == "OD", f"got {lay2['value']}")
lg2 = D.to_long(df2, lay2)
check("long->long keeps 5 rows", len(lg2) == 5, f"got {len(lg2)}")
check("WT t=0 has 2 reps", len(lg2[(lg2["sample"] == "WT") & (lg2["time"] == 0)]) == 2)

print("\n== single replicate => sd 0, not NaN ==")
one = D.aggregate(D.to_long(D.load_table(b"Time,A\n0,0.1\n10,0.2\n", "z.csv")))
check("sd is 0 for n=1", float(one["sd"].iloc[0]) == 0.0, f"got {one['sd'].iloc[0]}")
check("err is finite for n=1", np.isfinite(one["err"]).all())
check("ci95 is 0 for n=1", float(one["ci95"].iloc[0]) == 0.0)

print("\n== 95% CI uses t, not 1.96 ==")
# n=3 -> t(0.975, df=2) = 4.302..., so ci95 must be ~4.3x sem, not 1.96x
tri = D.aggregate(D.to_long(D.load_table(b"Time,A,A,A\n0,0.10,0.20,0.30\n", "t.csv")), error_type="ci95")
row = tri.iloc[0]
ratio = row["ci95"] / row["sem"] if row["sem"] else float("nan")
check("ci95/sem ~ 4.30 for n=3", abs(ratio - 4.3027) < 0.01, f"got {ratio}")

print("\n== delimiters and encodings ==")
check("TSV parses", D.detect_layout(D.load_table(b"Time\tA\n0\t0.1\n", "a.tsv"))["samples"] == ["A"])
check("semicolon parses", D.detect_layout(D.load_table(b"Time;A\n0;0.1\n", "a.csv"))["samples"] == ["A"])
check("BOM/utf-8-sig parses", D.detect_layout(D.load_table("﻿Time,A\n0,0.1\n".encode("utf-8"), "a.csv"))["time"] == "Time")
check("latin-1 fallback", len(D.load_table(b"Time,Caf\xe9\n0,0.1\n", "a.csv").columns) == 2)

print("\n== blank subtraction ==")
b = D.load_table(b"Time,Blank,A\n0,0.01,0.11\n10,0.02,0.22\n", "b.csv")
ab = D.aggregate(D.to_long(b), blank_sample="Blank")
check("blank row removed from output", "Blank" not in set(ab["sample"]))
check("A t=0 == 0.11-0.01 == 0.10", abs(float(ab[ab["time"] == 0]["mean"].iloc[0]) - 0.10) < 1e-12,
      f"got {float(ab[ab['time']==0]['mean'].iloc[0])}")
check("A t=10 == 0.22-0.02 == 0.20", abs(float(ab[ab["time"] == 10]["mean"].iloc[0]) - 0.20) < 1e-12)

print("\n== normalisation ==")
nz = D.load_table(b"Time,A\n0,0.2\n10,0.4\n20,0.8\n", "n.csv")
n0 = D.aggregate(D.to_long(nz), normalize="t0")
check("t0 normalisation starts at 1.0", abs(float(n0["mean"].iloc[0]) - 1.0) < 1e-12)
check("t0 normalisation doubles to 2.0", abs(float(n0["mean"].iloc[1]) - 2.0) < 1e-12)
nm = D.aggregate(D.to_long(nz), normalize="max")
check("max normalisation peaks at 1.0", abs(float(nm["mean"].max()) - 1.0) < 1e-12)

print("\n== robustness ==")
for name, payload in [
    ("empty file", b""),
    ("header only", b"Time,A\n"),
    ("non-numeric values", b"Time,A\n0,abc\n10,def\n"),
    ("missing cells", b"Time,A,B\n0,0.1,\n10,,0.2\n"),
    ("one column", b"Time\n0\n10\n"),
    ("no time-like name", b"X,Y\n0,0.1\n1,0.2\n"),
    ("ragged", b"Time,A\n0,0.1\n10,0.2,0.3\n"),
    ("duplicate times", b"Time,A\n0,0.1\n0,0.2\n"),
    ("negative values", b"Time,A\n0,-0.1\n10,-0.2\n"),
    ("text time column", b"Time,A\nzero,0.1\nten,0.2\n"),
]:
    try:
        d = D.load_table(payload, "r.csv")
        lay = D.detect_layout(d)
        lgx = D.to_long(d, lay)
        D.aggregate(lgx)
        check(f"survives {name}", True)
    except Exception as e:
        check(f"survives {name}", False, f"raised {type(e).__name__}: {e}")

print("\n== aggregate on empty is well-formed ==")
e = D.aggregate(pd.DataFrame(columns=["time", "sample", "value", "rep"]))
check("empty aggregate has the right columns",
      set(["sample", "time", "mean", "err"]).issubset(set(e.columns)), f"got {list(e.columns)}")
check("empty aggregate has 0 rows", len(e) == 0)

print("\n" + "=" * 60)
if FAIL:
    print(f"{len(FAIL)} FAILURE(S):")
    for f in FAIL:
        print("  -", f)
    sys.exit(1)
print("ALL DATA TESTS PASSED")
