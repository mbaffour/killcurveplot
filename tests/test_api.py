"""End-to-end API tests via Flask's test client (no network, no real browser).

Covers the full request surface plus the hostile inputs a real plate-reader
export throws at it. Static-image formats are opt-in via KC_TEST_IMAGES=1
because each one starts a headless browser and takes ~10 s.
"""
import io
import json
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
os.environ.setdefault("OPENBLAS_NUM_THREADS", "2")

from app import server as S  # noqa: E402

FAIL = []
DO_IMAGES = os.environ.get("KC_TEST_IMAGES") == "1"


def check(name, cond, detail=""):
    if cond:
        print(f"  PASS  {name}")
    else:
        print(f"  FAIL  {name} {detail}")
        FAIL.append(f"{name} {detail}")


def upload(c, content, filename="d.csv"):
    return c.post("/api/upload", data={"file": (io.BytesIO(content), filename)},
                  content_type="multipart/form-data")


S.app.config["TESTING"] = True
c = S.app.test_client()

print("\n== health / static ==")
r = c.get("/api/health")
check("health 200", r.status_code == 200)
check("health reports capabilities", "capabilities" in r.get_json())
check("index served", c.get("/").status_code == 200)
check("app.js served", c.get("/static/app.js").status_code == 200)
check("style.css served", c.get("/static/style.css").status_code == 200)
check("plotly vendored locally", c.get("/static/plotly.min.js").status_code == 200)
check("template csv served", c.get("/api/sample-csv").status_code == 200)

print("\n== upload validation ==")
check("no file -> 400", c.post("/api/upload").status_code == 400)
check("empty file -> 400", upload(c, b"").status_code == 400)
r = upload(c, b"not,a,table\n")
check("headers with no data -> 400", r.status_code == 400, f"got {r.status_code}")
r = upload(c, b"Time,A\nfoo,bar\nbaz,qux\n")
check("no numeric values -> 400", r.status_code == 400, f"got {r.status_code}")
check("error message is human-readable",
      "error" in (r.get_json() or {}) and len(r.get_json()["error"]) > 20)

print("\n== real upload ==")
csv = (b"Time,WT,WT,WT,Mutant,Mutant,Mutant\n"
       b"0,0.05,0.06,0.04,0.05,0.04,0.06\n"
       b"30,0.12,0.15,0.10,0.08,0.09,0.07\n"
       b"60,0.35,0.40,0.28,0.14,0.15,0.13\n"
       b"90,0.70,0.82,0.61,0.20,0.22,0.19\n"
       b"120,1.05,1.10,0.98,0.10,0.11,0.09\n")
r = upload(c, csv)
j = r.get_json()
check("upload 200", r.status_code == 200, f"got {r.status_code}: {j}")
check("2 samples detected", j["samples"] == ["WT", "Mutant"], f"got {j.get('samples')}")
check("3 replicates detected", j["max_replicates"] == 3, f"got {j.get('max_replicates')}")
check("5 timepoints", j["n_points"] == 5, f"got {j.get('n_points')}")
check("wide layout", j["layout"] == "wide")

print("\n== figure ==")
r = c.get("/api/figure")
fig = r.get_json()
check("figure 200", r.status_code == 200)
check("figure has 2 traces", len(fig.get("data", [])) == 2, f"got {len(fig.get('data', []))}")
check("figure has a layout", "layout" in fig)
check("trace names match samples",
      sorted(t.get("name") for t in fig["data"]) == ["Mutant", "WT"])
check("error bars present (3 reps)",
      any(t.get("error_y", {}).get("array") for t in fig["data"]))

print("\n== config ==")
r = c.post("/api/config", json={"cfg": {"y_scale_type": "linear", "plot_title": "T"}})
check("config POST 200", r.status_code == 200)
check("config persisted", r.get_json()["cfg"]["y_scale_type"] == "linear")
c.post("/api/config", json={"cfg": {"__evil__": 1}})
check("unknown keys rejected", "__evil__" not in c.get("/api/config").get_json()["cfg"])
r = c.post("/api/config", json={"styles": {"WT": {"color": "#ff0000", "visible": False}}})
check("style persisted", r.get_json()["styles"]["WT"]["color"] == "#ff0000")
fig = c.get("/api/figure").get_json()
check("hidden sample is not drawn", len(fig["data"]) == 1, f"got {len(fig['data'])}")
c.post("/api/config", json={"styles": {"WT": {"visible": True}}})

print("\n== log axis range uses exponents ==")
c.post("/api/config", json={"cfg": {"y_scale_type": "log", "y_min": 0.01, "y_max": 10}})
fig = c.get("/api/figure").get_json()
yr = fig["layout"]["yaxis"].get("range")
check("log range is [-2, 1] not [0.01, 10]",
      yr and abs(yr[0] + 2) < 1e-9 and abs(yr[1] - 1) < 1e-9, f"got {yr}")
c.post("/api/config", json={"cfg": {"y_min": None, "y_max": None}})

print("\n== analysis ==")
r = c.get("/api/analysis")
j = r.get_json()
check("analysis 200", r.status_code == 200)
check("one row per sample", len(j["rows"]) == 2, f"got {len(j['rows'])}")
row = {x["sample"]: x for x in j["rows"]}["WT"]
check("mu_max computed", row["mu_max"] is not None and row["mu_max"] > 0)
check("doubling time computed", row["doubling_time"] is not None)
check("no NaN leaks into JSON (would be invalid)",
      "NaN" not in json.dumps(j) and "Infinity" not in json.dumps(j))
c.post("/api/config", json={"cfg": {"control_sample": "WT"}})
j = c.get("/api/analysis").get_json()
mut = {x["sample"]: x for x in j["rows"]}["Mutant"]
check("log reduction appears with a control", "max_log_reduction" in mut)
check("Mutant shows a positive log reduction vs WT",
      (mut.get("max_log_reduction") or 0) > 0, f"got {mut.get('max_log_reduction')}")
check("analysis CSV downloads", c.get("/api/analysis.csv").status_code == 200)
c.post("/api/config", json={"cfg": {"control_sample": None}})

print("\n== fits ==")
r = c.post("/api/fit", json={"model": "logistic"})
check("fit 200", r.status_code == 200)
check("a fit is returned per sample", len(r.get_json()["fits"]) == 2)
for m in ("gompertz", "baranyi", "nonsense"):
    check(f"fit model '{m}' does not 500",
          c.post("/api/fit", json={"model": m}).status_code == 200)

print("\n== filtering ==")
c.post("/api/config", json={"cfg": {"time_min": 30, "time_max": 90}})
t = c.get("/api/table").get_json()
times = sorted(set(r["time"] for r in t["rows"]))
check("time filter applied", times == [30, 60, 90], f"got {times}")
c.post("/api/config", json={"cfg": {"time_min": None, "time_max": None,
                                    "exclude_timepoints": "0, 120"}})
t = c.get("/api/table").get_json()
times = sorted(set(r["time"] for r in t["rows"]))
check("exclusions applied", times == [30, 60, 90], f"got {times}")
c.post("/api/config", json={"cfg": {"exclude_timepoints": "30-90"}})
times = sorted(set(r["time"] for r in c.get("/api/table").get_json()["rows"]))
check("range exclusion applied", times == [0, 120], f"got {times}")
c.post("/api/config", json={"cfg": {"exclude_timepoints": "garbage, 1-, -"}})
check("garbage exclusion spec does not 500", c.get("/api/table").status_code == 200)
c.post("/api/config", json={"cfg": {"exclude_timepoints": ""}})

print("\n== settings round trip ==")
c.post("/api/config", json={"cfg": {"plot_title": "Round trip"},
                            "styles": {"WT": {"color": "#123456"}}})
blob = c.get("/api/settings/export").data
saved = json.loads(blob)
check("export contains cfg + styles", "cfg" in saved and "styles" in saved)
c.post("/api/config/reset")
check("reset clears the title", c.get("/api/config").get_json()["cfg"]["plot_title"] == "")
r = c.post("/api/settings/import",
           data={"file": (io.BytesIO(blob), "s.json")},
           content_type="multipart/form-data")
check("import 200", r.status_code == 200)
cfg = c.get("/api/config").get_json()
check("title restored", cfg["cfg"]["plot_title"] == "Round trip")
check("style restored", cfg["styles"]["WT"]["color"] == "#123456")
check("matched samples reported", "WT" in r.get_json()["matched"])
r = c.post("/api/settings/import", data={"file": (io.BytesIO(b"not json"), "s.json")},
           content_type="multipart/form-data")
check("bad settings file -> 400 not 500", r.status_code == 400, f"got {r.status_code}")

print("\n== long format ==")
long_csv = (b"Time,Sample,OD\n0,A,0.1\n0,A,0.12\n0,B,0.2\n"
            b"10,A,0.3\n10,A,0.32\n10,B,0.5\n")
j = upload(c, long_csv, "long.csv").get_json()
check("long detected", j["layout"] == "long", f"got {j.get('layout')}")
check("long samples", sorted(j["samples"]) == ["A", "B"], f"got {j['samples']}")
check("figure works for long", len(c.get("/api/figure").get_json()["data"]) == 2)

print("\n== demo + session ==")
r = c.post("/api/demo")
check("demo 200", r.status_code == 200)
check("demo has 4 samples", len(r.get_json()["samples"]) == 4)
check("demo analysis works", c.get("/api/analysis").status_code == 200)
check("session save", c.post("/api/session/save").status_code == 200)
check("session restore", c.post("/api/session/restore").status_code == 200)

print("\n== exports (data formats always; images opt-in) ==")
for fmt, magic in [("csv", b"sample"), ("json", b"{"), ("html", b"<")]:
    r = c.get(f"/api/export/{fmt}")
    check(f"export {fmt}", r.status_code == 200 and r.data[:len(magic)] == magic,
          f"got {r.status_code}")
check("unknown format does not crash the process",
      c.get("/api/export/bogus").status_code in (200, 500))

if DO_IMAGES:
    for fmt, magic in [("png", b"\x89PNG"), ("pdf", b"%PDF"), ("svg", b"<svg"),
                       ("tiff", None), ("pptx", b"PK"), ("gif", b"GIF8")]:
        r = c.get(f"/api/export/{fmt}?width=5&height=4&dpi=72")
        ok = r.status_code == 200 and len(r.data) > 500 and (magic is None or r.data[:4] == magic)
        check(f"export {fmt}", ok, f"status={r.status_code} bytes={len(r.data)}")
else:
    print("  SKIP  image exports (set KC_TEST_IMAGES=1 to include)")

print("\n== hostile inputs ==")
hostile = [
    ("only a time column", b"Time\n0\n10\n"),
    ("one row", b"Time,A\n0,0.5\n"),
    ("negative + zero", b"Time,A\n0,0\n10,-1\n20,0.5\n"),
    ("huge values", b"Time,A\n0,1e300\n10,1e300\n"),
    ("tiny values", b"Time,A\n0,1e-300\n10,1e-300\n"),
    ("unicode sample names", "Time,Café ☕,Café ☕\n0,0.1,0.2\n10,0.3,0.4\n".encode()),
    ("formula-looking header", b"Time,=cmd|calc,B\n0,0.1,0.2\n10,0.3,0.4\n"),
    ("very long name", b"Time," + b"X" * 500 + b"\n0,0.1\n10,0.2\n"),
    ("duplicate times", b"Time,A\n0,0.1\n0,0.2\n0,0.3\n"),
    ("unsorted times", b"Time,A\n90,0.9\n0,0.1\n30,0.3\n"),
    ("semicolon delim", b"Time;A;B\n0;0.1;0.2\n10;0.3;0.4\n"),
    ("tabs", b"Time\tA\tB\n0\t0.1\t0.2\n10\t0.3\t0.4\n"),
    ("missing cells", b"Time,A,B\n0,0.1,\n10,,0.2\n20,0.3,0.4\n"),
    ("BOM", "﻿Time,A\n0,0.1\n10,0.2\n".encode("utf-8")),
    ("crlf", b"Time,A\r\n0,0.1\r\n10,0.2\r\n"),
]
for name, payload in hostile:
    r = upload(c, payload, "h.csv")
    if r.status_code not in (200, 400):
        check(f"upload: {name}", False, f"status {r.status_code}")
        continue
    if r.status_code == 400:
        check(f"upload: {name} (rejected cleanly)", True)
        continue
    ok = True
    for ep in ["/api/figure", "/api/analysis", "/api/table", "/api/export/csv"]:
        if c.get(ep).status_code != 200:
            ok = False
            check(f"{name}: {ep}", False, "non-200")
    if c.post("/api/fit", json={"model": "logistic"}).status_code != 200:
        ok = False
        check(f"{name}: /api/fit", False, "non-200")
    check(f"upload+render: {name}", ok)

print("\n== large dataset ==")
rows = ["Time," + ",".join(f"S{i}" for i in range(30))]
for t in range(300):
    rows.append(",".join([str(t)] + [f"{0.05 * (1 + i) + t * 0.001:.4f}" for i in range(30)]))
big = ("\n".join(rows) + "\n").encode()
r = upload(c, big, "big.csv")
check("30 samples x 300 timepoints uploads", r.status_code == 200, f"got {r.status_code}")
if r.status_code == 200:
    import time as _t
    t0 = _t.time()
    check("figure builds", c.get("/api/figure").status_code == 200)
    t1 = _t.time()
    check(f"figure under 5 s (took {t1 - t0:.1f}s)", (t1 - t0) < 5.0)
    t0 = _t.time()
    check("analysis runs", c.get("/api/analysis").status_code == 200)
    check(f"analysis under 10 s (took {_t.time() - t0:.1f}s)", (_t.time() - t0) < 10.0)

print("\n== oversize upload guarded ==")
check("MAX_CONTENT_LENGTH set", S.app.config["MAX_CONTENT_LENGTH"] > 0)

print("\n" + "=" * 60)
if FAIL:
    print(f"{len(FAIL)} FAILURE(S):")
    for f in FAIL:
        print("  -", f)
    sys.exit(1)
print("ALL API TESTS PASSED")
