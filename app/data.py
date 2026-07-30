"""
Data ingestion for kill/lysis/growth curves.

Mirrors the R app's contract — wide or long CSV, auto-detected, with repeated
column names treated as replicates — and adds the things it couldn't do:
Excel/TSV input, blank subtraction, normalisation, and per-timepoint stats.
"""
from __future__ import annotations

import io
import re

import numpy as np
import pandas as pd

__all__ = ["load_table", "detect_layout", "to_long", "aggregate", "TIME_PATTERNS"]

# Column names that mean "time". Checked case-insensitively against the header.
TIME_PATTERNS = ("time", "t", "min", "minute", "minutes", "hour", "hours", "hr", "h",
                 "sec", "second", "seconds", "timepoint", "time_point", "elapsed")


def _dedupe_preserving(names):
    """pandas mangles duplicate headers to 'A', 'A.1'. Recover the original name.

    Duplicate headers ARE the replicate signal in this format, so the mangling
    has to be undone. But it must be undone ONLY where pandas actually created
    it: a bare regex strip of a trailing '.<digits>' would also rewrite genuine
    column names like 'OD.600' or 'A.550' and silently average two different
    measurements into one sample. pandas only ever appends '.N' when the base
    name already occurred earlier, so require that.
    """
    out, seen = [], set()
    for n in names:
        s = str(n).strip()
        m = re.match(r"^(.*)\.(\d+)$", s)
        if m and m.group(1) in seen:
            s = m.group(1)          # a pandas-generated suffix — drop it
        out.append(s)
        seen.add(s)
    return out


def load_table(content: bytes, filename: str = "") -> pd.DataFrame:
    """Read CSV/TSV/Excel bytes into a DataFrame, preserving duplicate headers."""
    name = (filename or "").lower()
    if name.endswith((".xlsx", ".xls", ".xlsm")):
        df = pd.read_excel(io.BytesIO(content))
        df.columns = _dedupe_preserving(df.columns)
        return df

    text = None
    for enc in ("utf-8-sig", "utf-8", "latin-1"):
        try:
            text = content.decode(enc)
            break
        except UnicodeDecodeError:
            continue
    if text is None:
        text = content.decode("utf-8", errors="replace")

    # Sniff the delimiter from the header line rather than trusting the extension.
    head = text.split("\n", 1)[0]
    delim = "\t" if head.count("\t") > head.count(",") else (
        ";" if head.count(";") > head.count(",") else ",")
    # A real plate-reader export is often ragged or empty-ish. Degrade to an
    # empty frame instead of raising, so the UI can say "no usable columns"
    # rather than showing a stack trace.
    try:
        df = pd.read_csv(io.StringIO(text), sep=delim)
    except pd.errors.EmptyDataError:
        return pd.DataFrame()
    except Exception:
        try:                       # ragged rows: keep the widest row, pad the rest
            df = pd.read_csv(io.StringIO(text), sep=delim, engine="python",
                             on_bad_lines="skip")
        except Exception:
            return pd.DataFrame()
    df.columns = _dedupe_preserving(df.columns)
    return df


def _looks_like_time(col: str) -> bool:
    c = str(col).strip().lower()
    return c in TIME_PATTERNS or any(c.startswith(p) for p in ("time", "elapsed"))


def detect_layout(df: pd.DataFrame) -> dict:
    """Decide wide vs long and name the key columns.

    Long requires a non-numeric grouping column alongside exactly one numeric
    value column; anything else is treated as wide, which is the common case.
    """
    cols = list(df.columns)
    if not cols:
        return {"layout": "wide", "time": None, "group": None, "value": None, "samples": []}

    time_col = next((c for c in cols if _looks_like_time(c)), cols[0])

    # Index POSITIONALLY: duplicate column names are the replicate signal in the
    # wide format, and df[name] returns a DataFrame (not a Series) when the name
    # repeats, which breaks to_numeric.
    numeric, categorical = [], []
    for i, c in enumerate(cols):
        if c == time_col:
            continue
        raw = df.iloc[:, i]
        s = pd.to_numeric(raw, errors="coerce")
        # >60% parseable as numbers => a measurement column, not a label.
        frac = s.notna().mean() if len(s) else 0.0
        # A column with no data at all is evidence of nothing; keep it as a
        # measurement so it stays visible. This must NOT catch all-text columns
        # (which coerce to all-NaN too) or every long-format group column would
        # be misread as a measurement.
        truly_blank = raw.isna().all() or (raw.astype(str).str.strip() == "").all()
        (numeric if (frac > 0.6 or truly_blank) else categorical).append(c)

    if len(categorical) >= 1 and len(numeric) == 1:
        return {"layout": "long", "time": time_col, "group": categorical[0],
                "value": numeric[0],
                "samples": sorted(df[categorical[0]].dropna().astype(str).unique().tolist())}

    return {"layout": "wide", "time": time_col, "group": None, "value": None,
            "samples": list(dict.fromkeys(numeric))}


def to_long(df: pd.DataFrame, layout: dict | None = None) -> pd.DataFrame:
    """Normalise any accepted layout to tidy columns: time, sample, value, rep.

    ``rep`` numbers the replicates within each (time, sample) so downstream code
    can tell "3 wells at t=0" from "3 separate timepoints".
    """
    layout = layout or detect_layout(df)
    tcol = layout["time"]
    if tcol is None or tcol not in df.columns:
        return pd.DataFrame(columns=["time", "sample", "value", "rep"])

    ti = list(df.columns).index(tcol)
    time = pd.to_numeric(df.iloc[:, ti], errors="coerce")

    if layout["layout"] == "long":
        gcol, vcol = layout["group"], layout["value"]
        gi = list(df.columns).index(gcol)
        vi = list(df.columns).index(vcol)
        out = pd.DataFrame({
            "time": time,
            "sample": df.iloc[:, gi].astype(str),
            "value": pd.to_numeric(df.iloc[:, vi], errors="coerce"),
        })
    else:
        frames = []
        # Positional access: duplicate column names are replicates and must all
        # be read, which df[name] cannot do when the name repeats.
        for i, cname in enumerate(df.columns):
            if cname == tcol or not layout["samples"] or cname not in layout["samples"]:
                continue
            frames.append(pd.DataFrame({
                "time": time,
                "sample": str(cname),
                "value": pd.to_numeric(df.iloc[:, i], errors="coerce"),
            }))
        out = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame(
            columns=["time", "sample", "value"])

    out = out.dropna(subset=["time"])
    out["rep"] = out.groupby(["time", "sample"], sort=False).cumcount() + 1
    return out.reset_index(drop=True)


def _blank_subtract(long_df, blank_sample):
    """Subtract the blank's mean value at each timepoint from every sample."""
    if not blank_sample or blank_sample not in set(long_df["sample"]):
        return long_df
    blanks = (long_df[long_df["sample"] == blank_sample]
              .groupby("time")["value"].mean())
    out = long_df.copy()
    out["value"] = out["value"] - out["time"].map(blanks).fillna(0.0)
    return out[out["sample"] != blank_sample].reset_index(drop=True)


def _normalize(long_df, mode):
    """t0 / max / control-relative normalisation, applied per sample."""
    if mode in (None, "none", ""):
        return long_df
    out = long_df.copy()
    if mode == "t0":
        # Divide by the MEAN of the first timepoint, not by whichever replicate
        # happens to sit in the first row. Using one replicate makes the
        # normalised curve start at that replicate's value (e.g. reps
        # 0.1/0.2/0.3 normalised to 0.1 average to 2.0, not 1.0).
        t0 = out["time"].min()
        base = (out[out["time"] == t0].groupby("sample")["value"].mean())
        denom = out["sample"].map(base).replace(0, np.nan)
        out["value"] = out["value"] / denom
    elif mode == "max":
        # Same reasoning: normalise to the peak of the averaged curve.
        peak = (out.groupby(["sample", "time"])["value"].mean()
                   .groupby("sample").max())
        out["value"] = out["value"] / out["sample"].map(peak).replace(0, np.nan)
    elif mode == "zscore":
        m = out.groupby("sample")["value"].transform("mean")
        s = out.groupby("sample")["value"].transform("std").replace(0, np.nan)
        out["value"] = (out["value"] - m) / s
    return out


def aggregate(long_df: pd.DataFrame, error_type: str = "sem",
              blank_sample: str | None = None, normalize: str | None = None,
              smooth_window: int = 1, log_transform: bool = False) -> pd.DataFrame:
    """Collapse replicates to per-(sample, time) mean plus a dispersion column.

    Returns columns: sample, time, mean, sd, sem, ci95, n, err.
    ``err`` is whichever dispersion the caller asked for, so the plot layer
    never has to branch on error_type.
    """
    if long_df is None or long_df.empty:
        return pd.DataFrame(columns=["sample", "time", "mean", "sd", "sem", "ci95", "n", "err"])

    df = long_df.dropna(subset=["value"]).copy()
    if blank_sample:
        df = _blank_subtract(df, blank_sample)
    if normalize:
        df = _normalize(df, normalize)
    if log_transform:
        pos = df["value"][df["value"] > 0]
        floor = (pos.min() * 1e-6) if len(pos) else 1e-12
        df["value"] = np.log10(df["value"].clip(lower=floor))

    g = df.groupby(["sample", "time"], sort=True)["value"]
    out = g.agg(mean="mean", sd="std", n="count").reset_index()
    # A single replicate has no dispersion — that is 0 to plot, not NaN.
    out["sd"] = out["sd"].fillna(0.0)
    out["sem"] = np.where(out["n"] > 0, out["sd"] / np.sqrt(out["n"].clip(lower=1)), 0.0)

    # 95% CI uses the t distribution so n=3 isn't reported as if it were n=300.
    try:
        from scipy import stats as _st
        crit = np.where(out["n"] > 1, _st.t.ppf(0.975, np.maximum(out["n"] - 1, 1)), 0.0)
    except Exception:
        crit = np.where(out["n"] > 1, 1.96, 0.0)
    out["ci95"] = out["sem"] * crit

    if smooth_window and int(smooth_window) > 1:
        from .analysis import smooth as _smooth
        out = out.sort_values(["sample", "time"])
        out["mean"] = (out.groupby("sample")["mean"]
                       .transform(lambda s: pd.Series(_smooth(s.to_numpy(), int(smooth_window)),
                                                      index=s.index)))

    et = (error_type or "sem").lower()
    out["err"] = out[et] if et in ("sd", "sem", "ci95") else 0.0
    return out.sort_values(["sample", "time"]).reset_index(drop=True)
