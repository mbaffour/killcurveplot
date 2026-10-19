# Ported from the Lysis Curve App (engine + ingest)

Reference: `Lysis Curve Ap 26.03.13.R` in `mbaffour/lysis-curve-app`
(findings in its `AUDIT-2026-09.md`). Output column names and the
`lysis_threshold` / `peak_mode` arguments are unchanged. Tests:
`tests/test_lc_port.R` (new), plus updated expectations in `test_v28.R`,
`test_v211_analysis.R` and `test_metrics_parity.R`.

## 1. Lysis-metric accuracy (LC-3, LC-9, LC-17)

Ported from the lysis block of `calculate_growth_metrics()`.

* `curve_noise_sd()` (new, top level) and `LYSIS_SUSTAIN_PTS <- 2L`: lysis is
  detected when the post-peak decline exceeds
  `max(lysis_threshold * max_od, 3 * curve_noise_sd(od))` for 2 consecutive
  points (LC-3).
* `lysis_time` is the linearly interpolated crossing of `lysis_threshold * max_od`
  below the peak (LC-9); the noise term is only in the *detection*, not in the
  interpolated level.
* `stat_phase_dur` is the longest contiguous run of points within 95 % of the
  peak (LC-17); new helper `longest_true_run()`.

Deliberate differences from the reference:

* Peak anchor: the reference anchors the lysis metrics on its first prominent
  peak (LC-2, `first_prominent_peak_idx()`); this app keeps `peak_mode`
  (`global` / `first_local`), so `max_od`/`time_max_od` and the lysis anchor are
  unchanged. LC-2 is not ported.
* `stat_phase_dur`: the longest run (as specified for this port), where the
  reference takes the run containing the peak. They coincide whenever the
  peak's own run is the longest.

Numbers that moved on purpose: the 4-point toy curve (0.1, 0.4, 0.2, 0.05 at
t = 0, 10, 20, 30) now has `lysis_time` 11 (was 20). On the demo data the
Uninfected control no longer gets a `lysis_time` on any replicate and the phage
curves' onsets are ~61.2 and ~106.3 (the true onsets are 60 and 105).
