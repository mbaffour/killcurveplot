<!-- Figure-list lines for README.md (the "Analysis figures" bullet list), to merge after the Phenotype heatmap entry. -->

- **Metric summary styles and brackets** (v2.21) — the metric summary also as *Bar*
  (the mean with whiskers, replicates as open circles, y axis from 0), *Box plot* or
  *Violin*, each with the replicate points. Tick *Significance brackets* to draw a
  bracket and stars for every pair of samples with Holm-adjusted p < 0.05 (all pairs,
  at most 6 per panel, the most significant first); the stars against the reference
  are then hidden, and the low-n warning is shown in a note under the controls.
- **OD at chosen times** (v2.21) — grouped bars (or dots) of the mean OD at the times
  typed in the Statistics tab's *OD at chosen times* box, the samples side by side,
  whiskers as in Error Bars, replicate points optional; empty box: the first, middle
  and last time points. Log y when the main figure's y axis is log.
- **Time to an OD threshold** (v2.21) — per sample, when its replicates cross the
  threshold set in the Statistics tab (up or down, from the start or the peak): replicate
  circles, a bar at the mean, whiskers as in Error Bars. Samples that never cross are
  listed in the note.
- **Replicate small multiples** (v2.21) — one panel per sample with its replicate curves
  thin (three shades of the sample colour, readable in greyscale) and the mean bold, on a
  shared y axis.
- **Fraction of replicates lysed** (v2.21) — a step curve per sample from 0 to 1: the
  fraction of replicates that are past the sample's peak and below the threshold (half its
  peak OD by default, or an absolute OD), with the time half of them are lysed marked by a
  point.
