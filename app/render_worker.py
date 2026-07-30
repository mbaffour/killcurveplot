"""
Out-of-process static image renderer.

Run as: ``python -m app.render_worker <spec.json>``

Why a separate process at all: the static-image engine (kaleido) drives a
headless Chrome, and in testing every in-process render left a browser behind —
41 orphaned Chrome processes and ~1.9 GB after a normal testing session. On a
lab laptop that is the difference between a working machine and a stalled one.

Running renders here means the parent can (a) impose a hard timeout so a wedged
browser cannot hang the app, and (b) kill this process *and its children* to
guarantee cleanup. It also keeps a crash in the rendering engine from taking
down the user's session.

The spec is JSON: {"figs": [...], "paths": [...], "format", "width", "height",
"scale"}. Figures are plotly figure dicts.
"""
import json
import sys


def main(spec_path):
    with open(spec_path, encoding="utf-8") as fh:
        spec = json.load(fh)

    figs = spec["figs"]
    paths = spec["paths"]
    fmt = spec.get("format", "png")
    w, h, scale = spec.get("width"), spec.get("height"), spec.get("scale", 1.0)

    import plotly.io as pio

    n = len(figs)
    if n > 1 and hasattr(pio, "write_images"):
        # Batch writer: one browser start-up for the whole set instead of one
        # per frame (~12 s each), which is what made GIF/PPTX unusably slow.
        pio.write_images(figs, paths, format=[fmt] * n, width=[w] * n,
                         height=[h] * n, scale=[scale] * n)
    else:
        for fig, p in zip(figs, paths):
            pio.write_image(fig, p, format=fmt, width=w, height=h, scale=scale)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1]))
    except Exception as exc:                      # noqa: BLE001 - report and exit
        sys.stderr.write(f"{type(exc).__name__}: {exc}\n")
        sys.exit(1)
