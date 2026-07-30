"""Export-layer checks: physical-size metadata on raster output.

Deliberately avoids kaleido. Driving a headless browser takes ~40 s per render
and is the flakiest thing in the app, so the byte-level tagging that actually
decides a figure's printed size is tested against PNGs made by PIL instead.
"""
import io
import os
import sys

from PIL import Image

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from app.plotting import _png_with_dpi  # noqa: E402

FAIL = []


def check(name, cond, detail=""):
    if cond:
        print(f"  PASS  {name}")
    else:
        print(f"  FAIL  {name} {detail}")
        FAIL.append(f"{name} {detail}")


def make_png(dpi=None, size=(40, 30)):
    buf = io.BytesIO()
    kw = {"dpi": (dpi, dpi)} if dpi else {}
    Image.new("RGB", size, (200, 30, 30)).save(buf, format="PNG", **kw)
    return buf.getvalue()


def read(png):
    im = Image.open(io.BytesIO(png))
    return im.info.get("dpi"), list(im.convert("RGB").getdata())


print("\n== tagging an untagged PNG ==")
src = make_png()
check("kaleido-style PNG starts untagged", Image.open(io.BytesIO(src)).info.get("dpi") is None)
out = _png_with_dpi(src, 300)
dpi, px = read(out)
check("declares 300 dpi", dpi is not None and abs(dpi[0] - 300) < 1, f"got {dpi}")
check("pixels are untouched", px == read(src)[1])
check("exactly one pHYs chunk", out.count(b"pHYs") == 1, f"got {out.count(b'pHYs')}")

print("\n== replacing a stale tag ==")
# PIL writes pHYs *after* IHDR, so a naive insert-on-IHDR leaves two chunks and
# a reader honouring the later one still sees the old density.
src72 = make_png(dpi=72)
out600 = _png_with_dpi(src72, 600)
dpi, px = read(out600)
check("stale 72 dpi replaced by 600", dpi is not None and abs(dpi[0] - 600) < 1, f"got {dpi}")
check("still exactly one pHYs chunk", out600.count(b"pHYs") == 1, f"got {out600.count(b'pHYs')}")
check("pixels still untouched", px == read(src72)[1])

print("\n== physical size is what was asked for ==")
# 6 x 4 in at 300 dpi is 1800 x 1200 px; without the tag a consumer reads 96 dpi
# and places it at 18.75 x 12.5 in.
big = _png_with_dpi(make_png(size=(1800, 1200)), 300)
im = Image.open(io.BytesIO(big))
d = im.info.get("dpi")
check("1800x1200 @300dpi reads as 6.00 x 4.00 in",
      d and abs(im.size[0] / d[0] - 6.0) < 0.02 and abs(im.size[1] / d[1] - 4.0) < 0.02,
      f"got {im.size} @ {d}")

print("\n== malformed input is passed through, not corrupted ==")
junk = b"not a png at all"
check("non-PNG returned unchanged", _png_with_dpi(junk, 300) == junk)
check("empty bytes returned unchanged", _png_with_dpi(b"", 300) == b"")
truncated = make_png()[:20]
check("truncated PNG does not raise", _png_with_dpi(truncated, 300) == truncated)

print("\n" + "=" * 60)
if FAIL:
    print(f"{len(FAIL)} FAILURE(S):")
    for f in FAIL:
        print("  -", f)
    sys.exit(1)
print("ALL EXPORT TESTS PASSED")
