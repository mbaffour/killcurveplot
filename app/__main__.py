"""Entry point: ``python -m app`` starts the local server."""
import os

# Cap the BLAS/OpenMP thread pools BEFORE numpy/scipy are imported — setting
# these afterwards has no effect. This workload is tiny (a few thousand points),
# so extra threads buy nothing, but each pool reserves per-thread buffers and on
# a many-core laptop that can exhaust OpenBLAS's allocator, especially alongside
# the headless browser used for image export. Seen in testing as
# "OpenBLAS error: Memory allocation still failed after 10 retries".
for _v in ("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS",
           "NUMEXPR_NUM_THREADS", "VECLIB_MAXIMUM_THREADS"):
    os.environ.setdefault(_v, "2")

import argparse  # noqa: E402
import socket  # noqa: E402


def _free_port(preferred, host="127.0.0.1"):
    """The preferred port, or the next free one if it is taken.

    A stale instance holding 8765 shouldn't stop the app from starting.
    """
    for port in [preferred] + [preferred + i for i in range(1, 25)]:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                s.bind((host, port))
                return port
            except OSError:
                continue
    return preferred


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Kill Curve Studio — local plotting app")
    ap.add_argument("--port", type=int, default=8765)
    ap.add_argument("--host", default="127.0.0.1")
    ap.add_argument("--no-browser", action="store_true", help="don't open a browser")
    a = ap.parse_args()

    from .server import run

    port = _free_port(a.port, a.host)
    if port != a.port:
        print(f"  Port {a.port} was busy — using {port} instead.")
    run(host=a.host, port=port, open_browser=not a.no_browser)
