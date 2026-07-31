#!/usr/bin/env python3
"""Export the sync-profiling CSVs as the 2-column data files that
\\cref{fig:sync-overhead} reads.

This stage was missing from the repo: aggregate_sync.py stopped at
csv_sync/<bench>_sync_<size>.csv, but the paper's results/sync/*.dat files
had no generator, so a re-run of the profiled benchmarks could not be carried
through to the figure.

For each csv_sync/<bench>_sync_<size>.csv writes into <paper>/results/sync/:

    <bench>_mem.dat       columns: workers memGB      (= mem_proc_peak_mb / 1000)
    <bench>_syncfrac.dat  columns: workers syncfrac   (= wait_frac, verbatim)

Note the output names carry no size, because the figure plots one workload per
benchmark: cpi 10^8, sumeuler 4x10^4, matmul 4000^2. If csv_sync/ ever holds two
sizes for the same benchmark, the later one silently wins -- pass --only to pick.

Usage:
    python3 export_sync_pgfplots.py
    python3 export_sync_pgfplots.py --only sumeuler_sync_40000,matmul_sync_4000
"""

import argparse
import csv
import pathlib

BENCH_DIR = pathlib.Path(__file__).parent
IN_DIR = BENCH_DIR / "csv_sync"
OUT_DIR = pathlib.Path(
    "/Users/cmb21/Papers/hlpp2026/paper/69ca3c4aadee49da6fc4aea4/results/sync"
)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--only", default="",
                    help="comma-separated csv_sync stems to export (default: all)")
    args = ap.parse_args()
    wanted = {s.strip() for s in args.only.split(",") if s.strip()}

    if not IN_DIR.is_dir():
        raise SystemExit(f"no such directory: {IN_DIR}")
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    files = sorted(IN_DIR.glob("*.csv"))
    if not files:
        raise SystemExit(f"no CSVs in {IN_DIR} -- run aggregate_sync.py first")

    for path in files:
        if wanted and path.stem not in wanted:
            continue
        bench = path.stem.split("_sync_")[0]
        rows = list(csv.DictReader(path.open(newline="")))
        rows.sort(key=lambda r: int(float(r["workers"])))

        mem = OUT_DIR / f"{bench}_mem.dat"
        with mem.open("w") as f:
            f.write("workers memGB\n")
            for r in rows:
                f.write(f"{int(float(r['workers']))} "
                        f"{float(r['mem_proc_peak_mb']) / 1000:.4f}\n")

        frac = OUT_DIR / f"{bench}_syncfrac.dat"
        with frac.open("w") as f:
            f.write("workers syncfrac\n")
            for r in rows:
                f.write(f"{int(float(r['workers']))} {r['wait_frac']}\n")

        print(f"{path.name:30s} -> sync/{mem.name}, sync/{frac.name}  "
              f"({len(rows)} worker counts)")

    print(f"\nwrote to {OUT_DIR}")
    print("Table 3 (tab:overhead) is still hand-written: take its row for each")
    print("benchmark from csv_sync/ at the peak-speedup worker count --")
    print("  Aggr. compute (s) = compute_s        Sync frac.  = wait_frac")
    print("  Spawn (s)         = spawn_ms / 1000  Msg (MB)    = msg_mb")
    print("  Mem peak (GB)     = mem_proc_peak_mb / 1000")


if __name__ == "__main__":
    main()
