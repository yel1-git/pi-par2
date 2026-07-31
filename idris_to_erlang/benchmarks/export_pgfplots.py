#!/usr/bin/env python3
"""Export the benchmark CSVs as simple 2-column data files for pgfplots.

For every *.csv in csv_overhead_added/ and csv2_overhead_added/ we emit two
whitespace-separated files -- one per plotted curve -- into
<paper>/results/<benchmark>/:

    <stem>_speedup.dat       columns: workers speedup
    <stem>_efficiency.dat    columns: workers efficiency

where <stem> is the CSV name (e.g. matmul_4000, matmul_4000_chunk_50,
cpi_100000000) and <benchmark> is its prefix (cpi, queens, fib, matmul,
sumeuler). Each file has a one-line header so pgfplots' \\addplot table can
name the columns. Pure stdlib.
"""

import csv
import pathlib

BENCH_DIR = pathlib.Path(__file__).parent
CSV_DIRS = [BENCH_DIR / "csv_overhead_added", BENCH_DIR / "csv2_overhead_added"]
OUT_ROOT = pathlib.Path(
    "/Users/cmb21/Papers/hlpp2026/paper/69ca3c4aadee49da6fc4aea4/results"
)

METRICS = ["speedup", "efficiency"]
PREFIXES = ["cpi", "queens", "fib", "matmul", "sumeuler"]


def benchmark_of(stem):
    for p in PREFIXES:
        if stem.startswith(p):
            return p
    raise ValueError(f"unknown benchmark for {stem!r}")


def write_curve(out_dir, stem, metric, rows):
    out = out_dir / f"{stem}_{metric}.dat"
    with out.open("w") as f:
        f.write(f"workers {metric}\n")
        for r in rows:
            f.write(f"{int(float(r['workers']))} {r[metric]}\n")
    return out


def main():
    count = 0
    for csv_dir in CSV_DIRS:
        if not csv_dir.is_dir():
            continue
        for path in sorted(csv_dir.glob("*.csv")):
            stem = path.stem
            bench = benchmark_of(stem)
            out_dir = OUT_ROOT / bench
            out_dir.mkdir(parents=True, exist_ok=True)
            with path.open(newline="") as f:
                rows = list(csv.DictReader(f))
            # sort by worker count so lines draw left-to-right
            rows.sort(key=lambda r: int(float(r["workers"])))
            for metric in METRICS:
                write_curve(out_dir, stem, metric, rows)
                count += 1
    print(f"wrote {count} data files under {OUT_ROOT}")


if __name__ == "__main__":
    main()
