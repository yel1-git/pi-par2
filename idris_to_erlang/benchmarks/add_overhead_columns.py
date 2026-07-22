#!/usr/bin/env python3
"""Adds overhead_ms and efficiency columns to benchmark CSVs.

overhead_ms = workers * par_time_ms - seq_time_ms   (p*Tp - Ts, total overhead)
efficiency  = speedup / workers

For each (src, dst) pair below, reads every *.csv in benchmarks/<src>/
and writes augmented copies of the same filenames into benchmarks/<dst>/.
"""

import csv
import pathlib

BASE_DIR = pathlib.Path(__file__).parent

DIR_PAIRS = [
    ("csv", "csv_overhead_added"),
    ("csv2", "csv2_overhead_added"),
]


def add_overhead_columns(src_dir: pathlib.Path, dst_dir: pathlib.Path) -> None:
    dst_dir.mkdir(exist_ok=True)

    for src_path in sorted(src_dir.glob("*.csv")):
        with src_path.open(newline="") as f:
            rows = list(csv.DictReader(f))

        fieldnames = list(rows[0].keys()) + ["overhead_ms", "efficiency"]

        for row in rows:
            workers = float(row["workers"])
            seq_time = float(row["seq_time_ms"])
            par_time = float(row["par_time_ms"])
            speedup = float(row["speedup"])

            overhead_ms = workers * par_time - seq_time
            efficiency = speedup / workers

            row["overhead_ms"] = f"{overhead_ms:.2f}"
            row["efficiency"] = f"{efficiency:.4f}"

        dst_path = dst_dir / src_path.name
        with dst_path.open("w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)

        print(f"wrote {dst_path}")


for src_name, dst_name in DIR_PAIRS:
    add_overhead_columns(BASE_DIR / src_name, BASE_DIR / dst_name)
