#!/usr/bin/env python3
"""Aggregate the sync-instrumented profiling runs in output_sync/ into per-worker
CSVs (one row per worker count, means over the repetitions).

Each output_sync/<bench>_sync_<N>.txt file contains, for every worker count,
several repetitions of a `... sync stats: [ {key,val}, ... ]` proplist emitted
by play_sync.erl. We parse those proplists, average each metric over the
repetitions at a given worker count, and derive a few convenience columns:

  wait_frac       wait_time_us / wall_time_us          (sync barrier fraction)
  compute_s       compute_time_us / 1e6                (aggregate worker compute)
  wall_s          wall_time_us / 1e6
  wait_s          wait_time_us / 1e6
  spawn_ms        spawn_time_us / 1e3
  msg_mb          message_bytes / 1e6
  mem_proc_peak_mb  mem_processes_peak_bytes / 1e6
  mem_proc_mean_mb  mem_processes_mean_bytes / 1e6
  mem_total_peak_mb mem_total_peak_bytes / 1e6

Writes csv_sync/<bench>_sync_<N>.csv. Pure stdlib, no dependencies.
"""

import csv
import pathlib
import re
import statistics as st

BENCH_DIR = pathlib.Path(__file__).parent
IN_DIR = BENCH_DIR / "output_sync"
OUT_DIR = BENCH_DIR / "csv_sync"

# Raw metric keys we pull straight out of each proplist.
RAW_KEYS = [
    "wall_time_us", "compute_time_us", "wait_time_us", "spawn_time_us",
    "spawn_count", "message_count", "message_bytes",
    "mem_total_peak_bytes", "mem_total_mean_bytes",
    "mem_processes_peak_bytes", "mem_processes_mean_bytes",
    "mem_sample_count",
]

# Output column order.
FIELDS = [
    "workers", "reps",
    "wall_s", "compute_s", "wait_s", "wait_frac",
    "spawn_ms", "spawn_count", "message_count", "msg_mb",
    "mem_proc_peak_mb", "mem_proc_mean_mb", "mem_total_peak_mb",
]

WORKER_SPLIT = re.compile(r"=== (\d+) workers ===")
STATS_BLOCK = re.compile(r"sync stats: \[(.*?)\]", re.S)


def parse_proplist(chunk):
    """Return {key: float} for the RAW_KEYS present in one `{k,v}, ...` blob."""
    out = {}
    for key in RAW_KEYS:
        m = re.search(rf"\{{{key},([0-9.]+)\}}", chunk)
        if m:
            out[key] = float(m.group(1))
    return out


def aggregate_file(path):
    """Yield one dict per worker count, metrics averaged over repetitions."""
    text = path.read_text()
    parts = WORKER_SPLIT.split(text)
    # parts = [header, w1, body1, w2, body2, ...]
    it = iter(parts[1:])
    for w, body in zip(it, it):
        reps = [parse_proplist(m.group(1)) for m in STATS_BLOCK.finditer(body)]
        reps = [r for r in reps if "wall_time_us" in r]
        if not reps:
            continue
        mean = {k: st.mean([r[k] for r in reps if k in r])
                for k in RAW_KEYS
                if any(k in r for r in reps)}
        wall = mean["wall_time_us"]
        row = {
            "workers": int(w),
            "reps": len(reps),
            "wall_s": round(wall / 1e6, 4),
            "compute_s": round(mean["compute_time_us"] / 1e6, 4),
            "wait_s": round(mean["wait_time_us"] / 1e6, 4),
            "wait_frac": round(mean["wait_time_us"] / wall, 4) if wall else "",
            "spawn_ms": round(mean["spawn_time_us"] / 1e3, 4),
            "spawn_count": round(mean["spawn_count"], 1),
            "message_count": round(mean["message_count"], 1),
            "msg_mb": round(mean["message_bytes"] / 1e6, 3),
            "mem_proc_peak_mb": round(mean["mem_processes_peak_bytes"] / 1e6, 1),
            "mem_proc_mean_mb": round(mean["mem_processes_mean_bytes"] / 1e6, 1),
            "mem_total_peak_mb": round(mean["mem_total_peak_bytes"] / 1e6, 1),
        }
        yield row


def main():
    OUT_DIR.mkdir(exist_ok=True)
    files = sorted(IN_DIR.glob("*.txt"))
    if not files:
        raise SystemExit(f"no input files in {IN_DIR}")
    for path in files:
        rows = list(aggregate_file(path))
        out_path = OUT_DIR / (path.stem + ".csv")
        with out_path.open("w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=FIELDS)
            writer.writeheader()
            writer.writerows(rows)
        print(f"{path.name:32s} -> {out_path.relative_to(BENCH_DIR)}  ({len(rows)} worker counts)")


if __name__ == "__main__":
    main()
