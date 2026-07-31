#!/usr/bin/env python3
"""Turn the Elysium task-farm results (farm_*.txt + seq_baselines.txt, produced
by comparison/run_comparison.sh) into 2-column .dat files for pgfplots, so the
Elysium curves can be overlaid on the Parallel-Types figures.

For each farm_<bench>_<size>[_<date>].txt it emits, into the paper's
results/<bench>/ directory:

    <bench>_<size>_elysium_speedup.dat     columns: workers speedup
    <bench>_<size>_elysium_efficiency.dat  columns: workers efficiency

speedup   = Ts / mean_parallel_time   (mean over the reps at each worker count)
efficiency= speedup / workers

Ts (sequential baseline, seconds) is taken from seq_baselines.txt when present,
else from the built-in PT Table 1 values (the sequential code is identical
across the two codebases, so these are the reconciled baselines).

Usage:  python3 parse_farm.py <results_dir>   [default: ./farm_results]
Times in the farm files are microseconds (sk_profile / timer:tc).
"""

import pathlib
import re
import statistics as st
import sys

PAPER_RESULTS = pathlib.Path(
    "/Users/cmb21/Papers/hlpp2026/paper/69ca3c4aadee49da6fc4aea4/results"
)

# Sequential baselines Ts (seconds), taken from the Parallel-Types runs' own
# seq_time_ms column (benchmarks/csv2/<bench>_<size>.csv, the vintage the paper
# plots) so that the Elysium overlay and the PT curve in the same panel are
# normalised against the SAME Ts. Keyed (bench,size).
#
# NB these are read off the CSVs, NOT off Table 1 of the paper: four of Table 1's
# "Seq. (s)" entries disagree with the CSVs the rest of that row was computed
# from -- fib 25 1000 (10.98 vs 1.10), fib 20 10000 (9.84 vs 0.98), pi 1e9
# (210.26 vs 384.45) and matmul 1000 (48.69 vs 76.04). An earlier version of
# this table used Table 1's numbers, which understated the Elysium matmul 1000
# curve by 1.56x and the Elysium cpi 1e9 curve by 1.83x.
FALLBACK_TS = {
    ("matmul", 500): 6.93, ("matmul", 1000): 76.04,
    ("matmul", 2000): 736.19, ("matmul", 4000): 6230.64,
    ("sumeuler", 10000): 1.69, ("sumeuler", 20000): 6.25, ("sumeuler", 40000): 27.39,
    ("cpi", 1000000): 0.57, ("cpi", 10000000): 3.82,
    ("cpi", 100000000): 38.95, ("cpi", 1000000000): 384.45,
    ("queens", 12): 106.95,
}

# {mean,X} ... "on N cores": pair each rep's mean time with its worker count.
# Erlang prints large floats as 1.510605e8, so allow an exponent.
MEAN_CORES = re.compile(r"\{mean,\s*([0-9.]+(?:[eE][+-]?\d+)?)\}.*?on\s+(\d+)\s+cores", re.S)
# seq_baselines line: "sumeuler 40000: mean 27.3800 s over ..."
SEQ_LINE = re.compile(r"^(\w+)\s+(\d+):\s*mean\s+([0-9.]+)\s*s", re.M)


def parse_seq_baselines(results_dir):
    """Return {(bench,size): Ts_seconds} from seq_baselines.txt if present."""
    out = {}
    f = results_dir / "seq_baselines.txt"
    if f.exists():
        for m in SEQ_LINE.finditer(f.read_text()):
            out[(m.group(1), int(m.group(2)))] = float(m.group(3))
    return out


def parse_farm(path):
    """Return {workers: mean_seconds} averaged over reps for one farm file."""
    per_worker = {}
    for m in MEAN_CORES.finditer(path.read_text()):
        secs = float(m.group(1)) / 1e6
        w = int(m.group(2))
        per_worker.setdefault(w, []).append(secs)
    return {w: st.mean(v) for w, v in per_worker.items()}


def bench_size(stem):
    """farm_<bench>_<size>[_<date>] -> (bench, size)."""
    parts = stem.split("_")            # ['farm','matmul','4000',('1903')]
    return parts[1], int(parts[2])


def write_dat(out_dir, base, metric, rows):
    out_dir.mkdir(parents=True, exist_ok=True)
    p = out_dir / f"{base}_elysium_{metric}.dat"
    with p.open("w") as f:
        f.write(f"workers {metric}\n")
        for w, val in rows:
            f.write(f"{w} {val:.4f}\n")
    return p


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    flags = {a for a in sys.argv[1:] if a.startswith("--")}
    results_dir = pathlib.Path(args[0] if args else "farm_results")
    if not results_dir.is_dir():
        raise SystemExit(f"no such directory: {results_dir}")
    # --pt-baseline: ignore this run's own seq_baselines.txt and use the PT
    # baselines above. Required whenever the overlay shares a panel with a PT
    # curve, since a speedup ratio is only meaningful if both series are
    # divided by the same Ts.
    seq = {} if "--pt-baseline" in flags else parse_seq_baselines(results_dir)

    farm_files = sorted(results_dir.glob("farm_*.txt"))
    if not farm_files:
        raise SystemExit(f"no farm_*.txt in {results_dir}")

    for path in farm_files:
        bench, size = bench_size(path.stem)
        ts = seq.get((bench, size)) or FALLBACK_TS.get((bench, size))
        if ts is None:
            print(f"! skip {path.name}: no Ts for ({bench},{size}) -- add to FALLBACK_TS")
            continue
        src = "seq_baselines" if (bench, size) in seq else "PT run csv2"
        means = parse_farm(path)
        if not means:
            print(f"! skip {path.name}: no timing data parsed")
            continue
        rows_sp = [(w, ts / means[w]) for w in sorted(means)]
        rows_ef = [(w, (ts / means[w]) / w) for w in sorted(means)]
        base = f"{bench}_{size}"
        out_dir = PAPER_RESULTS / bench
        write_dat(out_dir, base, "speedup", rows_sp)
        write_dat(out_dir, base, "efficiency", rows_ef)
        best = max(rows_sp, key=lambda r: r[1])
        print(f"{path.name:32s} Ts={ts:>8.2f}s ({src:12s}) "
              f"-> {bench}/{base}_elysium_*.dat   peak {best[1]:.2f}x @ {best[0]}w")


if __name__ == "__main__":
    main()
