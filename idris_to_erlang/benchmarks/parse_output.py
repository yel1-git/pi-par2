#!/usr/bin/env python3
"""Turn the raw Parallel-Types benchmark logs in output/ into the per-benchmark
CSVs that add_overhead_columns.py and export_pgfplots.py consume.

This stage was missing from the repo: output/ held the raw sk_profile logs and
csv/ + csv2/ held the parsed results, but nothing in the tree converted one to
the other, so a re-run could not be carried through to the paper.

Reads, for each (bench, size):
    output/<bench>_seq_<size>.txt                  -> Ts (mean over reps)
    output/<bench>_par_<size>.txt                  -> Tp per worker count
    output/<bench>_par_<size>_chunk_<c>.txt        -> Tp for a fixed chunk size
Writes:
    <outdir>/<bench>_<size>.csv
    <outdir>/<bench>_<size>_chunk_<c>.csv
with columns  workers,seq_time_ms,par_time_ms,speedup  -- matching the existing
csv2/ files exactly, so the rest of the pipeline is unchanged.

Times in the logs are microseconds (sk_profile / timer:tc); CSVs are milliseconds.

Usage:
    python3 parse_output.py                 # output/ -> csv2/   (the vintage the paper plots)
    python3 parse_output.py --outdir csv    # output/ -> csv/
    python3 parse_output.py --indir output2 --outdir csv2
"""

import argparse
import csv
import pathlib
import re
import statistics as st

BENCH_DIR = pathlib.Path(__file__).parent

# "{mean,26003402.0}" and Erlang's exponent form "{mean,1.777319e8}" alike.
MEAN = re.compile(r"\{mean,\s*([0-9.]+(?:[eE][+-]?\d+)?)\}")
# "=== 12 workers ===" section headers in the parallel logs.
WORKERS_HDR = re.compile(r"^===\s*(\d+)\s*workers\s*===", re.M)
# seq log:  <bench>_seq_<size>.txt ; par log: <bench>_par_<size>[_chunk_<c>].txt
SEQ_NAME = re.compile(r"^(?P<bench>[a-z]+)_seq_(?P<size>\d+)$")
PAR_NAME = re.compile(r"^(?P<bench>[a-z]+)_par_(?P<size>\d+)(?:_chunk_(?P<chunk>\d+))?$")


def means_us(text):
    """All {mean,X} values in a blob, in microseconds."""
    return [float(m.group(1)) for m in MEAN.finditer(text)]


def parse_seq(path):
    """Mean sequential time in ms over all reps in a seq log."""
    vals = means_us(path.read_text())
    if not vals:
        return None
    return st.mean(vals) / 1000.0


def parse_par(path):
    """{workers: mean_ms} from a parallel log, averaging the reps in each section."""
    text = path.read_text()
    hdrs = list(WORKERS_HDR.finditer(text))
    if not hdrs:
        return {}
    out = {}
    for i, h in enumerate(hdrs):
        end = hdrs[i + 1].start() if i + 1 < len(hdrs) else len(text)
        vals = means_us(text[h.end():end])
        if vals:
            out.setdefault(int(h.group(1)), []).extend(vals)
    return {w: st.mean(v) / 1000.0 for w, v in out.items()}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--indir", default="output")
    ap.add_argument("--outdir", default="csv2")
    ap.add_argument("--glob", default="*_par_*.txt",
                    help="which parallel logs to convert, e.g. 'sumeuler_par_*.txt'. "
                         "Restrict this when only some benchmarks were re-run: the "
                         "default would overwrite every CSV in --outdir, including "
                         "ones whose logs in --indir are an older vintage.")
    args = ap.parse_args()

    in_dir = BENCH_DIR / args.indir
    out_dir = BENCH_DIR / args.outdir
    if not in_dir.is_dir():
        raise SystemExit(f"no such directory: {in_dir}")
    out_dir.mkdir(exist_ok=True)

    # Collect sequential baselines first: one Ts per (bench, size).
    seq = {}
    for path in sorted(in_dir.glob("*_seq_*.txt")):
        m = SEQ_NAME.match(path.stem)
        if not m:
            print(f"! skip {path.name}: unrecognised seq log name")
            continue
        ts = parse_seq(path)
        if ts is None:
            print(f"! skip {path.name}: no {{mean,_}} values found")
            continue
        seq[(m.group("bench"), int(m.group("size")))] = ts

    if not seq:
        raise SystemExit(f"no usable *_seq_*.txt in {in_dir}")

    written = 0
    for path in sorted(in_dir.glob(args.glob)):
        m = PAR_NAME.match(path.stem)
        if not m:
            print(f"! skip {path.name}: unrecognised par log name")
            continue
        bench, size = m.group("bench"), int(m.group("size"))
        ts = seq.get((bench, size))
        if ts is None:
            print(f"! skip {path.name}: no sequential baseline for ({bench},{size})"
                  f" -- run the *_seq_* script too")
            continue
        pars = parse_par(path)
        if not pars:
            print(f"! skip {path.name}: no per-worker timings parsed")
            continue

        stem = f"{bench}_{size}"
        if m.group("chunk"):
            stem += f"_chunk_{m.group('chunk')}"
        out_path = out_dir / f"{stem}.csv"
        with out_path.open("w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["workers", "seq_time_ms", "par_time_ms", "speedup"])
            for nw in sorted(pars):
                tp = pars[nw]
                w.writerow([nw, round(ts, 2), round(tp, 2), round(ts / tp, 2)])
        best_w = max(pars, key=lambda k: ts / pars[k])
        print(f"{path.name:38s} Ts={ts/1000:8.2f}s -> {out_path.name:34s} "
              f"peak {ts/pars[best_w]:6.2f}x @{best_w:2d}w")
        written += 1

    print(f"\nwrote {written} CSVs to {out_dir}")
    print("next: python3 add_overhead_columns.py && python3 export_pgfplots.py")


if __name__ == "__main__":
    main()
