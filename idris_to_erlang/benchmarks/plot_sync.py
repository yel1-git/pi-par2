#!/usr/bin/env python3
"""Generates the overhead-decomposition figure for the sync-instrumented runs,
mirroring the house style of plot_overhead.py (twin-axis, sized-up rcParams so
the ~0.99\\textwidth include stays legible in the sn-jnl single column).

For each profiled benchmark (cpi 1e8, matmul 4000, sumeuler 40000) we plot, vs
worker count:

  - solid line, left axis  : process-memory peak (GB)  -- the copy-based
                             message-passing cost the speedup plots don't show
  - dashed line, right axis: sync-barrier fraction wait_time / wall_time
                             -- ~1 when workers are the bottleneck (good),
                             collapsing when a sequential fraction dominates
                             (the CPI plateau).

Reads csv_sync/*.csv (produced by aggregate_sync.py). Writes one PNG into the
paper's figures/ directory, next to the existing *_overhead.png figures.
"""

import csv
import pathlib

import matplotlib.lines as mlines
import matplotlib.pyplot as plt

BENCH_DIR = pathlib.Path(__file__).parent
CSV_DIR = BENCH_DIR / "csv_sync"
OUT_DIR = pathlib.Path(
    "/Users/cmb21/Papers/hlpp2026/paper/69ca3c4aadee49da6fc4aea4/figures"
)

# Matched to plot_overhead.py so the new figure renders at the same weight.
plt.rcParams.update({
    "font.size": 15,
    "axes.titlesize": 19,
    "axes.labelsize": 17,
    "xtick.labelsize": 14,
    "ytick.labelsize": 14,
    "legend.fontsize": 13,
    "figure.titlesize": 23,
    "lines.linewidth": 2.2,
    "lines.markersize": 8,
})

COLORS = plt.rcParams["axes.prop_cycle"].by_key()["color"]

# One panel per profiled benchmark: (csv stem, panel title, color, marker).
PANELS = [
    ("cpi_sync_100000000", r"CPI $N=10^8$",        COLORS[0], "o"),
    ("sumeuler_sync_40000", r"SumEuler $N=4\times10^4$", COLORS[1], "s"),
    ("matmul_sync_4000",   r"MatMul $4000^2$",     COLORS[2], "^"),
]

STYLE_HANDLES = [
    mlines.Line2D([], [], color="black", linestyle="-", marker="o",
                  label="process-memory peak (left axis)"),
    mlines.Line2D([], [], color="black", linestyle="--", marker="o",
                  markerfacecolor="none", label="sync fraction (right axis)"),
]


def load(stem):
    with (CSV_DIR / f"{stem}.csv").open(newline="") as f:
        rows = list(csv.DictReader(f))
    workers = [int(r["workers"]) for r in rows]
    mem_gb = [float(r["mem_proc_peak_mb"]) / 1e3 for r in rows]
    wait_frac = [float(r["wait_frac"]) for r in rows]
    return workers, mem_gb, wait_frac


def plot_panel(ax, stem, title, color, marker):
    ax2 = ax.twinx()
    workers, mem_gb, wait_frac = load(stem)

    ax.plot(workers, mem_gb, marker=marker, color=color, linestyle="-")
    ax2.plot(workers, wait_frac, marker=marker, color=color, linestyle="--",
             alpha=0.6, markerfacecolor="none", markersize=6)

    ax.set_title(title)
    ax.set_xlabel("Workers")
    ax.set_ylabel("Process-memory peak (GB)")
    ax2.set_ylabel("Sync fraction")
    ax.set_ylim(bottom=0)
    ax2.set_ylim(0, 1.05)
    ax.grid(alpha=0.3)


def main():
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    fig, axes = plt.subplots(1, 3, figsize=(21, 6))
    fig.suptitle("Synchronisation Cost and Memory Footprint vs Workers")

    for ax, (stem, title, color, marker) in zip(axes, PANELS):
        plot_panel(ax, stem, title, color, marker)

    fig.legend(handles=STYLE_HANDLES, loc="lower center",
               bbox_to_anchor=(0.5, -0.02), ncol=2, borderaxespad=0.0)
    fig.subplots_adjust(wspace=0.45, bottom=0.22)

    out = OUT_DIR / "sync_overhead.png"
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
