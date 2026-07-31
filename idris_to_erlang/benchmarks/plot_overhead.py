#!/usr/bin/env python3
"""Generates overhead-vs-workers figures mirroring the existing *_speedup.png
figures in the paper, plotting overhead_ms (p*Tp - Ts, solid lines, left axis)
together with efficiency (speedup/p, dashed lines, right axis).

Reads from csv_overhead_added/ (dynamic-only benchmarks: cpi, queens) and
csv2_overhead_added/ (dynamic + fixed chunk-size sweeps: fib, matmul, sumeuler).
Writes PNGs into the paper's figures/ directory.
"""

import csv
import pathlib

import matplotlib.lines as mlines
import matplotlib.pyplot as plt

BENCH_DIR = pathlib.Path(__file__).parent
CSV_DIR = BENCH_DIR / "csv_overhead_added"
CSV2_DIR = BENCH_DIR / "csv2_overhead_added"
OUT_DIR = pathlib.Path(
    "/Users/cmb21/Papers/hlpp2026/paper/69ca3c4aadee49da6fc4aea4/figures"
)

# The paper (sn-jnl, single-column) has a text width of ~31pc (~5.15in).
# Figures are included at width=0.99\textwidth, so anything wider than that
# in inches gets shrunk on the page -- shrinking the embedded font sizes
# along with it. These rcParams are sized up to stay legible after that
# shrink for the figure widths used below (~8-10in).
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
MARKERS = ["s", "o", "^", "d", "v", "P", "X"]

STYLE_HANDLES = [
    mlines.Line2D([], [], color="black", linestyle="-", marker="o", label="overhead (left axis)"),
    mlines.Line2D([], [], color="black", linestyle="--", marker="o", markerfacecolor="none", label="efficiency (right axis)"),
]

STYLE_HANDLES_MERGED = [
    mlines.Line2D([], [], color="black", linestyle="-", marker="o", label="speedup (left axis)"),
    mlines.Line2D([], [], color="black", linestyle="--", marker="o", markerfacecolor="none", label="efficiency (right axis)"),
]


def load(path):
    with path.open(newline="") as f:
        rows = list(csv.DictReader(f))
    workers = [int(float(r["workers"])) for r in rows]
    overhead = [float(r["overhead_ms"]) for r in rows]
    efficiency = [float(r["efficiency"]) for r in rows]
    speedup = [float(r["speedup"]) for r in rows]
    return workers, overhead, efficiency, speedup


def plot_one_series(ax, ax2, path, label, color, marker):
    workers, overhead, efficiency, _speedup = load(path)
    line, = ax.plot(workers, overhead, marker=marker, color=color, label=label)
    ax2.plot(
        workers, efficiency, marker=marker, color=color, linestyle="--",
        alpha=0.6, markerfacecolor="none",
    )
    return line


def plot_one_series_merged(ax, ax2, path, label, color, marker):
    workers, _overhead, efficiency, speedup = load(path)
    line, = ax.plot(workers, speedup, marker=marker, color=color, label=label)
    ax2.plot(
        workers, efficiency, marker=marker, color=color, linestyle="--",
        alpha=0.6, markerfacecolor="none", markersize=5,
    )
    return line


def finish_axes(ax, ax2, series_handles, xlabel, ylabel_left, ylabel_right):
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel_left)
    ax2.set_ylabel(ylabel_right)
    ax2.set_ylim(bottom=0)
    ax.grid(alpha=0.3)
    all_handles = series_handles + STYLE_HANDLES
    ax.legend(
        handles=all_handles,
        loc="upper center",
        bbox_to_anchor=(0.5, -0.18),
        ncol=min(4, len(all_handles)),
        borderaxespad=0.0,
    )


def format_axes_merged(ax, ax2, xlabel, ylabel_left, ylabel_right):
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel_left)
    ax2.set_ylabel(ylabel_right)
    ax.set_ylim(bottom=0)
    ax2.set_ylim(bottom=0)
    ax.grid(alpha=0.3)


def finish_axes_merged(ax, ax2, series_handles, xlabel, ylabel_left, ylabel_right):
    format_axes_merged(ax, ax2, xlabel, ylabel_left, ylabel_right)
    all_handles = series_handles + STYLE_HANDLES_MERGED
    ax.legend(
        handles=all_handles,
        loc="upper center",
        bbox_to_anchor=(0.5, -0.20),
        ncol=min(4, len(all_handles)),
        borderaxespad=0.0,
    )


def build_style_map(chunk_size_lists):
    """Assigns one (color, marker) per label, shared across all panels of a
    figure, so a label (e.g. "chunk=50") always renders identically no
    matter which panel's chunk list it came from -- required for a single
    shared legend to be accurate."""
    labels = ["dynamic"]
    for chunk_sizes in chunk_size_lists:
        for c in chunk_sizes:
            label = f"chunk={c}"
            if label not in labels:
                labels.append(label)
    return {
        label: (COLORS[i % len(COLORS)], MARKERS[i % len(MARKERS)])
        for i, label in enumerate(labels)
    }


def plot_series(ax, csv_dir, base_name, chunk_sizes, style_map=None):
    ax2 = ax.twinx()

    handles = []
    configs = [("dynamic", None)] + [(f"chunk={c}", c) for c in chunk_sizes]
    for i, (label, chunk) in enumerate(configs):
        path = csv_dir / (
            f"{base_name}.csv" if chunk is None else f"{base_name}_chunk_{chunk}.csv"
        )
        if not path.exists():
            continue
        if style_map is not None:
            color, marker = style_map[label]
        else:
            color = COLORS[i % len(COLORS)]
            marker = MARKERS[i % len(MARKERS)]
        handles.append(plot_one_series(ax, ax2, path, label, color, marker))

    finish_axes(ax, ax2, handles, "Workers", "Overhead (proc-ms)", "Efficiency")


def plot_series_merged(ax, csv_dir, base_name, chunk_sizes, add_legend=True, style_map=None):
    ax2 = ax.twinx()

    handles = []
    configs = [("dynamic", None)] + [(f"chunk={c}", c) for c in chunk_sizes]
    for i, (label, chunk) in enumerate(configs):
        path = csv_dir / (
            f"{base_name}.csv" if chunk is None else f"{base_name}_chunk_{chunk}.csv"
        )
        if not path.exists():
            continue
        if style_map is not None:
            color, marker = style_map[label]
        else:
            color = COLORS[i % len(COLORS)]
            marker = MARKERS[i % len(MARKERS)]
        handles.append(plot_one_series_merged(ax, ax2, path, label, color, marker))

    if add_legend:
        finish_axes_merged(ax, ax2, handles, "Workers", "Speedup", "Efficiency")
    else:
        format_axes_merged(ax, ax2, "Workers", "Speedup", "Efficiency")
    return handles


def fig_fib():
    fig, axes = plt.subplots(1, 2, figsize=(18, 5))
    fig.suptitle("Fib Overhead & Efficiency")

    plot_series(axes[0], CSV2_DIR, "fib_1000_25", [1, 10, 20, 35])
    axes[0].set_title("Fib 1000_25")

    plot_series(axes[1], CSV2_DIR, "fib_10000_20", [1, 100, 200, 350])
    axes[1].set_title("Fib 10000_20")

    fig.subplots_adjust(wspace=0.7)
    fig.savefig(OUT_DIR / "fib_overhead.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_cpi():
    fig, ax = plt.subplots(figsize=(10, 6))
    ax2 = ax.twinx()

    handles = []
    for i, n in enumerate([1000000, 10000000, 100000000, 1000000000]):
        color = COLORS[i % len(COLORS)]
        marker = MARKERS[i % len(MARKERS)]
        handles.append(
            plot_one_series(ax, ax2, CSV_DIR / f"cpi_{n}.csv", f"N={n}", color, marker)
        )

    ax.set_title("CPI - Overhead & Efficiency vs Workers")
    finish_axes(ax, ax2, handles, "Number of Workers", "Overhead (proc-ms)", "Efficiency")
    fig.savefig(OUT_DIR / "cpi_overhead.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_matmul():
    fig, axes = plt.subplots(2, 2, figsize=(20, 12))
    fig.suptitle("MatMul Overhead & Efficiency")

    configs = [
        ("matmul_500", [1, 10, 17]),
        ("matmul_1000", [1, 10, 20, 35]),
        ("matmul_2000", [1, 10, 20, 35, 50, 70]),
        ("matmul_4000", [50]),
    ]
    style_map = build_style_map([c for _, c in configs])
    for ax, (base_name, chunks) in zip(axes.flat, configs):
        plot_series(ax, CSV2_DIR, base_name, chunks, style_map=style_map)
        ax.set_title(f"N={base_name.split('_')[1]}")

    fig.subplots_adjust(wspace=0.8, hspace=0.35)
    fig.savefig(OUT_DIR / "matmul_overhead.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_queens_12():
    fig, ax = plt.subplots(figsize=(10, 6))
    ax2 = ax.twinx()

    handle = plot_one_series(
        ax, ax2, CSV_DIR / "queens_12.csv", "dynamic", COLORS[0], MARKERS[0]
    )

    ax.set_title("queens_12 - Overhead & Efficiency vs Workers")
    finish_axes(ax, ax2, [handle], "Number of Workers", "Overhead (proc-ms)", "Efficiency")
    fig.savefig(OUT_DIR / "queens_12_overhead.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_sumeuler():
    fig, axes = plt.subplots(2, 2, figsize=(20, 12))
    fig.suptitle("SumEuler Overhead & Efficiency")

    configs = [10000, 20000, 40000]
    for ax, n in zip(axes.flat, configs):
        plot_series(ax, CSV2_DIR, f"sumeuler_{n}", [1, 50, 100, 250])
        ax.set_title(f"N={n}")

    axes.flat[-1].axis("off")

    fig.subplots_adjust(wspace=0.8, hspace=0.35)
    fig.savefig(OUT_DIR / "sumeuler_overhead.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def shared_legend(fig, handles_lists):
    seen = {}
    for handles in handles_lists:
        for h in handles:
            seen.setdefault(h.get_label(), h)
    all_handles = list(seen.values()) + STYLE_HANDLES_MERGED
    fig.legend(
        handles=all_handles,
        loc="lower center",
        bbox_to_anchor=(0.5, 0.0),
        ncol=min(4, len(all_handles)),
        borderaxespad=0.0,
    )


def fig_fib_merge():
    fig, axes = plt.subplots(2, 1, figsize=(10, 9.5))
    fig.suptitle("Fib Efficiency & Speedup")

    chunk_lists = [[1, 10, 20, 35], [1, 100, 200, 350]]
    style_map = build_style_map(chunk_lists)

    h1 = plot_series_merged(axes[0], CSV2_DIR, "fib_1000_25", chunk_lists[0], add_legend=False, style_map=style_map)
    axes[0].set_title("Fib 1000_25")

    h2 = plot_series_merged(axes[1], CSV2_DIR, "fib_10000_20", chunk_lists[1], add_legend=False, style_map=style_map)
    axes[1].set_title("Fib 10000_20")

    fig.subplots_adjust(hspace=0.5, bottom=0.16)
    shared_legend(fig, [h1, h2])
    fig.savefig(OUT_DIR / "fib_merge.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_cpi_merge():
    fig, ax = plt.subplots(figsize=(10, 5.6))
    ax2 = ax.twinx()

    handles = []
    for i, n in enumerate([1000000, 10000000, 100000000, 1000000000]):
        color = COLORS[i % len(COLORS)]
        marker = MARKERS[i % len(MARKERS)]
        handles.append(
            plot_one_series_merged(ax, ax2, CSV_DIR / f"cpi_{n}.csv", f"N={n}", color, marker)
        )

    ax.set_title("CPI - Efficiency & Speedup vs Workers")
    finish_axes_merged(
        ax, ax2, handles, "Number of Workers", "Speedup", "Efficiency",
    )
    fig.savefig(OUT_DIR / "cpi_merge.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_matmul_merge():
    fig, axes = plt.subplots(2, 2, figsize=(14, 8.5))
    fig.suptitle("MatMul Efficiency & Speedup")

    configs = [
        ("matmul_500", [1, 10, 17]),
        ("matmul_1000", [1, 10, 20, 35]),
        ("matmul_2000", [1, 10, 20, 35, 50, 70]),
        ("matmul_4000", [50]),
    ]
    style_map = build_style_map([c for _, c in configs])
    all_handles = []
    for ax, (base_name, chunks) in zip(axes.flat, configs):
        all_handles.append(
            plot_series_merged(ax, CSV2_DIR, base_name, chunks, add_legend=False, style_map=style_map)
        )
        ax.set_title(f"N={base_name.split('_')[1]}")

    fig.subplots_adjust(wspace=0.65, hspace=0.45, bottom=0.16)
    shared_legend(fig, all_handles)
    fig.savefig(OUT_DIR / "matmul_merge.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_queens_12_merge():
    fig, ax = plt.subplots(figsize=(10, 5.6))
    ax2 = ax.twinx()

    handle = plot_one_series_merged(
        ax, ax2, CSV_DIR / "queens_12.csv", "dynamic", COLORS[0], MARKERS[0]
    )

    ax.set_title("queens_12 - Efficiency & Speedup vs Workers")
    finish_axes_merged(
        ax, ax2, [handle], "Number of Workers", "Speedup", "Efficiency",
    )
    fig.savefig(OUT_DIR / "queens_12_merge.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_sumeuler_merge():
    fig, axes = plt.subplots(2, 2, figsize=(14, 8.5))
    fig.suptitle("SumEuler Efficiency & Speedup")

    configs = [10000, 20000, 40000]
    style_map = build_style_map([[1, 50, 100, 250]])
    all_handles = []
    for ax, n in zip(axes.flat, configs):
        all_handles.append(
            plot_series_merged(ax, CSV2_DIR, f"sumeuler_{n}", [1, 50, 100, 250], add_legend=False, style_map=style_map)
        )
        ax.set_title(f"N={n}")

    axes.flat[-1].axis("off")

    fig.subplots_adjust(wspace=0.65, hspace=0.45, bottom=0.14)
    shared_legend(fig, all_handles)
    fig.savefig(OUT_DIR / "sumeuler_merge.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    fig_fib()
    fig_cpi()
    fig_matmul()
    fig_queens_12()
    fig_sumeuler()
    fig_fib_merge()
    fig_cpi_merge()
    fig_matmul_merge()
    fig_queens_12_merge()
    fig_sumeuler_merge()
    print("done")
