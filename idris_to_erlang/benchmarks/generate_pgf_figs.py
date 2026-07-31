#!/usr/bin/env python3
"""Generate the pgfplots figure blocks (fib, sumeuler, matmul) for sn-article.tex.

Each figure is a grid of twin-axis panels (speedup solid/left, efficiency
dashed/right) reading the 2-column data files under results/<bench>/. A single
shared legend (union of configs across the figure's panels) sits below.

Writes one .tex snippet per figure into the paper dir (fig_<name>.tex); those
are inlined into sn-article.tex. Run from the benchmarks/ directory.
"""

import pathlib

OUT_DIR = pathlib.Path(
    "/Users/cmb21/Papers/hlpp2026/paper/69ca3c4aadee49da6fc4aea4"
)

COLORS = ["cA", "cB", "cC", "cD", "cE", "cF", "cG", "cH", "cI"]
MARKS_SOLID = ["square*", "*", "triangle*", "diamond*", "pentagon*",
               "otimes*", "oplus*", "star", "x"]
MARKS_HOLLOW = ["square", "o", "triangle", "diamond", "pentagon",
                "otimes", "oplus", "star", "x"]

PANEL_W = "0.47\\linewidth"  # half-width panels (2 per row), matching Fig. 6
PANEL_H = "4.6cm"

# Elysium task-farm overlay (drawn on the speedup axis where the data exists).
ELYSIUM_STYLE = "black, thick, dash dot, mark=x, mark size=2pt"


def elysium_speedup(bench, base):
    """Relative .dat path for the Elysium farm overlay, or None if not present
    yet (e.g. fib has no farm run, CPI before the server run)."""
    rel = f"results/{bench}/{base}_elysium_speedup.dat"
    return rel if (OUT_DIR / rel).exists() else None

# (bench, [(title, csv_stem_base, [chunk sizes]) ...], columns, xmax)
FIGURES = {
    "fib": dict(
        bench="fib", cols=2, xmax=28,
        caption=("Speedup (solid, left axis) and efficiency (dashed, right "
                 "axis) for Fibonacci 25 executed 1000 times and Fibonacci 20 "
                 "executed 10000 times."),
        label="fig:fib",
        # Single row, so a touch taller than the 2-row grids.
        panel_h="5.2cm",
        panels=[
            ("Fib $25\\times1000$", "fib_1000_25", [1, 10, 20, 35]),
            ("Fib $20\\times10000$", "fib_10000_20", [1, 100, 200, 350]),
        ],
    ),
    "sumeuler": dict(
        bench="sumeuler", cols=2, xmax=28,
        caption=("Speedup (solid, left axis) and efficiency (dashed, right "
                 "axis) for SumEuler for three workloads: $10^4$, "
                 "$2\\times10^4$, $4\\times10^4$."),
        label="fig:sumeuler",
        panels=[
            ("$N=10^4$", "sumeuler_10000", [1, 50, 100, 250]),
            ("$N=2\\times10^4$", "sumeuler_20000", [1, 50, 100, 250]),
            ("$N=4\\times10^4$", "sumeuler_40000", [1, 50, 100, 250]),
        ],
    ),
    "matmul": dict(
        bench="matmul", cols=2, xmax=28,
        caption=("Speedup (solid, left axis) and efficiency (dashed, right "
                 "axis) for matrix multiplication for $500^2$, $1000^2$, "
                 "$2000^2$ and $4000^2$ sized matrices."),
        label="fig:matmul",
        panels=[
            ("$N=500^2$", "matmul_500", [1, 10, 17]),
            ("$N=1000^2$", "matmul_1000", [1, 10, 20, 35]),
            ("$N=2000^2$", "matmul_2000", [1, 10, 20, 35, 50, 70]),
            ("$N=4000^2$", "matmul_4000", [50]),
        ],
    ),
}


def configs(base, chunks):
    """Ordered (label, stem) for a panel: dynamic first, then chunks."""
    out = [("dynamic", base)]
    for c in chunks:
        out.append((f"chunk={c}", f"{base}_chunk_{c}"))
    return out


def style_map(spec):
    """Global label -> style index, union across panels, dynamic then chunks
    ascending, so a config keeps one colour/mark throughout the figure."""
    labels = ["dynamic"]
    chunks = set()
    for _, _, cs in spec["panels"]:
        chunks.update(cs)
    for c in sorted(chunks):
        labels.append(f"chunk={c}")
    return {lab: i for i, lab in enumerate(labels)}, labels


def panel_tex(spec, title, base, chunks, smap, show_speedup_label,
              show_eff_label):
    bench = spec["bench"]
    xmax = spec["xmax"]
    pw = spec.get("panel_w", PANEL_W)
    ph = spec.get("panel_h", PANEL_H)
    sp_ylabel = "" if show_speedup_label else "ylabel={},"
    ef_ylabel = "" if show_eff_label else "ylabel={},"
    lines = ["\\begin{tikzpicture}"]
    # speedup axis (left, solid)
    lines.append(f"\\begin{{axis}}[speedup axis, width={pw}, "
                 f"height={ph}, xmax={xmax}, title={{{title}}}, {sp_ylabel}]")
    for lab, stem in configs(base, chunks):
        i = smap[lab]
        lines.append(
            f"  \\addplot[{COLORS[i]}, mark={MARKS_SOLID[i]}, mark size=1.6pt] "
            f"table[x=workers,y=speedup] {{results/{bench}/{stem}_speedup.dat}};")
    ely = elysium_speedup(bench, base)
    if ely:
        lines.append(f"  \\addplot[{ELYSIUM_STYLE}] "
                     f"table[x=workers,y=speedup] {{{ely}}};")
    lines.append("\\end{axis}")
    # efficiency axis (right, dashed)
    lines.append(f"\\begin{{axis}}[efficiency axis, width={pw}, "
                 f"height={ph}, xmax={xmax}, ymax=1.1, {ef_ylabel}]")
    for lab, stem in configs(base, chunks):
        i = smap[lab]
        lines.append(
            f"  \\addplot[{COLORS[i]}, dashed, mark={MARKS_HOLLOW[i]}, "
            f"mark size=1.6pt, mark options={{solid}}] "
            f"table[x=workers,y=efficiency] {{results/{bench}/{stem}_efficiency.dat}};")
    lines.append("\\end{axis}")
    lines.append("\\end{tikzpicture}")
    return "\n".join(lines)


def legend_tex(spec, smap, labels, has_elysium=False):
    """Shared legend as an inline row of TikZ line+mark samples (no axis, so no
    minimum-plot-height check). Entries wrap across the text width."""
    items = []
    for lab in labels:
        i = smap[lab]
        sample = ("\\tikz[baseline=-0.6ex]{\\draw[%s,mark=%s,mark size=1.6pt] "
                  "plot coordinates {(0,0) (0.45,0)};}" % (COLORS[i], MARKS_SOLID[i]))
        items.append(f"{sample}~{escape(lab)}")
    if has_elysium:
        sample = ("\\tikz[baseline=-0.6ex]{\\draw[%s] "
                  "plot coordinates {(0,0) (0.45,0)};}" % ELYSIUM_STYLE)
        items.append(f"{sample}~Elysium farm")
    return "{\\footnotesize\\centering\n" + "\\quad\n".join(items) + "\\par}"


def escape(lab):
    return lab.replace("=", "{=}")


def figure_tex(name, spec):
    smap, labels = style_map(spec)
    cols = spec["cols"]
    body = ["\\begin{figure}[t]", "\\centering"]
    panels = spec["panels"]
    for idx, (title, base, chunks) in enumerate(panels):
        col = idx % cols
        # last panel alone on its row -> show both labels
        alone = (col == 0 and idx == len(panels) - 1)
        show_sp = (col == 0) or alone
        show_ef = (col == cols - 1) or alone
        body.append(panel_tex(spec, title, base, chunks, smap, show_sp, show_ef))
        if col == cols - 1 and idx != len(panels) - 1:
            body.append("\\\\[1ex]")
        elif col != cols - 1 and idx != len(panels) - 1:
            body.append("\\hfill")
    has_elysium = any(elysium_speedup(spec["bench"], base)
                      for _title, base, _chunks in panels)
    body.append("\\\\[1.5ex]")
    body.append(legend_tex(spec, smap, labels, has_elysium))
    body.append(f"\\caption{{{spec['caption']}}}")
    body.append(f"\\label{{{spec['label']}}}")
    body.append("\\end{figure}")
    return "\n".join(body)


def main():
    for name, spec in FIGURES.items():
        tex = figure_tex(name, spec)
        out = OUT_DIR / f"fig_{name}.tex"
        out.write_text(tex + "\n")
        print(f"wrote {out} ({tex.count(chr(10))+1} lines)")


if __name__ == "__main__":
    main()
