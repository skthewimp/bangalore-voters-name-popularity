#!/usr/bin/env python3
"""
Name popularity visualizer by inferred birth year.

Birth year is inferred as:
    birth_year = reference_year - age

This script is built to avoid noisy tails by default:
- drops years with tiny total sample sizes
- drops years where the queried name has very low counts
"""

from __future__ import annotations

import argparse
import csv
import re
from collections import Counter
from dataclasses import dataclass
from pathlib import Path


NAME_CLEAN_RE = re.compile(r"[^a-z]")


@dataclass
class YearRow:
    year: int
    total_people: int
    name_count: int
    share_pct: float


def normalize_name(value: str) -> str:
    return NAME_CLEAN_RE.sub("", (value or "").strip().lower())


def rolling_mean(values: list[float], window: int) -> list[float]:
    if window <= 1:
        return values[:]
    out = []
    half = window // 2
    for i in range(len(values)):
        lo = max(0, i - half)
        hi = min(len(values), i + half + 1)
        chunk = values[lo:hi]
        out.append(sum(chunk) / len(chunk))
    return out


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Visualize first-name popularity by inferred birth year."
    )
    parser.add_argument("--csv", default="bangalore first names.csv", help="Input CSV file")
    parser.add_argument("--name", required=True, help="Name to query (first-name token)")
    parser.add_argument("--sex", choices=["M", "F", "all"], default="all", help="Sex filter")
    parser.add_argument(
        "--reference-year",
        type=int,
        default=2018,
        help="Reference year used to infer birth year from age",
    )
    parser.add_argument("--min-age", type=int, default=18, help="Minimum included age")
    parser.add_argument("--max-age", type=int, default=110, help="Maximum included age")
    parser.add_argument(
        "--min-total-per-year",
        type=int,
        default=5000,
        help="Drop years with denominator below this threshold",
    )
    parser.add_argument(
        "--min-name-count-per-year",
        type=int,
        default=20,
        help="Drop years where name count is below this threshold",
    )
    parser.add_argument("--start-year", type=int, default=None, help="Optional year lower bound")
    parser.add_argument("--end-year", type=int, default=None, help="Optional year upper bound")
    parser.add_argument(
        "--smooth-window",
        type=int,
        default=3,
        help="Centered rolling mean window for plotted trend",
    )
    parser.add_argument(
        "--out-csv",
        default=None,
        help="Output CSV path (default: outputs/<name>_popularity.csv)",
    )
    parser.add_argument(
        "--out-png",
        default=None,
        help="Output PNG path (default: outputs/<name>_popularity.png)",
    )
    return parser.parse_args()


def collect_year_rows(
    csv_path: Path,
    query_name: str,
    sex_filter: str,
    reference_year: int,
    min_age: int,
    max_age: int,
) -> tuple[list[YearRow], dict[str, int]]:
    totals = Counter()
    hits = Counter()
    meta = Counter()

    with csv_path.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        cols = set(reader.fieldnames or [])
        age_col = "Age" if "Age" in cols else None
        word_col = "word" if "word" in cols else None
        name_col = "Name" if "Name" in cols else None
        sex_col = "Sex" if "Sex" in cols else None

        if age_col is None:
            raise ValueError("Could not find 'Age' column")

        for row in reader:
            meta["rows_seen"] += 1

            age_raw = (row.get(age_col) or "").strip()
            if not age_raw:
                meta["skip_missing_age"] += 1
                continue

            try:
                age = int(float(age_raw))
            except ValueError:
                meta["skip_bad_age"] += 1
                continue

            if age < min_age or age > max_age:
                meta["skip_age_out_of_bounds"] += 1
                continue

            if sex_filter != "all":
                if (row.get(sex_col) or "").strip().upper() != sex_filter:
                    meta["skip_sex"] += 1
                    continue

            birth_year = reference_year - age
            totals[birth_year] += 1
            meta["rows_used"] += 1

            token = ""
            if word_col:
                token = normalize_name(row.get(word_col) or "")
            if not token and name_col:
                token = normalize_name((row.get(name_col) or "").split(" ")[0])

            if token == query_name:
                hits[birth_year] += 1

    rows = []
    for year in sorted(totals):
        total = totals[year]
        count = hits[year]
        share = (count / total) * 100.0 if total else 0.0
        rows.append(YearRow(year=year, total_people=total, name_count=count, share_pct=share))
    return rows, dict(meta)


def filter_rows(
    rows: list[YearRow],
    min_total_per_year: int,
    min_name_count_per_year: int,
    start_year: int | None,
    end_year: int | None,
) -> list[YearRow]:
    out = []
    for row in rows:
        if row.total_people < min_total_per_year:
            continue
        if row.name_count < min_name_count_per_year:
            continue
        if start_year is not None and row.year < start_year:
            continue
        if end_year is not None and row.year > end_year:
            continue
        out.append(row)
    return out


def write_csv(path: Path, rows: list[YearRow], smooth_window: int) -> None:
    smoothed = rolling_mean([r.share_pct for r in rows], smooth_window)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(
            ["year", "total_people", "name_count", "share_pct", "share_pct_smoothed"]
        )
        for r, s in zip(rows, smoothed):
            writer.writerow([r.year, r.total_people, r.name_count, f"{r.share_pct:.6f}", f"{s:.6f}"])


def make_plot(path: Path, rows: list[YearRow], query_name: str, sex_filter: str, smooth_window: int) -> None:
    try:
        import matplotlib.pyplot as plt
    except Exception as exc:
        raise RuntimeError(
            "matplotlib is required for PNG output. Install it with: pip3 install matplotlib"
        ) from exc

    years = [r.year for r in rows]
    shares = [r.share_pct for r in rows]
    smoothed = rolling_mean(shares, smooth_window)

    fig, ax = plt.subplots(figsize=(11, 6), dpi=150)
    ax.plot(years, shares, color="#7f8c8d", linewidth=1.2, alpha=0.7, label="yearly share")
    ax.plot(years, smoothed, color="#1f77b4", linewidth=2.2, label=f"smoothed ({smooth_window}-year)")
    ax.set_title(
        f"Popularity of '{query_name}' by inferred birth year ({sex_filter})",
        fontweight="bold",
    )
    ax.set_xlabel("Inferred birth year")
    ax.set_ylabel("Share of people with name (%)")
    ax.grid(alpha=0.2, linewidth=0.5)
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(path)
    plt.close(fig)


def main() -> None:
    args = parse_args()

    csv_path = Path(args.csv)
    if not csv_path.exists():
        raise FileNotFoundError(f"CSV not found: {csv_path}")

    query_name = normalize_name(args.name)
    if not query_name:
        raise ValueError("Query name is empty after normalization")

    out_dir = Path("outputs")
    out_csv = Path(args.out_csv) if args.out_csv else out_dir / f"{query_name}_popularity.csv"
    out_png = Path(args.out_png) if args.out_png else out_dir / f"{query_name}_popularity.png"
    out_csv.parent.mkdir(parents=True, exist_ok=True)
    out_png.parent.mkdir(parents=True, exist_ok=True)

    rows, meta = collect_year_rows(
        csv_path=csv_path,
        query_name=query_name,
        sex_filter=args.sex,
        reference_year=args.reference_year,
        min_age=args.min_age,
        max_age=args.max_age,
    )

    filtered = filter_rows(
        rows=rows,
        min_total_per_year=args.min_total_per_year,
        min_name_count_per_year=args.min_name_count_per_year,
        start_year=args.start_year,
        end_year=args.end_year,
    )

    if not filtered:
        raise RuntimeError(
            "No years left after filtering. Lower --min-total-per-year or --min-name-count-per-year."
        )

    write_csv(path=out_csv, rows=filtered, smooth_window=args.smooth_window)
    make_plot(
        path=out_png,
        rows=filtered,
        query_name=query_name,
        sex_filter=args.sex,
        smooth_window=args.smooth_window,
    )

    print(f"Saved CSV: {out_csv}")
    print(f"Saved PNG: {out_png}")
    print(
        "Applied filters:"
        f" min_total_per_year={args.min_total_per_year},"
        f" min_name_count_per_year={args.min_name_count_per_year},"
        f" start_year={args.start_year}, end_year={args.end_year}"
    )
    print(
        "Meta:"
        f" rows_seen={meta.get('rows_seen', 0)}"
        f" rows_used={meta.get('rows_used', 0)}"
        f" skip_missing_age={meta.get('skip_missing_age', 0)}"
        f" skip_bad_age={meta.get('skip_bad_age', 0)}"
        f" skip_age_out_of_bounds={meta.get('skip_age_out_of_bounds', 0)}"
        f" skip_sex={meta.get('skip_sex', 0)}"
    )
    print("Note: inferred birth year is approximate (reference_year - age).")


if __name__ == "__main__":
    main()
