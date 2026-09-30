#!/usr/bin/env python3
"""Compare Tupa and the prototype mHEM code against Grcev Fig. 12 data."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd


RHO_VALUES = (30.0, 300.0, 3000.0)


def interpolated_magnitude(frame: pd.DataFrame, rho: float, frequencies: np.ndarray) -> np.ndarray:
    case = frame.loc[frame["rho_ohm_m"] == rho].sort_values("frequency_hz")
    return np.interp(np.log10(frequencies), np.log10(case["frequency_hz"]), case["z_magnitude_ohm"])


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--tupa", required=True, type=Path)
    parser.add_argument("--mhem", required=True, type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    parser.add_argument("--results", required=True, type=Path)
    parser.add_argument("--metrics", required=True, type=Path)
    parser.add_argument("--reference-results", required=True, type=Path)
    args = parser.parse_args()

    tupa = pd.read_csv(args.tupa)
    mhem = pd.read_csv(args.mhem)
    combined = pd.concat([tupa, mhem], ignore_index=True)
    args.results.parent.mkdir(parents=True, exist_ok=True)
    combined.to_csv(args.results, index=False)

    raw_reference = pd.read_excel(args.reference, sheet_name=0, header=None)
    metrics: list[dict[str, float | str]] = []
    reference_rows: list[dict[str, float]] = []

    for column, rho in zip((0, 2, 4), RHO_VALUES):
        values = raw_reference.iloc[3:, column : column + 2].dropna().astype(float)
        reference_frequency = values.iloc[:, 0].to_numpy()
        reference_magnitude = values.iloc[:, 1].to_numpy()
        predicted = {}
        for solver, frame in (("Tupa", tupa), ("mHEM", mhem)):
            predicted[solver] = interpolated_magnitude(frame, rho, reference_frequency)
            for scope, mask in (("100 Hz–10 MHz", np.ones(len(reference_frequency), dtype=bool)),
                                ("100 Hz–1 MHz", reference_frequency <= 1e6)):
                observed = reference_magnitude[mask]
                estimate = predicted[solver][mask]
                relative_error = (estimate - observed) / observed
                db_error = 20 * np.log10(estimate / observed)
                metrics.append(
                    {
                        "comparison": f"{solver} vs full-wave reference",
                        "scope": scope,
                        "rho_ohm_m": rho,
                        "points": np.count_nonzero(mask),
                        "mean_abs_percent_error": 100 * np.mean(np.abs(relative_error)),
                        "median_abs_percent_error": 100 * np.median(np.abs(relative_error)),
                        "max_abs_percent_error": 100 * np.max(np.abs(relative_error)),
                        "relative_l2_percent": 100 * np.linalg.norm(estimate - observed) / np.linalg.norm(observed),
                        "mean_abs_db_error": np.mean(np.abs(db_error)),
                    }
                )

        solver_difference = (predicted["Tupa"] - predicted["mHEM"]) / predicted["mHEM"]
        for scope, mask in (("100 Hz–10 MHz", np.ones(len(reference_frequency), dtype=bool)),
                            ("100 Hz–1 MHz", reference_frequency <= 1e6)):
            metrics.append(
                {
                    "comparison": "Tupa vs mHEM at reference frequencies",
                    "scope": scope,
                    "rho_ohm_m": rho,
                    "points": np.count_nonzero(mask),
                    "mean_abs_percent_error": 100 * np.mean(np.abs(solver_difference[mask])),
                    "median_abs_percent_error": 100 * np.median(np.abs(solver_difference[mask])),
                    "max_abs_percent_error": 100 * np.max(np.abs(solver_difference[mask])),
                    "relative_l2_percent": 100 * np.linalg.norm(predicted["Tupa"][mask] - predicted["mHEM"][mask]) / np.linalg.norm(predicted["mHEM"][mask]),
                    "mean_abs_db_error": np.mean(np.abs(20 * np.log10(predicted["Tupa"][mask] / predicted["mHEM"][mask]))),
                }
            )
        for index, frequency in enumerate(reference_frequency):
            reference_rows.append(
                {
                    "rho_ohm_m": rho,
                    "frequency_hz": frequency,
                    "reference_magnitude_ohm": reference_magnitude[index],
                    "tupa_magnitude_ohm": predicted["Tupa"][index],
                    "mhem_magnitude_ohm": predicted["mHEM"][index],
                    "tupa_error_percent": 100 * (predicted["Tupa"][index] - reference_magnitude[index]) / reference_magnitude[index],
                    "mhem_error_percent": 100 * (predicted["mHEM"][index] - reference_magnitude[index]) / reference_magnitude[index],
                }
            )

    metrics_frame = pd.DataFrame(metrics)
    metrics_frame.to_csv(args.metrics, index=False)
    pd.DataFrame(reference_rows).to_csv(args.reference_results, index=False)


if __name__ == "__main__":
    main()
