# Example datasets

Three fully **synthetic** input files for trying the EU SAE Application
Package end to end, plus the script that generated them.

| File | What it is | Where to select it |
|---|---|---|
| `survey_example.csv` | 3,708 synthetic households, 40 domains (`D01`–`D40`) in 5 regions (`R1`–`R5`), 2 years (2022, 2023) | Step 1 → Survey |
| `auxiliary_example.csv` | Domain-by-year covariates (education, unemployment, schooling, night-lights, urban share, population) | Step 1 → Auxiliary |
| `geometry_example.geojson` | A synthetic 8×5 grid of square polygons, one per domain | Step 1 → Geometry |
| `generate_example_data.py` | The exact, seeded script that produced the three files (Python 3, numpy only) | — |

## Quick start

1. In Step 1 (Data) — or the dashboard sidebar — select the three files above
   and set the **two analysis years to 2022 and 2023**. The year inputs do not
   follow the data automatically: left at their defaults (2012, 2013), the
   readiness check reports *"Requested analysis years (2012, 2013) are not
   available"*, the domain counts show 0, and a join error
   (*"'by' must specify uniquely valid columns"*) follows — all three are the
   same problem.
2. In Step 2 (Mapping), assign the columns as in the table below.
3. Answer **yes** to the PSU-consistency question, then run
   **Check Data Readiness** again; all tests should pass.

A second example -- real Spanish province geography and the dataset used in
the guidance notes -- ships in `Data/Spain/`. It carries third-party licence terms
(GPL-2); see `Data/Spain/README.md`. Use the synthetic files here for anything you
intend to pass on.

## Suggested variable mapping (Step 2)

| App field | Column |
|---|---|
| year | `year` |
| domain | `domain` |
| psu | `psu` |
| strata (optional) | `strata` |
| weight | `weight` |
| household size | `hhsize` |
| welfare | `income` |
| poverty line (column) | `povline` |
| benchmark level (grouped) | `region` |
| auxiliary / geometry join key | `domain` |

PSU identifiers are consistent across the two years (answer **yes** to the PSU
consistency question), so the cross-year covariance used by the MFH models is
estimable. The poverty line is 60% of the person-weighted national median
income, per year.

## What the data deliberately contains

- Persistent domain effects across years, so the multivariate (MFH) models have
  a genuine covariance to exploit.
- Four domains with real changes between 2022 and 2023 (`D07`, `D19`, `D28`,
  `D33`) — candidates for the significance tests.
- Two deliberately small domains (`D05`, `D31`, 15 households each) and a few
  domains with **zero observed poverty** in a year, so the small-area and
  boundary-domain behaviour of the package is visible with the example data.
- Domain-level covariates constructed to correlate strongly (|r| ≈ 0.85) with
  the true poverty rates, so covariate selection has real signal to find.

## Provenance and licence

Every value was drawn from documented random distributions by
`generate_example_data.py` (seed 20260826). **No real survey, census,
administrative, or geographic data was used or transformed in any way**; the
geometry is an abstract grid, not any real territory. The files therefore
carry the same licence as the package itself and are safe to redistribute
with it. They are for learning and testing only — never as an input to any
real estimate.

## Regenerate the examples

From the package root, run `python Data/simulated/generate_example_data.py`.
Python 3.10+ and numpy are required. The generator writes beside itself, not
into the current working directory. Run it in a disposable package copy for
verification so the distributed example files remain untouched.
