"""Generate the synthetic example datasets shipped with the EU SAE package.

Seeded and deterministic: running this script reproduces the shipped files
byte-for-byte (Python 3.10+, numpy). No real data of any kind is used.
Outputs: survey_example.csv, auxiliary_example.csv, geometry_example.geojson.
"""
import numpy as np, json, csv
from pathlib import Path
OUTPUT_DIR = Path(__file__).resolve().parent
rng = np.random.default_rng(20260826)

REGIONS = [f"R{i}" for i in range(1, 6)]
DOMAINS = [f"D{i:02d}" for i in range(1, 41)]
dom_region = {d: REGIONS[(i - 1) // 8] for i, d in enumerate(DOMAINS, 1)}
YEARS = [2022, 2023]

u = dict(zip(DOMAINS, rng.normal(0, 0.35, len(DOMAINS))))     # persistent domain effects
year_eff = {2022: 0.0, 2023: 0.03}
special = {"D07": -0.12, "D19": -0.10, "D28": +0.10, "D33": -0.08}  # real changes in 2023
small = {"D05", "D31"}                                        # deliberately small samples
BASE = np.log(12000)

rows = []
for d in DOMAINS:
    n_psu, hh_per = (3, 5) if d in small else (6, 8)
    psus = [f"{d}_P{k}" for k in range(1, n_psu + 1)]
    psu_eff = dict(zip(psus, rng.normal(0, 0.15, n_psu)))     # PSU panel across years
    for y in YEARS:
        for p in psus:
            for h in range(hh_per):
                inc = np.exp(BASE + u[d] + year_eff[y]
                             + (special.get(d, 0) if y == 2023 else 0)
                             + psu_eff[p] + rng.normal(0, 0.45))
                rows.append({
                    "year": y, "domain": d, "region": dom_region[d], "psu": p,
                    "strata": dom_region[d],
                    "weight": round(float(rng.uniform(80, 120)), 2),
                    "hhsize": int(min(1 + rng.poisson(1.6), 8)),
                    "income": round(float(inc), 2),
                })

for y in YEARS:   # poverty line: 60% of the person-weighted national median, per year
    yr = [r for r in rows if r["year"] == y]
    vals = np.repeat([r["income"] for r in yr],
                     [int(r["weight"] * r["hhsize"]) for r in yr])
    pl = round(float(np.median(vals)) * 0.6, 2)
    for r in yr:
        r["povline"] = pl

with open(OUTPUT_DIR / "survey_example.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["year", "domain", "region", "psu", "strata",
                                       "weight", "hhsize", "income", "povline"])
    w.writeheader(); w.writerows(rows)

aux = []
for d in DOMAINS:
    for y in YEARS:
        drift = 0.01 * (y - 2022)
        aux.append({
            "domain": d, "year": y, "region": dom_region[d],
            "urban_share":    round(float(np.clip(0.5 + 0.45*u[d] + rng.normal(0, 0.06), 0.05, 0.95)), 4),
            "educ_secondary": round(float(np.clip(0.55 + 0.50*u[d] + drift + rng.normal(0, 0.05), 0.1, 0.95)), 4),
            "unemp_rate":     round(float(np.clip(0.12 - 0.16*u[d]
                                                  - (special.get(d, 0)*0.5 if y == 2023 else 0)
                                                  + rng.normal(0, 0.02), 0.01, 0.40)), 4),
            "mean_schooling": round(float(9.5 + 3.2*u[d] + drift*10 + rng.normal(0, 0.4)), 3),
            "nightlights":    round(float(np.exp(1.2 + 1.5*u[d] + rng.normal(0, 0.25))), 3),
            "pop":            int(rng.uniform(20000, 90000)),
        })
# `region` is built above for readability but deliberately not written: the
# auxiliary file feeds the Fay-Herriot covariate pool, which must be numeric,
# and the region label the benchmarking uses is read from the survey file.
aux_fields = [k for k in aux[0].keys() if k != "region"]
with open(OUTPUT_DIR / "auxiliary_example.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=aux_fields, extrasaction="ignore")
    w.writeheader(); w.writerows(aux)

feats = []
for i, d in enumerate(DOMAINS):   # abstract 8x5 grid; not any real territory
    col, row = i % 8, i // 8
    x0, y0 = col * 1.0, row * 1.0
    poly = [[[x0, y0], [x0+1, y0], [x0+1, y0+1], [x0, y0+1], [x0, y0]]]
    feats.append({"type": "Feature",
                  "properties": {"domain": d, "region": dom_region[d]},
                  "geometry": {"type": "Polygon", "coordinates": poly}})
json.dump({"type": "FeatureCollection", "name": "synthetic_domains", "features": feats},
          open(OUTPUT_DIR / "geometry_example.geojson", "w", encoding="utf-8"))
print("wrote survey_example.csv, auxiliary_example.csv, geometry_example.geojson")
