# Example data: Spain (provinces)

This folder holds the **Spain example dataset** -- the reference example used in
the guidance notes and in earlier published runs of this package. A second,
fully synthetic example lives in `Data/simulated/`; see "Which example to use"
below.

| File | What it is |
|---|---|
| `survey.rds` | 5,148 household records: 52 Spanish provinces x 3 years (2012, 2013, 2014), with income, poverty line, household size, PSU (`ea_id`), household panel identifiers and a 5-way region grouping |
| `auxiliary.rds` | 156 province-year rows (52 provinces x 3 years) with domain-level covariates -- demographic, education, labour-market, night-lights and market-access indicators, plus interaction terms |
| `shapefile.rds` | Province boundaries as an `sf` object: 52 polygons, WGS 84 (EPSG:4326) |

## Suggested variable mapping (Step 2)

| App field | Column |
|---|---|
| year | `year` |
| domain | `prov` |
| psu | `ea_id` |
| weight | `weight` |
| household size | `hhsize` |
| welfare | `income` |
| poverty line (column) | `povline` |
| benchmark level (grouped) | `region` |
| auxiliary / geometry join key | `prov` |

The application's default analysis years -- 2012 and 2013 -- already match this
dataset; 2014 is available as a third round.

`provlab` in `auxiliary.rds` is a province *name*, not a covariate. It is
skipped automatically during variable selection (a note appears in the step
log), so there is no need to remove it.

## Provenance, licence and attribution

The survey and auxiliary data derive from the `incomedata` dataset in the CRAN
package **sae: Small Area Estimation**, by Isabel Molina and Yolanda Marhuenda.
The `sae` authors document `incomedata` as *synthetic* data on income and
related variables for Spanish provinces -- it is not real respondent microdata,
so no disclosure risk attaches to it. The records here have been restructured
into a three-period panel with household panel identifiers and a poverty-line
column.

**`sae` is distributed under GPL-2, so these derived files are distributed
under GPL-2** -- not under the MIT grant that covers this package's own code.
Keep this notice with the files if you pass them on.

> Molina, I. and Marhuenda, Y. *sae: Small Area Estimation.* R package
> version 1.3. https://CRAN.R-project.org/package=sae

The transformation from `incomedata` into `survey.rds` was carried out outside
this repository and the derivation script is not held here, so these files
cannot currently be regenerated from source. That remains an open item for the
reproducibility review in `docs/RELEASE_CHECKLIST.md`.

### Boundary file — IGN/CNIG CartoBase ANE replacement

This candidate replaces the former boundary of unresolved provenance with
**IGN/CNIG CartoBase ANE**, scale **1:3,000,000**, distributed under **CC BY 4.0**.
The source is the 2024 edition from the documented mapSpain CDN mirror, pinned
to commit `4cd2698c3f6783eb1498edfe0224646953abe71c`. It is not a direct CNIG download.

Records were selected to cover the entire **2012–2014** example period.
Mainland/Balearic and Canary Islands layers are combined, transformed from their
declared ETRS89 (EPSG:4258) to WGS84 (EPSG:4326), and joined by INE province code.
The existing 52 `prov` IDs and `provlab` names are preserved, including Ceuta
and Melilla. The Canary Islands remain at their geographic coordinates.
Four invalid source geometries (province IDs 14, 39, 41 and 45) were repaired
with `sf::st_make_valid`; the generator records these repairs in its metadata.
This is simplified cartography for province-level illustration; outlines differ
from the former map and are not intended for cadastral or precise boundary use.

Required attribution for this derived file and its maps:

> Obra derivada de CartoBase ANE 2006-2024 CC-BY 4.0 ign.es

The map is **not covered by GPL-2 or the application's MIT licence**.
Preserve the IGN attribution, [CC BY 4.0 link](https://creativecommons.org/licenses/by/4.0/),
and description of modifications when sharing it. The application's map captions
carry this credit only when the selected geometry includes the attribution metadata.
Do not strip that metadata when adapting this file.

- [Official catalogue](https://centrodedescargas.cnig.es/CentroDescargas/cartobase-ane)
- [IGN licence](IGN_source/IGN_licence.pdf)
- [Source record, checksums and transformations](boundary_provenance.json)
- [Province crosswalk](province_crosswalk.csv)
- [Pinned distribution documentation](https://github.com/rOpenSpain/mapSpain/blob/4cd2698c3f6783eb1498edfe0224646953abe71c/README.md)

The two original mirror GeoPackages are included in `IGN_source/`.
Regenerate offline with `sf`, `digest`, and `jsonlite` installed:

```r
# Run in a terminal from the package root:
# Rscript Data/Spain/generate_spain_boundary.R
```

An optional output-directory argument allows regeneration into a scratch folder.
The script verifies source hashes before writing. Reproducibility across different
R/sf/GDAL/PROJ versions should be checked separately.

Only the boundary has changed; survey and auxiliary files are unchanged.
This resolves the replacement map's source/licence documentation, not the missing
survey derivation or the package's other release approvals.

## Which example to use

- **`Data/simulated/`** -- fully synthetic, generated by an included seeded
  script, no third-party rights attached. Best for training, and for anything
  that will be passed on to others.
- **`Data/Spain/`** (this folder) -- the Spain example. Best for reproducing the
  results shown in the guidance notes and for comparing against earlier runs.

Neither dataset is a substitute for real survey data in a real estimate.

## Obtaining `survey.rds` and `auxiliary.rds`

These two files are **not stored in the Git repository**. They are derived from
the CRAN `sae` package and carry GPL-2 terms, while this repository's code is
MIT licensed; keeping them out avoids publishing a mixed licence in the source
tree. They ship inside the release archive instead.

To use the Spain example after cloning the repository, download the release
archive, open `Data/Spain/` inside it, and copy `survey.rds` and
`auxiliary.rds` into this folder. Everything else the example needs --
the IGN boundary, the province crosswalk, the provenance record and the
boundary generator -- is already here.

A copy of the GNU General Public License version 2, as required when these
files are redistributed, is included with the package at
`LICENSES/GPL-2.0.txt`. Keep it with the data if you pass the files on.
