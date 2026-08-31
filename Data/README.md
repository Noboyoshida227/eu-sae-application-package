# Input data

These examples are for learning and testing, not for producing real estimates.

| Folder | Purpose and limitations |
|---|---|
| [Spain](Spain/README.md) | Spain province example used in the guidance. The survey and auxiliary files carry their documented GPL-2 terms. The replacement province map is IGN/CNIG CartoBase ANE under CC BY 4.0, with attribution and an offline regeneration script. The survey derivation script is not held. |
| [simulated](simulated/README.md) | Fully synthetic survey, auxiliary covariates, and abstract geometry. The included seeded Python generator reproduces these files; see its README for mapping instructions. |

For your own analysis, create a clearly named folder such as `Data/Romania_2025/`
and group your approved survey, auxiliary, and geometry inputs there. Browse
can also select files elsewhere on your computer. Generated results belong in
`outputs/`, not in your input folder. User-created Data folders are excluded
from the release builder's explicit file inventory.

## Existing setups

Package-relative references beginning with `data/` or `example_data/` are
resolved to the corresponding file under `Data/Spain/` or `Data/simulated/`
only when that file exists. A message records this path migration. Bare saved
filenames resolve only if unambiguous in Data or the two example folders.
Absolute paths and missing user inputs are never replaced with example data.
If an old absolute path or temporary upload is unavailable, Browse to the
intended file and save the setup again. Saved copies under
`app_runs/_last_setup_files/` retain their existing behavior.

Standalone UFH/MFH/Comparison scripts with no input path configured use the
Spain example files. An explicitly configured missing path causes an error.
