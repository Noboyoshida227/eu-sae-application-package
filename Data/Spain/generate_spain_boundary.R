# Regenerate the Spain example boundary from pinned, bundled IGN/CNIG inputs.
# Usage: Rscript Data/Spain/generate_spain_boundary.R [output-directory]
# Requires sf, digest and jsonlite; no download or package installation occurs.
args <- commandArgs(trailingOnly = TRUE)
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United States.utf8")
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Run this file with Rscript.")
input_dir <- dirname(normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE))
output_dir <- if (length(args)) args[[1L]] else input_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (pkg in c("sf", "digest", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Required package not installed: ", pkg)
}
source_files <- c("se89_3_admin_prov_a_x.gpkg", "se89_3_admin_prov_a_y.gpkg")
expected_hash <- c(
  "a9dd54dcb4778d99caacc85891dba54a359c636fe5548b4f7a8490ac57cd2117",
  "796c8e24ee06361d267cb9641a741f12773dce2d28f1c1e3065bc63b7d18d8aa"
)
source_paths <- file.path(input_dir, "IGN_source", source_files)
actual_hash <- vapply(source_paths, function(f) digest::digest(file = f, algo = "sha256"), character(1))
if (!identical(unname(actual_hash), expected_hash)) stop("IGN source hash mismatch; do not regenerate from unverified inputs.")
raw <- do.call(rbind, lapply(source_paths, function(f) sf::st_read(f, quiet = TRUE)))
start_date <- as.Date("2012-01-01")
end_date <- as.Date("2014-12-31")
valid_from <- as.Date(raw$fecha_alta)
valid_to <- as.Date(raw$fecha_baja)
selected <- raw[valid_from <= start_date & (is.na(valid_to) | valid_to > end_date), ]
selected$prov <- as.integer(selected$id_prov)
selected <- selected[order(selected$prov), ]
stopifnot(nrow(selected) == 52L, identical(selected$prov, 1:52), !anyDuplicated(selected$id_prov))
# The package uses INE province codes but retains sae's familiar historical labels.
survey <- readRDS(file.path(input_dir, "survey.rds"))
auxiliary <- readRDS(file.path(input_dir, "auxiliary.rds"))
labels <- unique(as.data.frame(auxiliary)[, c("prov", "provlab")])
labels <- labels[order(as.integer(labels$prov)), ]
stopifnot(nrow(labels) == 52L, identical(as.integer(labels$prov), 1:52),
          setequal(auxiliary$prov, selected$prov), setequal(survey$prov, selected$prov))
selected$provlab <- as.character(labels$provlab[match(selected$prov, labels$prov)])
stopifnot(!anyNA(selected$provlab), !anyDuplicated(selected$provlab))
crosswalk <- data.frame(prov = selected$prov, provlab = selected$provlab,
                        ign_id_prov = selected$id_prov, ign_name = selected$rotulo,
                        valid_from = as.character(selected$fecha_alta),
                        valid_to = as.character(selected$fecha_baja))
# Both mirror layers declare ETRS89 (EPSG:4258); use that declared CRS and transform,
# not relabel. The Canaries remain at their geographic coordinates.
stopifnot(sf::st_crs(selected)$epsg == 4258L)
boundary <- sf::st_sf(prov = selected$prov, provlab = selected$provlab,
                      geometry = sf::st_transform(sf::st_geometry(selected), 4326))
invalid_before <- which(!sf::st_is_valid(boundary))
if (length(invalid_before)) sf::st_geometry(boundary)[invalid_before] <- sf::st_make_valid(sf::st_geometry(boundary)[invalid_before])
sf::st_geometry(boundary) <- sf::st_cast(sf::st_geometry(boundary), "MULTIPOLYGON")
stopifnot(nrow(boundary) == 52L, all(sf::st_is_valid(boundary)), !any(sf::st_is_empty(boundary)),
          all(as.numeric(sf::st_area(boundary)) > 0))
bbox <- sf::st_bbox(boundary)
stopifnot(bbox[["xmin"]] < -18, bbox[["ymin"]] < 28, bbox[["xmax"]] > 4,
          bbox[["ymax"]] > 43, sf::st_crs(boundary)$epsg == 4326L)
attribution <- "Obra derivada de CartoBase ANE 2006-2024 CC-BY 4.0 ign.es"
commit <- "4cd2698c3f6783eb1498edfe0224646953abe71c"
provenance <- list(
  provider = "Instituto Geografico Nacional (IGN), Spain",
  product = "CartoBase ANE", scale = "1:3,000,000",
  source_edition = "2024; mapSpain mirror README last update 2024-08-02",
  valid_period = c("2012-01-01", "2014-12-31"),
  licence = "CC-BY-4.0", licence_url = "https://creativecommons.org/licenses/by/4.0/",
  provider_licence_url = "https://www.ign.es/resources/licencia/Condiciones_licenciaUso_IGN.pdf",
  catalogue_url = "https://centrodedescargas.cnig.es/CentroDescargas/cartobase-ane",
  distribution = "Pinned mapSpain CDN mirror of IGN CartoBase ANE, not a direct CNIG download",
  mirror_commit = commit,
  source_files = source_files, source_sha256 = expected_hash,
  source_urls = paste0("https://raw.githubusercontent.com/rOpenSpain/mapSpain/", commit, "/dist/", source_files),
  attribution = attribution,
  modifications = c("Selected records valid throughout 2012-2014", "Combined mainland/Balearic and Canary layers",
                    "Mapped INE id_prov to existing prov and provlab labels",
                    "Transformed declared EPSG:4258 to EPSG:4326",
                    "Retained Canaries in geographic position; no further simplification"),
  repaired_province_ids = boundary$prov[invalid_before]
)
attr(boundary, "boundary_attribution") <- attribution
attr(boundary, "boundary_provenance") <- provenance
saveRDS(boundary, file.path(output_dir, "shapefile.rds"), version = 3, compress = "xz")
utils::write.csv(crosswalk, file.path(output_dir, "province_crosswalk.csv"), row.names = FALSE, fileEncoding = "UTF-8")
provenance$output_sha256 <- digest::digest(file = file.path(output_dir, "shapefile.rds"), algo = "sha256")
provenance$features <- nrow(boundary)
provenance$coordinate_rows <- nrow(sf::st_coordinates(boundary))
jsonlite::write_json(provenance, file.path(output_dir, "boundary_provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Created", file.path(output_dir, "shapefile.rds"), "\n",
    nrow(boundary), "valid provinces;", provenance$coordinate_rows, "coordinate rows\n",
    "SHA-256:", provenance$output_sha256, "\n", attribution, "\n")
