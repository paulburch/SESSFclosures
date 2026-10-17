## =============================================================================
## Rebuild the SESSFclosures data from the legislation.
##
## Run from the package root:   Rscript data-raw/run_all.R
## Options (environment variables):
##   REFRESH_CATALOGUE=1   re-query the Federal Register for closures
##                         instruments (otherwise data-raw/closures/catalogue/ is used)
##
## Steps (scripts in data-raw/closures/R, working directory data-raw/):
##   14 catalogue of instruments (optional refresh)  15 fetch missing texts
##   03 coastline / coastal waters / AFZ features     19 dates in force and rules
##   04 parse every instrument                        downloads: AFMA, CAPAD
##   12, 12b compare with AFMA's polygons             21 PDFs, 22 PDF pages
##   20 assemble the history (GeoPackage)             make_data.R package data
##
## Needs: sf, jsonlite, xml2, pdftools, ozmaps, rnaturalearth (+ rnaturalearthdata,
## rnaturalearthhires for scale 10). Internet only for texts, PDFs or downloads
## not already present.
## Written 18 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
if (!file.exists("DESCRIPTION") || !dir.exists("data-raw/closures")) stop("run from the package root")
pkg_root <- getwd()
setwd("data-raw")
on.exit(setwd(pkg_root), add = TRUE)
dir.create("closures/output", showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
step <- function(script, ...) {
  cat("\n==>", script, ..., "\n")
  st <- system2(rscript, c(file.path("closures/R", script), ...))
  if (st != 0) stop(script, " failed")
}

## 1 catalogue and texts
if (Sys.getenv("REFRESH_CATALOGUE") == "1") step("14_catalogue.R")
step("15_fetch_all.R")

## 2 features (coastline, coastal waters, n nm lines, AFZ)
if (!file.exists("closures/output/features.rds")) step("03_features.R")

## 3 dates in force and operative rules
step("19_meta.R")

## 4 parse every instrument and compilation carrying closures
cat_df <- read.csv("closures/catalogue/instrument_catalogue.csv", stringsAsFactors = FALSE)
vers   <- read.csv("closures/catalogue/instrument_versions.csv", stringsAsFactors = FALSE)
skip <- c("F2011L02125", "F2012L00473", "F2016L00842", "F2017L00515", "F2019L00650", "F2019L01200",
          "F2019L01609", "F2018L01077", "F2022L00116", "F2023L00439", "F2024L00546", "F2024L01344", "F2025L00736")
keys <- c(setdiff(cat_df$id, skip), vers$registerId[grepl("C[0-9]{5}$", vers$registerId)])
for (k in unique(keys)) {
  st <- system2(rscript, c("closures/R/04_build_version.R", k), stdout = FALSE, stderr = FALSE)
  cat(k, if (st == 0) "ok" else "FAILED", "\n")
}

## 5 downloads: AFMA shapefiles (2016 direction per schedule; 2021 direction bundle), CAPAD
dl <- function(url, dest) if (!file.exists(dest)) utils::download.file(url, dest, mode = "wb", quiet = TRUE)
if (!dir.exists("closures/afma_2024/unzipped")) {
  dir.create("closures/afma_2024", showWarnings = FALSE)
  dl("https://www.afma.gov.au/sites/default/files/2024-05/sessf_schedule_direction_shapefiles.zip",
     "closures/afma_2024/sessf_schedule_direction_shapefiles.zip")
  utils::unzip("closures/afma_2024/sessf_schedule_direction_shapefiles.zip", exdir = "closures/afma_2024/unzipped")
}
if (!dir.exists("closures/afma_2016/unzipped")) {
  dir.create("closures/afma_2016/zips", recursive = TRUE, showWarnings = FALSE)
  urls <- readLines("closures/afma_2016_urls.txt")
  for (i in seq_along(urls)) {
    z <- sprintf("closures/afma_2016/zips/S%02d.zip", i)
    dl(urls[i], z)
    utils::unzip(z, exdir = sprintf("closures/afma_2016/unzipped/S%02d", i))
  }
}
if (!file.exists("closures/capad/capad2022_marine_selected.geojson")) {
  dir.create("closures/capad", showWarnings = FALSE)
  dl(paste0("https://gis.environment.gov.au/gispubmap/rest/services/ogc_services/CAPAD_Marine/MapServer/0/query?",
            "where=NAME+IN+(%27Kent+Group%27,%27Cape+Howe%27,%27Discovery+Bay%27,%27Twelve+Apostles%27)",
            "&outFields=PA_ID,NAME,TYPE,STATE,ZONE_TYPE,GAZ_DATE,LATEST_GAZ,GAZ_AREA,GIS_AREA,AUTHORITY,COMMENTS",
            "&outSR=4326&f=geojson"), "closures/capad/capad2022_marine_selected.geojson")
}

## 6 agreement with AFMA's polygons
step("12_compare_afma.R")
step("12b_compare_afma_2016.R")

## 7 authorised PDFs (copied from the installed package folder when present) and their pages
dir.create("closures/legislation", showWarnings = FALSE)
have <- list.files(file.path(pkg_root, "inst/legislation"), pattern = "\\.pdf$", full.names = TRUE)
file.copy(have[!file.exists(file.path("closures/legislation", basename(have)))], "closures/legislation")
step("21_fetch_pdfs.R")
step("22_pdf_pages.R")

## 8 assemble the history, then the package data
step("20_assemble_all.R")
setwd(pkg_root)
cat("\n==> make_data.R\n")
source("data-raw/make_data.R", local = new.env())
