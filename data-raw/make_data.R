## =============================================================================
## Build the package data from the closures review outputs
## (C:/_AI/Claude_code/SESSF_zones/closures, run its pipeline first:
##  19_meta.R -> 18_build_all.ps1 -> 22_pdf_pages.R -> 20_assemble_all.R).
## Run from the package root: Rscript data-raw/make_data.R
## Written 18 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf))
src <- Sys.getenv("SESSF_CLOSURES_DIR", "C:/_AI/Claude_code/SESSF_zones/closures")
gp  <- file.path(src, "output/sessf_closures.gpkg")
stopifnot(file.exists(gp))

## the full GeoPackage (vertices and AFMA layers are read from it on demand)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
file.copy(gp, "inst/extdata/sessf_closures.gpkg", overwrite = TRUE)

## the authorised PDFs
dir.create("inst/legislation", showWarnings = FALSE)
pdfs <- list.files(file.path(src, "legislation"), pattern = "\\.pdf$", full.names = TRUE)
file.copy(pdfs, "inst/legislation", overwrite = TRUE)

rd <- function(layer) st_read(gp, layer = layer, quiet = TRUE)
closures <- rd("closures")
closures$valid_from <- as.Date(closures$valid_from); closures$valid_to <- as.Date(closures$valid_to)
closures$pdf_file <- basename(closures$pdf_file)              # file name in inst/legislation
closures <- closures[order(closures$closure_id, closures$version), ]
row.names(closures) <- NULL

instruments <- rd("instruments")
instruments$start <- as.Date(instruments$start); instruments$end <- as.Date(instruments$end)
instruments$register_end <- as.Date(instruments$register_end)
legislation <- rd("legislation_pdfs"); legislation$file <- basename(legislation$file)
boundary_pages <- rd("boundary_pages"); boundary_pages$file <- basename(boundary_pages$file)
other_measures <- rd("other_measures")
other_measures$start <- as.Date(other_measures$start); other_measures$end <- as.Date(other_measures$end)
not_mapped <- rd("not_mapped")
closure_changes <- rd("changes"); closure_changes$date <- as.Date(closure_changes$date)
text_fixes <- read.csv(file.path(src, "data/text_fixes.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")

save(closures, file = "data/closures.rda", compress = "xz")
save(instruments, file = "data/instruments.rda", compress = "xz")
save(legislation, file = "data/legislation.rda", compress = "xz")
save(boundary_pages, file = "data/boundary_pages.rda", compress = "xz")
save(other_measures, file = "data/other_measures.rda", compress = "xz")
save(not_mapped, file = "data/not_mapped.rda", compress = "xz")
save(closure_changes, file = "data/closure_changes.rda", compress = "xz")
save(text_fixes, file = "data/text_fixes.rda", compress = "xz")
cat("closures:", nrow(closures), "versions,", length(unique(closures$closure_id)), "closures\n")
