## =============================================================================
## Package data from the assembled closure history (data-raw/closures/output,
## produced by data-raw/run_all.R, which calls this script at the end).
## Run from the package root: Rscript data-raw/make_data.R
## Written 18 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf))
src <- "data-raw/closures"
gp  <- file.path(src, "output/sessf_closures.gpkg")
if (!file.exists(gp)) stop("run data-raw/run_all.R first (no ", gp, ")")

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

## human review of the boundaries (data-raw/closures/review/review_log.csv):
## the latest row for each version decides its status
rv <- read.csv(file.path(src, "review/review_log.csv"), stringsAsFactors = FALSE, colClasses = "character")
rv <- rv[nzchar(rv$version_id), , drop = FALSE]
bad <- setdiff(rv$version_id, closures$version_id)
if (length(bad)) stop("review_log.csv has unknown version_id: ", paste(bad, collapse = ", "))
rv <- rv[order(rv$version_id, rv$review_date), , drop = FALSE]
rv <- rv[!duplicated(rv$version_id, fromLast = TRUE), , drop = FALSE]
j <- match(closures$version_id, rv$version_id)
closures$human_review <- ifelse(is.na(j), "not reviewed", rv$result[j])
closures$reviewed_by  <- rv$reviewed_by[j]
closures$review_date  <- as.Date(rv$review_date[j])
review_log <- read.csv(file.path(src, "review/review_log.csv"), stringsAsFactors = FALSE)

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

dir.create("data", showWarnings = FALSE)
save(closures, file = "data/closures.rda", compress = "xz")
save(instruments, file = "data/instruments.rda", compress = "xz")
save(legislation, file = "data/legislation.rda", compress = "xz")
save(boundary_pages, file = "data/boundary_pages.rda", compress = "xz")
save(other_measures, file = "data/other_measures.rda", compress = "xz")
save(not_mapped, file = "data/not_mapped.rda", compress = "xz")
save(closure_changes, file = "data/closure_changes.rda", compress = "xz")
save(text_fixes, file = "data/text_fixes.rda", compress = "xz")
save(review_log, file = "data/review_log.rda", compress = "xz")
cat("closures:", nrow(closures), "versions,", length(unique(closures$closure_id)), "closures\n")
