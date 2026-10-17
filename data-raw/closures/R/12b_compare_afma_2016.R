## =============================================================================
## Closures review, step 12b: compare our polygons of the 2016 direction
## (F2016L00549) with AFMA's per-schedule shapefiles of that direction
## (afma.gov.au "Maps, directions, closures and fishing areas" page, Dropbox
## links dated 21 June 2016; S13 and S34 there are the April 2023 files).
## Downloaded 17 Oct 2026 to closures/afma_2016/.
## Output: closures/output/afma_comparison_2016.csv
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf)); sf_use_s2(FALSE)
fs <- list.files("closures/afma_2016/unzipped", pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
land <- st_union(readRDS("closures/output/features.rds")$land)
ours <- readRDS("closures/output/build_F2016L00549.rds")$areas
km2 <- function(g) if (!length(g) || all(st_is_empty(g))) 0 else sum(as.numeric(st_area(st_transform(g, 3577)))) / 1e6
as_poly <- function(x) {
  g <- st_zm(st_geometry(x)); if (is.na(st_crs(g))) st_crs(g) <- 4283
  g <- st_make_valid(st_transform(g, 4326))
  t <- unique(as.character(st_geometry_type(g)))
  if (all(t %in% c("LINESTRING", "MULTILINESTRING"))) g <- st_polygonize(st_union(g))
  st_union(suppressWarnings(st_collection_extract(st_make_valid(st_union(g)), "POLYGON")))
}
res <- list(); pairs <- list()
for (f in fs) {
  s <- as.integer(sub("^S([0-9]+).*", "\\1", basename(dirname(dirname(f)))))
  a <- tryCatch(as_poly(st_read(f, quiet = TRUE)), error = function(e) NULL)
  if (is.null(a) || st_is_empty(a)) { cat("could not read", basename(f), "\n"); next }
  a_sea <- suppressMessages(suppressWarnings(st_difference(a, land)))
  g <- st_union(st_geometry(ours[ours$schedule == s, ]))
  if (!length(g) || st_is_empty(g)) { res[[length(res) + 1]] <- data.frame(schedule = s, afma_file = basename(f),
      quality = NA, km2_ours = 0, km2_afma = round(km2(a_sea), 1), iou = NA, sym_diff_km2 = NA, max_offset_km = NA); next }
  it <- suppressMessages(suppressWarnings(st_intersection(g, a_sea)))
  sd <- suppressMessages(suppressWarnings(st_sym_difference(g, a_sea)))
  bg <- st_cast(st_boundary(st_transform(g, 3577)), "MULTILINESTRING")
  ba <- st_cast(st_boundary(st_transform(a_sea, 3577)), "MULTILINESTRING")
  pg <- st_cast(st_line_sample(st_cast(bg, "LINESTRING"), density = 1 / 500), "POINT")
  res[[length(res) + 1]] <- data.frame(schedule = s, afma_file = basename(f),
    quality = paste(unique(ours$quality[ours$schedule == s]), collapse = ","),
    km2_ours = round(km2(g), 1), km2_afma = round(km2(a_sea), 1),
    iou = round(km2(it) / (km2(g) + km2(a_sea) - km2(it)), 4), sym_diff_km2 = round(km2(sd), 2),
    max_offset_km = if (length(pg)) round(as.numeric(max(st_distance(pg, ba))) / 1000, 2) else NA)
  pairs[[as.character(s)]] <- list(ours = g, afma = a_sea)
}
out <- do.call(rbind, res); out <- out[order(out$schedule), ]
out$note <- ifelse(out$schedule %in% c(13, 34), "AFMA file is the April 2023 version (2021 direction)", "")
write.csv(out, "closures/output/afma_comparison_2016.csv", row.names = FALSE)
saveRDS(pairs, "closures/output/afma_pairs_2016.rds")
print(out[, -2], row.names = FALSE)
