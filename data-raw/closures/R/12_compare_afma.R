## =============================================================================
## Closures review, step 12: compare our polygons with AFMA's published
## shapefiles of the 2021 direction's schedules (bundle dated 16 May 2024,
## afma.gov.au/sites/default/files/2024-05/sessf_schedule_direction_shapefiles.zip,
## downloaded 17 Oct 2026 to closures/afma_2024/).
## Output: closures/output/afma_comparison.csv
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf)); sf_use_s2(FALSE)
dir <- "closures/afma_2024/unzipped/GDA Extracted"
fs <- list.files(dir, pattern = "\\.shp$", full.names = TRUE)
sch_of <- function(f) {
  b <- basename(f); n <- suppressWarnings(as.integer(sub("^S(chedule )?\\s*0?([0-9]+).*", "\\2", b)))
  if (!is.na(n)) return(n)
  if (grepl("babel", b, TRUE)) return(43L); if (grepl("conran", b, TRUE)) return(41L)
  if (grepl("flounder", b, TRUE)) return(42L); if (grepl("gabo", b, TRUE)) return(40L)
  if (grepl("tasman", b, TRUE)) return(44L); if (grepl("deepwater closure shapefile", b, TRUE)) return(13L)
  NA_integer_
}
land <- readRDS("closures/output/features.rds")$land
ours25 <- readRDS("closures/output/build_F2025C00776.rds")$areas   # Compilation No. 5 (2025)
ours23 <- readRDS("closures/output/build_F2023C00427.rds")$areas   # Compilation No. 2 (2023)
km2 <- function(g) if (!length(g) || all(st_is_empty(g))) 0 else sum(as.numeric(st_area(st_transform(g, 3577)))) / 1e6
as_poly <- function(x) {
  g <- st_zm(st_geometry(x)); g <- st_make_valid(st_transform(g, 4326))
  t <- unique(as.character(st_geometry_type(g)))
  if (all(t %in% c("LINESTRING", "MULTILINESTRING"))) g <- st_polygonize(st_union(g))
  g <- suppressWarnings(st_collection_extract(st_make_valid(st_union(g)), "POLYGON"))
  st_union(g)
}
res <- list(); pairs <- list()
for (f in fs) {
  s <- sch_of(f)
  a <- tryCatch(as_poly(st_read(f, quiet = TRUE)), error = function(e) NULL)
  if (is.null(a)) { cat("could not read", basename(f), "\n"); next }
  a_sea <- suppressMessages(suppressWarnings(st_difference(a, st_union(land))))
  for (ver in c("2025", if (s %in% 42:44) "2023")) {
    o <- if (ver == "2025") ours25 else ours23
    g <- st_union(st_geometry(o[o$schedule == s, ]))
    if (!length(g) || st_is_empty(g)) next
    it <- suppressMessages(suppressWarnings(st_intersection(g, a_sea)))
    sd <- suppressMessages(suppressWarnings(st_sym_difference(g, a_sea)))
    ## largest boundary offset: furthest point of either boundary from the other
    bg <- st_cast(st_boundary(st_transform(g, 3577)), "MULTILINESTRING")
    ba <- st_cast(st_boundary(st_transform(a_sea, 3577)), "MULTILINESTRING")
    pg <- st_cast(st_line_sample(st_cast(bg, "LINESTRING"), density = 1 / 500), "POINT")
    off_ours <- if (length(pg)) as.numeric(max(st_distance(pg, ba))) / 1000 else NA
    res[[length(res) + 1]] <- data.frame(schedule = s, our_version = ver, afma_file = basename(f),
      quality = paste(unique(o$quality[o$schedule == s]), collapse = ","),
      km2_ours = round(km2(g), 1), km2_afma = round(km2(a_sea), 1),
      iou = round(km2(it) / (km2(g) + km2(a_sea) - km2(it)), 4), sym_diff_km2 = round(km2(sd), 2),
      max_offset_km = round(off_ours, 2))
    if (ver == "2025") pairs[[as.character(s)]] <- list(ours = g, afma = a_sea)
  }
}
out <- do.call(rbind, res); out <- out[order(out$schedule, out$our_version), ]
write.csv(out, "closures/output/afma_comparison.csv", row.names = FALSE)
print(out[, -3], row.names = FALSE)
saveRDS(pairs, "closures/output/afma_pairs.rds")
