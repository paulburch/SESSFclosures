## =============================================================================
## Closures review, step 3: the boundary features some closures follow
##
## Builds, once, the lines that closure boundaries run along but that the
## instruments do not give as coordinates. All are APPROXIMATIONS, and every
## closure that uses one is flagged:
##   coast            mean low water line  ~ ozmaps (ABS) state outlines
##   nm_<N>           N nm seaward of the low water line ~ buffer of the land
##   coastal_waters   3 nm from the territorial sea baseline ~ 3 nm buffer of
##                    the land with bays and gulf mouths closed (a morphological
##                    closing of 60 km, per landmass, stands in for the
##                    straight baselines; it closes Spencer Gulf and Gulf St
##                    Vincent, which lie inside South Australian waters)
##   AFZ              outer limit of the AFZ ~ 200 nm buffer of the land
##                    (mainland, Tasmania, islands, Lord Howe Island)
## Saved as closures/output/features.rds (rings densified, lon/lat).
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
NM <- 1852
alb <- 3577                                         # GDA94 / Australian Albers, metres

land <- st_union(st_make_valid(st_transform(st_as_sf(ozmaps::ozmap_states), 4326)))
## Lord Howe Island is not in ozmaps; take it from Natural Earth (coarse)
ne <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
ne <- st_make_valid(st_transform(ne[ne$admin == "Australia", ], 4326))
lhi <- suppressWarnings(st_intersection(st_geometry(ne),
         st_as_sfc(st_bbox(c(xmin = 158.5, xmax = 159.6, ymin = -32, ymax = -31.2), crs = 4326))))
land <- st_union(c(land, lhi))
land_a <- st_transform(land, alb)

## drop slivers: keep polygons > 0.05 km2 for the ring sets
rings_of <- function(g, min_km2 = 0.05) {
  p <- st_cast(st_make_valid(g), "POLYGON")
  p <- p[as.numeric(st_area(p)) > min_km2 * 1e6]
  r <- st_cast(st_boundary(p), "LINESTRING")       # exterior and holes
  st_transform(st_segmentize(r, 500), 4326)         # vertices every 500 m
}
buf <- function(d) st_union(st_buffer(land_a, d))

feat <- list()
feat$coast <- rings_of(land_a, 0.5)
for (n in c(1, 2, 4, 11)) feat[[paste0("nm_", n)]] <- rings_of(buf(n * NM), 5)
## close bay and gulf mouths of EACH landmass separately, so that islands are
## not bridged to each other (Kangaroo Island to Fleurieu, the Bass Strait
## island chain)
lp <- st_cast(st_make_valid(land_a), "POLYGON")
lp <- lp[as.numeric(st_area(lp)) > 5e6]
closed <- st_union(do.call(c, lapply(seq_along(lp), function(i)
  st_buffer(st_buffer(lp[i], 60000), -60000))))
cw_poly <- st_union(st_buffer(closed, 3 * NM))
feat$coastal_waters <- rings_of(cw_poly, 5)
## the area inside the outer limit of coastal waters (land included): what
## "that part of the AFZ" excludes
feat$coastal_poly <- st_transform(cw_poly, 4326)
feat$AFZ <- rings_of(st_buffer(st_simplify(land_a, dTolerance = 2000), 200 * NM), 1000)
feat$land <- st_transform(land_a, 4326)
attr(feat, "note") <- "approximations; see header of closures/R/03_features.R"
saveRDS(feat, "closures/output/features.rds")
cat(vapply(names(feat), function(k) sprintf("%s: %d ring(s)", k, length(feat[[k]])), ""), sep = "\n")
