## =============================================================================
## Closures review, step 4: build the closure polygons of one instrument version
## Usage: Rscript closures/R/04_build_version.R comp5_2025
## Output: closures/output/build_<key>.rds (sf of areas) and check_<key>.csv
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
if (!exists("key")) { args <- commandArgs(TRUE); key <- if (length(args)) args[1] else "comp5_2025" }
source("closures/R/fun_closures.R"); source("closures/R/fun_parse.R")
if (!exists("feat")) feat <- readRDS("closures/output/features.rds")
tx_all <- read_instrument(key)
sch <- split_schedules(tx_all)
## short instruments describe their areas without schedule headings: take the
## text from the first "The area of" as one pseudo-schedule (number 0)
if (!length(sch)) {
  body <- paste(vapply(tx_all, clean_page, ""), collapse = "\n")
  i <- regexpr("The area of", body)
  if (i > 0) sch <- list(list(number = 0L, title = "(no schedule)", page = 1, text = substr(body, i, nchar(body))))
}
if (!length(sch)) { cat("no closure areas in", key, "\n"); saveRDS(list(areas = NULL, vertices = NULL), file.path("closures/output", paste0("build_", key, ".rds"))); quit(save = "no") }

rows <- list(); geoms <- list(); verts <- list(); excls <- list()
for (s in sch) {
  areas <- split_areas(s$text)
  for (a in seq_along(areas)) {
    ar <- areas[[a]]
    sw <- parse_seaward_points(ar$text)
    ## (only when islands are named: Head of the Bight 2005-07 says "2 nm
    ## seaward" inside a bounded area)
    if (!is.null(sw) && nrow(sw) && (!all(is.na(sw$name)) || !grepl("commencing|beginning at", ar$text, ignore.case = TRUE))) {
      ## "a line N nautical miles seaward of" listed islands: one circle each
      for (i in seq_len(nrow(sw))) {
        geoms[[length(geoms) + 1]] <- circle_geom(sw$lat[i], sw$lon[i], sw$r_nm[i])
        rows[[length(rows) + 1]] <- data.frame(schedule = s$number, schedule_title = s$title, afz_only = FALSE,
          area = paste0(ar$name, ": ", sw$name[i]), page = s$page, n_points = 1,
          method = "seaward_of_point", features = "", quality = "approx_island_as_point",
          notes = sprintf("%g nm from the island's 'near' position; the island's own coastline is ignored", sw$r_nm[i]))
        verts[[length(verts) + 1]] <- data.frame(schedule = s$number, area = rows[[length(rows)]]$area,
          vertex = 1, lat = sw$lat[i], lon = sw$lon[i], role = "centre", text = NA)
      }
      next
    }
    pa <- parse_area(ar$text)
    b <- build_area(pa, feat)
    g <- b$geom
    for (cc in pa$circles) {
      cg <- circle_geom(cc$lat, cc$lon, cc$r_nm)
      g <- if (is.null(g)) cg else st_union(g, cg)
    }
    for (ex in pa$excl) excls[[length(excls) + 1]] <- c(list(owner = s$number), ex)
    ## a circle-only area ("circle of radius 15 nm centred at ...") has no boundary steps but is exact
    if (length(pa$circles)) b$notes <- setdiff(b$notes, "no boundary steps")
    q <- if (is.null(g)) "FAILED" else if (length(b$feats) || length(b$notes)) "approx_feature" else "exact"
    geoms[[length(geoms) + 1]] <- if (is.null(g)) st_sfc(st_polygon(), crs = 4326) else g
    intro <- tolower(gsub("\\s+", " ", substr(ar$text, 1, 400)))
    afz_only <- grepl("part of the afz", intro) && !grepl("part of the afz (and|and/or) (part of )?(the )?coastal waters", intro)
    rows[[length(rows) + 1]] <- data.frame(schedule = s$number, schedule_title = s$title, afz_only = afz_only,
      area = ifelse(is.na(ar$name), s$title, ar$name), page = s$page,
      n_points = length(pa$steps), method = if (length(pa$circles)) "boundary+circle" else "boundary",
      features = paste(b$feats, collapse = ","), quality = q, notes = paste(unique(b$notes), collapse = " | "))
    if (!is.null(b$pts)) verts[[length(verts) + 1]] <- data.frame(schedule = s$number,
      area = rows[[length(rows)]]$area, vertex = seq_len(nrow(b$pts)), lat = b$pts[, 2], lon = b$pts[, 1],
      role = "boundary", text = vapply(pa$steps, function(z) substr(z$text, 1, 120), ""))
  }
}
res <- st_sf(do.call(rbind, rows), geometry = do.call(c, geoms))

## exclusions: a circle, or another schedule's area, cut out of the closure
for (ex in excls) {
  i <- which(res$schedule == ex$owner)
  cut <- NULL; lab <- ""
  if (identical(ex$kind, "circle")) { cut <- circle_geom(ex$lat, ex$lon, ex$r_nm); lab <- sprintf("circle r=%g nm", ex$r_nm) }
  if (identical(ex$kind, "schedule")) { cut <- st_union(st_geometry(res[res$schedule == ex$schedule, ])); lab <- paste("Schedule", ex$schedule) }
  if (!is.null(cut) && length(i)) for (k in i) {
    st_geometry(res)[k] <- st_union(st_difference(st_geometry(res)[k], cut))
    res$notes[k] <- paste0(res$notes[k], ifelse(nzchar(res$notes[k]), " | ", ""), "excludes ", lab)
  }
}
## land is never part of a closure; a closure described as "that part of the
## AFZ" (and not "and coastal waters") also excludes coastal waters, which the
## AFZ does not include
land_u <- st_union(feat$land); cw_u <- st_union(feat$coastal_poly)
g_all <- st_make_valid(st_geometry(res))
res$clip <- ifelse(res$afz_only, "AFZ (outside coastal waters)", "land removed")
st_geometry(res) <- do.call(c, lapply(seq_along(g_all), function(i) suppressMessages(suppressWarnings({
  d <- st_difference(g_all[i], if (isTRUE(res$afz_only[i]) && res$method[i] != "seaward_of_point") cw_u else land_u)
  if (length(d)) st_union(st_collection_extract(d, "POLYGON")) else st_sfc(st_polygon(), crs = 4326)
}))))
res$area_km2 <- round(as.numeric(st_area(st_transform(res, 3577))) / 1e6, 1)
saveRDS(list(areas = res, vertices = do.call(rbind, verts)),
        file.path("closures/output", paste0("build_", key, ".rds")))
chk <- st_drop_geometry(res)
write.csv(chk, file.path("closures/output", paste0("check_", key, ".csv")), row.names = FALSE)
print(chk[, c("schedule", "area", "n_points", "features", "quality", "area_km2")], row.names = FALSE, right = FALSE)
cat("\nnotes:\n")
for (i in which(nzchar(chk$notes))) cat(sprintf(" S%d %s: %s\n", chk$schedule[i], substr(chk$area[i], 1, 40), chk$notes[i]))
