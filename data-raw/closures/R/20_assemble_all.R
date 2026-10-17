## =============================================================================
## Closures review, step 20: assemble the closure history from every SESSF
## closures instrument on the Federal Register (May 2005 to now).
##
## Snapshots: on every date an instrument (or compilation) starts or stops,
## the closure areas of all instruments then in effect. Areas in consecutive
## snapshots are linked (same source text, else name, else > 50% overlap); an
## unchanged boundary (< 0.5 km2 difference) continues the same version row.
## Instruments with no closure polygons (species bans over named waters,
## catch triggers) go to a separate table.
##
## Output: closures/output/sessf_closures.gpkg  (closures, vertices,
##   instruments, other_measures, afma_2024) and CSV copies.
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf)); sf_use_s2(FALSE)
TODAY <- as.Date("2026-10-17")
meta  <- read.csv("closures/output/source_meta.csv", stringsAsFactors = FALSE)
meta$start <- as.Date(meta$start); meta$end <- as.Date(meta$end)
meta$end[!is.na(meta$end) & meta$end > TODAY] <- NA
rules <- read.csv("closures/output/source_rules.csv", stringsAsFactors = FALSE)
bp    <- read.csv("closures/output/boundary_pages.csv", stringsAsFactors = FALSE)   # step 22

## Closures defined only by reference to State law (no coordinates in the
## instrument): the 2005 "Victorian Marine Park Closures" (Cape Howe, Discovery
## Bay and Twelve Apostles Marine National Parks, National Parks Act 1975 (Vic)
## Sch 7 Pt 2) and the 2007-2013 "Kent Group National Park" (Nature Conservation
## Act 2002 (Tas), S.R. 2004 No. 143). Their polygons are taken from CAPAD 2022
## Marine (DCCEEW map service, downloaded 18 Oct 2026, closures/capad/): the
## boundaries as at 30 June 2022, which may differ from those at the time.
capad <- st_read("closures/capad/capad2022_marine_selected.geojson", quiet = TRUE)
land_u <- st_union(readRDS("closures/output/features.rds")$land)
capad_poly <- function(nm) {
  g <- st_union(st_make_valid(st_geometry(capad[capad$NAME == nm, ])))
  g2 <- suppressMessages(suppressWarnings(st_difference(g, land_u)))   # land removed, as for every closure
  if (length(g2) && !all(st_is_empty(g2))) g2 else g
}
fill_from_capad <- function(a) {
  e <- which(st_is_empty(a))
  if (!length(e)) return(a)
  add <- list(); drop <- integer()
  for (i in e) {
    parks <- if (grepl("victorian marine park", a$area[i], TRUE)) c("Cape Howe", "Discovery Bay", "Twelve Apostles") else
             if (grepl("kent group", a$area[i], TRUE)) "Kent Group" else character()
    if (!length(parks)) next
    drop <- c(drop, i)
    for (p in parks) {
      r <- a[i, ]; g <- capad_poly(p)
      st_geometry(r) <- st_sfc(g, crs = st_crs(a))
      r$area <- if (length(parks) > 1) paste0(a$area[i], ": ", p, " Marine National Park") else "Kent Group National Park (marine area)"
      r$quality <- "capad_2022"; r$features <- "CAPAD 2022 marine polygon"
      r$notes <- sprintf("No coordinates in the instrument (defined by State law). Polygon: CAPAD 2022 Marine, '%s' (%s), boundary as at 30 June 2022 - may differ from the boundary when this instrument applied.",
                         p, paste(unique(capad$TYPE[capad$NAME == p]), collapse = "/"))
      r$area_km2 <- as.numeric(st_area(st_transform(g, 3577))) / 1e6
      add[[length(add) + 1]] <- r
    }
  }
  if (!length(drop)) return(a)
  rbind(a[-drop, ], do.call(rbind, add))
}

## areas of each source, with their rule
B <- list(); V <- list(); other <- list(); NM <- list()
for (i in seq_len(nrow(meta))) {
  k <- meta$key[i]; f <- file.path("closures/output", paste0("build_", k, ".rds"))
  b <- if (file.exists(f)) readRDS(f) else NULL
  a <- if (!is.null(b)) b$areas else NULL
  if (!is.null(a)) a <- fill_from_capad(a)
  if (!is.null(a)) {
    ## areas with no geometry: defined by reference to State law (Kent Group
    ## National Park, Victorian marine national parks) or not a closure
    e <- a[st_is_empty(a), ]
    if (nrow(e)) NM[[length(NM) + 1]] <- data.frame(key = k, instrument = meta$name[i], start = meta$start[i], end = meta$end[i],
      schedule = e$schedule, area = e$area, reason = ifelse(grepl("no boundary steps", e$notes),
        "no coordinates in the text (defined by reference to other legislation, or not a closure)", e$notes))
    a <- a[!st_is_empty(a) & !grepl("^Amendments", a$area), ]
  }
  r <- rules[rules$key == k, ]
  if (is.null(a) || !nrow(a)) {
    other[[length(other) + 1]] <- data.frame(key = k, instrument = meta$name[i], start = meta$start[i], end = meta$end[i],
      measure = if (nrow(r)) paste(unique(r$sentence), collapse = " || ") else "(no operative clause found)")
    next
  }
  ## area names the splitter took from running text (2010): use the schedule title
  long <- nchar(a$area) > 90 | grepl("bounded by|commencing", a$area)
  a$area[long] <- a$schedule_title[long]
  rule_for <- function(s) {
    x <- r[r$schedule %in% c(s, if (s == 0) unique(r$schedule)), ]
    if (!nrow(x)) x <- r[r$schedule == 0, ]
    p <- x[x$gear != "exemption", ]; e <- x[x$gear == "exemption", ]
    ## gear from the standing closure; a catch trigger (dogfish: all methods for
    ## 12 months once more than three are taken) is flagged, not added to the gear
    pg <- if (any(!p$trigger)) p[!p$trigger, ] else p
    c(gear = if (nrow(pg)) paste(unique(pg$gear), collapse = "; ") else NA,
      trigger = any(p$trigger), species = any(p$species_specific),
      rule = paste(unique(substr(c(p$sentence, e$sentence), 1, 400)), collapse = " || "))
  }
  rr <- t(vapply(a$schedule, rule_for, character(4)))
  ## orange roughy management areas carry no closure clause of their own
  rr[is.na(rr[, "gear"]) & grepl("management area", a$area, TRUE), "gear"] <- "none (orange roughy management area)"
  a$gear_closed <- rr[, "gear"]; a$trigger <- rr[, "trigger"] == "TRUE"; a$species_specific <- rr[, "species"] == "TRUE"
  a$rule_text <- rr[, "rule"]
  a$source_key <- k; a$instrument <- meta$name[i]; a$title_id <- meta$title[i]
  ## where the boundary is written: PDF pages of the schedule (else of the coordinates)
  bk <- bp[bp$key == k, ]
  j <- match(a$schedule, bk$schedule); j[is.na(j)] <- match(0, bk$schedule)
  a$pdf_pages <- bk$pdf_pages[j]; a$pdf_file <- bk$file[j]; a$register_url <- bk$url[j]
  a$source_ref <- ifelse(is.na(a$pdf_pages), k, sprintf("%s, Schedule %s, PDF p. %s", k, a$schedule, a$pdf_pages))
  a$source_ref[a$schedule == 0 & !is.na(a$pdf_pages)] <- sprintf("%s, PDF p. %s", k, a$pdf_pages[a$schedule == 0 & !is.na(a$pdf_pages)])
  B[[k]] <- a
  if (!is.null(b$vertices)) { v <- b$vertices; v$source_key <- k; V[[k]] <- v }
}

## snapshot dates
dates <- sort(unique(c(meta$start, meta$end[!is.na(meta$end)])))
dates <- dates[dates <= TODAY]
nm <- function(x) gsub("[^a-z0-9]", "", gsub("closures?|area", "", tolower(x)))
sym_km2 <- function(g1, g2) {
  d <- suppressMessages(suppressWarnings(st_sym_difference(st_union(g1), st_union(g2))))
  if (length(d)) sum(as.numeric(st_area(st_transform(d, 3577)))) / 1e6 else 0
}
iou <- function(g1, g2) {
  g1 <- st_transform(g1, 3577); g2 <- st_transform(g2, 3577)
  it <- suppressMessages(suppressWarnings(st_intersection(g1, g2)))
  if (!length(it)) return(0)
  as.numeric(sum(st_area(it))) / as.numeric(sum(st_area(st_union(g1, g2))))
}
pair_rows <- function(prev, cur) {
  p <- rep(NA_integer_, nrow(cur))
  ## 1 same source text and same area name
  k1 <- paste(prev$source_key, prev$area); k2 <- paste(cur$source_key, cur$area)
  p <- match(k2, k1)
  ## each earlier row continues at most once (pieces of one area share a name)
  p[duplicated(p) & !is.na(p)] <- NA
  ## 2 name match (small spelling differences allowed)
  d <- adist(nm(cur$area), nm(prev$area))
  for (i in which(is.na(p))) { free <- setdiff(seq_len(nrow(prev)), na.omit(p)); if (!length(free)) break
    x <- d[i, free]; if (min(x) <= 3) p[i] <- free[which.min(x)] }
  ## 3 overlap
  for (i in which(is.na(p))) {
    free <- setdiff(seq_len(nrow(prev)), na.omit(p)); if (!length(free)) break
    hit <- free[lengths(st_intersects(st_geometry(prev)[free], st_geometry(cur)[i])) > 0]
    if (!length(hit)) next
    s <- vapply(hit, function(j) iou(st_geometry(cur)[i], st_geometry(prev)[j]), 0)
    if (max(s) > 0.5) p[i] <- hit[which.max(s)]
  }
  p
}

rows <- list(); nclos <- 0; open <- NULL; changes <- list()
for (di in seq_along(dates)) {
  d <- dates[di]
  act <- meta$key[meta$start <= d & (is.na(meta$end) | meta$end > d)]
  act <- act[act %in% names(B)]
  a <- if (length(act)) do.call(rbind, lapply(act, function(k) B[[k]])) else NULL
  ended_here <- meta$key[!is.na(meta$end) & meta$end == d]; started_here <- meta$key[meta$start == d]
  what <- paste(c(if (length(started_here)) paste("starts:", paste(started_here, collapse = ", ")),
                  if (length(ended_here)) paste("ends:", paste(ended_here, collapse = ", "))), collapse = "; ")
  changes[[length(changes) + 1]] <- data.frame(date = d, what = what, n_areas_in_effect = if (is.null(a)) 0 else nrow(a))
  if (!is.null(open)) {
    p <- if (is.null(a)) integer() else pair_rows(open, a)
    for (j in setdiff(seq_len(nrow(open)), na.omit(p))) { rows[[open$row_ref[j]]]$valid_to <- d - 1
      rows[[open$row_ref[j]]]$ended_by <- what }
  }
  if (is.null(a)) { open <- NULL; next }
  a$valid_from <- d; a$valid_to <- as.Date(NA); a$row_ref <- NA_integer_; a$closure_no <- NA_integer_; a$change <- NA_character_
  for (i in seq_len(nrow(a))) {
    j <- if (is.null(open)) NA else p[i]
    if (is.na(j)) { nclos <- nclos + 1; a$closure_no[i] <- nclos; a$change[i] <- paste("new:", what); next }
    a$closure_no[i] <- open$closure_no[j]
    same <- a$source_key[i] == open$source_key[j] && a$area[i] == open$area[j]
    ## a new version when the boundary OR the gear rule changes
    same_rule <- identical(a$gear_closed[i], open$gear_closed[j]) && identical(a$trigger[i], open$trigger[j])
    if (same || (same_rule && sym_km2(st_geometry(a)[i], st_geometry(open)[j]) < 0.5)) {
      a$row_ref[i] <- open$row_ref[j]
      rows[[a$row_ref[i]]]$source_last <- a$source_key[i]; rows[[a$row_ref[i]]]$instrument_last <- a$instrument[i]
    } else {
      rows[[open$row_ref[j]]]$valid_to <- d - 1
      bchg <- sym_km2(st_geometry(a)[i], st_geometry(open)[j]) >= 0.5
      rchg <- if (identical(open$gear_closed[j], a$gear_closed[i]))
                sprintf("catch-trigger / conditional rule %s", if (isTRUE(a$trigger[i])) "added" else "removed") else
                sprintf("gear rule changed (%s -> %s)", open$gear_closed[j], a$gear_closed[i])
      a$change[i] <- paste0(if (bchg && !same_rule) paste("boundary changed;", rchg) else if (bchg) "boundary changed" else rchg, ": ", what)
    }
  }
  for (i in which(is.na(a$row_ref))) {
    r <- a[i, ]; r$ended_by <- NA_character_; r$source_last <- r$source_key; r$instrument_last <- r$instrument
    rows[[length(rows) + 1]] <- r; a$row_ref[i] <- length(rows)
  }
  open <- a
}
cl <- do.call(rbind, rows)
## a closure that lapsed and came back (e.g. no consolidated direction between
## 1 Jan and 13 Jul 2007) is the same closure: same name, overlapping, gap < 400 days
repeat {
  merged <- FALSE
  fst <- tapply(seq_len(nrow(cl)), cl$closure_no, function(ix) ix[which.min(cl$valid_from[ix])])
  lst <- tapply(seq_len(nrow(cl)), cl$closure_no, function(ix) ix[which.max(cl$valid_from[ix])])
  for (c1 in names(fst)) {
    i <- fst[[c1]]
    for (c0 in names(lst)) {
      if (c0 == c1) next
      j <- lst[[c0]]
      if (is.na(cl$valid_to[j]) || cl$valid_to[j] >= cl$valid_from[i] || cl$valid_from[i] - cl$valid_to[j] > 400) next
      if (adist(nm(cl$area[i]), nm(cl$area[j])) > 3) next
      if (iou(st_geometry(cl)[i], st_geometry(cl)[j]) < 0.5) next
      cl$closure_no[cl$closure_no == as.integer(c1)] <- as.integer(c0)
      cl$change[i] <- paste0(cl$change[i], " (resumed after a gap from ", cl$valid_to[j] + 1, ")")
      merged <- TRUE; break
    }
    if (merged) break
  }
  if (!merged) break
}
cl$closure_no <- match(cl$closure_no, unique(cl$closure_no[order(cl$valid_from)]))
cl$closure_id <- sprintf("C%03d", cl$closure_no)
cl <- cl[order(cl$closure_no, cl$valid_from), ]
cl$version <- ave(seq_len(nrow(cl)), cl$closure_no, FUN = seq_along)
cl$version_id <- sprintf("%s_v%d", cl$closure_id, cl$version)
names(cl)[names(cl) == "features"] <- "approximated_features"
keep <- c("version_id", "closure_id", "version", "area", "schedule", "schedule_title", "instrument", "source_key",
          "instrument_last", "source_last", "valid_from", "valid_to", "change", "ended_by", "gear_closed", "trigger",
          "species_specific", "rule_text", "quality", "approximated_features", "notes", "area_km2", "n_points", "method",
          "source_ref", "pdf_pages", "pdf_file", "register_url", "title_id")
cl <- cl[, intersect(keep, names(cl))]

## agreement with AFMA's own polygons (step 12 / 12b), for the directions AFMA mapped
c16 <- read.csv("closures/output/afma_comparison_2016.csv"); c21 <- read.csv("closures/output/afma_comparison.csv")
c21 <- c21[c21$our_version == "2025", ]
cl$afma_iou <- NA_real_; cl$afma_layer <- NA_character_
i16 <- cl$title_id == "F2016L00549" & !(cl$schedule %in% c(13, 34))
cl$afma_iou[i16] <- c16$iou[match(cl$schedule[i16], c16$schedule)]; cl$afma_layer[i16 & !is.na(cl$afma_iou)] <- "afma_2016"
i21 <- cl$title_id == "F2021L00445"
cl$afma_iou[i21] <- c21$iou[match(cl$schedule[i21], c21$schedule)]; cl$afma_layer[i21 & !is.na(cl$afma_iou)] <- "afma_2024"
cl$afma_flag <- ifelse(is.na(cl$afma_iou), NA, ifelse(cl$afma_iou >= 0.99, "matches AFMA",
                  ifelse(cl$afma_iou >= 0.9, "close to AFMA (coast/AFZ approximation)", "differs from AFMA: prefer AFMA polygon")))

## companion layer: AFMA's own polygons of the 2021 direction's schedules (bundle 16 May 2024)
afma <- NULL
fs <- list.files("closures/afma_2024/unzipped/GDA Extracted", pattern = "\\.shp$", full.names = TRUE)
if (length(fs)) afma <- do.call(rbind, lapply(fs, function(f) {
  x <- st_read(f, quiet = TRUE); g <- st_union(st_make_valid(st_transform(st_zm(st_geometry(x)), 4326)))
  st_sf(afma_file = basename(f), geometry = g)
}))
cmp <- if (file.exists("closures/output/afma_comparison.csv")) read.csv("closures/output/afma_comparison.csv") else NULL

instr <- meta[, c("key", "title", "kind", "name", "start", "end", "register_end", "commencement_text", "end_text")]
other <- do.call(rbind, other)
V <- do.call(rbind, V); V <- V[is.finite(V$lat) & is.finite(V$lon), ]
g <- "closures/output/sessf_closures.gpkg"
if (file.exists(g)) file.remove(g)
st_write(cl, g, layer = "closures", quiet = TRUE)
st_write(st_as_sf(V, coords = c("lon", "lat"), crs = 4326, remove = FALSE), g, layer = "vertices", quiet = TRUE, append = TRUE)
st_write(instr, g, layer = "instruments", quiet = TRUE, append = TRUE)
st_write(other, g, layer = "other_measures", quiet = TRUE, append = TRUE)
NM <- do.call(rbind, NM)
if (!is.null(NM)) { st_write(NM, g, layer = "not_mapped", quiet = TRUE, append = TRUE)
  write.csv(NM, "closures/output/sessf_closures_not_mapped.csv", row.names = FALSE, na = "") }
st_write(do.call(rbind, changes), g, layer = "changes", quiet = TRUE, append = TRUE)
if (!is.null(afma)) st_write(afma, g, layer = "afma_2024", quiet = TRUE, append = TRUE)
fs16 <- list.files("closures/afma_2016/unzipped", pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (length(fs16)) {
  afma16 <- do.call(rbind, lapply(fs16, function(f) {
    x <- st_read(f, quiet = TRUE); gg <- st_zm(st_geometry(x)); if (is.na(st_crs(gg))) st_crs(gg) <- 4283
    st_sf(schedule = as.integer(sub("^S([0-9]+).*", "\\1", basename(dirname(dirname(f))))), afma_file = basename(f),
          geometry = st_union(st_make_valid(st_transform(gg, 4326))))
  }))
  st_write(afma16, g, layer = "afma_2016", quiet = TRUE, append = TRUE)
}
pdfs <- read.csv("closures/output/legislation_pdfs.csv", stringsAsFactors = FALSE)
st_write(pdfs, g, layer = "legislation_pdfs", quiet = TRUE, append = TRUE)
st_write(bp, g, layer = "boundary_pages", quiet = TRUE, append = TRUE)
w <- st_drop_geometry(cl); w$wkt <- st_as_text(st_geometry(cl), digits = 8)
write.csv(w, "closures/output/sessf_closures_wkt.csv", row.names = FALSE, na = "")
write.csv(V, "closures/output/sessf_closures_vertices.csv", row.names = FALSE, na = "")
write.csv(instr, "closures/output/sessf_closures_instruments.csv", row.names = FALSE, na = "")
write.csv(other, "closures/output/sessf_closures_other_measures.csv", row.names = FALSE, na = "")
write.csv(do.call(rbind, changes), "closures/output/sessf_closures_changes.csv", row.names = FALSE)
cat("closure versions:", nrow(cl), " closures:", length(unique(cl$closure_id)),
    " in force now:", sum(is.na(cl$valid_to)), " other measures:", nrow(other), "\n")
print(table(cl$quality, useNA = "ifany"))
print(table(format(cl$valid_from, "%Y")))
