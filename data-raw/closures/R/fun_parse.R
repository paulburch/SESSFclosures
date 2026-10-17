## =============================================================================
## Closures review: parse a schedule's text into areas and build their geometry
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
NM <- 1852

num_word <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, eight = 8,
              ten = 10, eleven = 11, twelve = 12)

## which boundary feature a phrase refers to
classify_feature <- function(x) {
  x <- tolower(x)
  if (grepl("outer limits? of (the )?afz|outer limits? of the australian fishing zone", x)) return("AFZ")
  if (grepl("outer limit of [a-z ]*coastal waters|boundary of the coastal waters|(and|with) (the|these) coastal waters", x)) return("coastal_waters")
  m <- regmatches(x, regexec("(one|two|three|four|five|six|eight|ten|eleven|twelve|[0-9]+(?:\\.[0-9]+)?)[ -]nautical[ -]mile", x, perl = TRUE))[[1]]
  if (length(m) == 2 && grepl("seaward|line", x)) {
    n <- if (m[2] %in% names(num_word)) num_word[[m[2]]] else as.numeric(m[2])
    return(paste0("nm_", n))
  }
  if (grepl("adjacent area boundary", x)) return("adjacent")
  if (grepl("low water|coast ?line", x)) return("coast")
  "line"
}

## split an area's text into ordered chunks: lettered clauses, roman items, table rows
chunk_area <- function(txt) {
  l <- strsplit(txt, "\n")[[1]]
  l <- l[nzchar(trimws(l))]
  ## older instruments (2016): clauses are sentences beginning "Then ...",
  ## sometimes several in one paragraph
  l <- unlist(strsplit(l, "(?<=[,.;])\\s+(?=[Tt]hen(ce)?[\\s,])", perl = TRUE))   # 2010: "Thence"
  ## and point tables without point numbers: a latitude line then a longitude line
  islat <- grepl("^\\s*[0-9]{1,3}\\s*°[^°]*(south|north)\\s*[,.;]?\\s*$", l, ignore.case = TRUE)
  islon <- grepl("^\\s*[0-9]{1,3}\\s*°[^°]*(east|west)\\s*[,.;]?\\s*$", l, ignore.case = TRUE)
  ## (2008-2010 HTML tables: "170" / longitude / latitude on separate lines, either order)
  islat <- islat | grepl("^\\s*[0-9]{1,3}\\s*°[^°]*\\bS\\s*$", l)
  islon <- islon | grepl("^\\s*[0-9]{1,3}\\s*°[^°]*\\bE\\s*$", l)
  isnum <- grepl("^\\s*[0-9]{1,3}\\s*$", l)
  if (any(islat)) {
    out <- character(); i <- 1; n <- length(l)
    while (i <= n) {
      nm0 <- if (isnum[i] && i + 2 <= n) trimws(l[i]) else "0"; j <- if (isnum[i] && i + 2 <= n) i + 1 else i
      if (j < n && islat[j] && islon[j + 1]) { out <- c(out, paste(nm0, "  ", l[j], "  ", l[j + 1])); i <- j + 2 }
      else if (j < n && islon[j] && islat[j + 1]) { out <- c(out, paste(nm0, "  ", l[j + 1], "  ", l[j])); i <- j + 2 }
      else { out <- c(out, l[i]); i <- i + 1 }
    }
    l <- out
  }
  start <- grepl("^\\s*\\(?[a-z]\\)\\s|^\\s*\\(?[ivx]{1,4}[.)]\\s|^\\s*[0-9]{1,3}\\s+[0-9]{1,3}\\s*°|^\\s*[Tt]hen(ce)?[[:space:],]|^\\s*Also\\s", l)
  ## compilations shown without clause letters (Register HTML, 2024-25 Sch 40):
  ## each clause is a line beginning with "commencing" or a direction
  start <- start | grepl("^\\s*(commencing|(generally |initially )?(north|south|east|west)[a-z -]*\\s+(along|to)\\b)", l, ignore.case = TRUE)
  start[1] <- TRUE
  id <- cumsum(start)
  ch <- tapply(l, id, function(x) paste(trimws(x), collapse = " "))
  unname(ch)
}

## coordinates in a chunk, each tagged as a hint ("close to", "near") or definite
coords_tagged <- function(ch) {
  co <- find_coords(ch)
  if (!nrow(co)) { co$hint <- logical(); return(co) }
  pre <- vapply(co$pos, function(p) tolower(substr(ch, max(1, p - 60), p - 1)), "")
  ## a hint applies to the coordinate it introduces and to the one right after it
  hint <- grepl("(close to|closest to|near|vicinity)[^;]*$", pre) &
          !grepl("(to the point|at the point|commencing at the point)[^;]*$", pre)
  co$hint <- hint
  co
}

gv0 <- function(x) if (is.null(x)) NA else x

## parse one area into steps and extras
parse_area <- function(txt) {
  ch <- chunk_area(txt)
  steps <- list(); circles <- list(); excl <- list(); in_excl <- FALSE; also <- FALSE
  ## 2005-2013: "The area within 1 nautical mile around: (a) North Page Island
  ## located at ...; (b) ..." -> a circle round each listed point
  around <- regmatches(tolower(txt), regexec("within ([0-9.]+) nautical miles? (around|of)\\s*:", tolower(txt)))[[1]]
  r_around <- if (length(around)) as.numeric(around[2]) else NA
  for (c0 in ch) {
    x <- tolower(c0)
    ## the schedule heading is not a point, even when its title is a latitude ("41° South", 2005)
    if (grepl("^\\s*schedule\\s+[0-9]+", x) && !grepl("commencing|beginning at|radius|around|point of latitude", x)) next
    if (grepl("excludes|does not include|but excluding", x)) { in_excl <- TRUE }
    if (grepl("also included", x)) also <- TRUE
    if (!is.na(r_around) && !in_excl) {
      co <- coords_tagged(c0)
      if (any(co$type == "lat") && any(co$type == "lon")) {
        circles[[length(circles) + 1]] <- list(kind = "circle", lat = co$value[co$type == "lat"][1],
          lon = co$value[co$type == "lon"][1], r_nm = r_around, text = c0)
        next
      }
    }
    ## circles: "circle of radius 15 nautical miles", "a 15 nautical mile radius from" (2005-2013)
    if (grepl("[0-9.]+ nautical miles? radius", x)) x <- sub("(?:a )?([0-9.]+) nautical miles? radius", "circle of radius \\1 nautical miles", x, perl = TRUE)
    if (grepl("circle of radius", x)) {
      r <- as.numeric(sub(".*radius (of )?([0-9.]+) nautical.*", "\\2", x))
      co <- coords_tagged(c0)
      lat <- co$value[co$type == "lat"][1]; lon <- co$value[co$type == "lon"][1]
      item <- list(kind = "circle", lat = lat, lon = lon, r_nm = r, text = c0)
      if (in_excl && !also) excl[[length(excl) + 1]] <- item else circles[[length(circles) + 1]] <- item
      next
    }
    if (in_excl && grepl("schedule\\s+[0-9]+", x)) {
      excl[[length(excl) + 1]] <- list(kind = "schedule",
        schedule = as.integer(sub(".*schedule\\s+([0-9]+).*", "\\1", x)), text = c0)
      next
    }
    if (in_excl) next
    ## table row: n  lat  lon
    if (grepl("^\\s*[0-9]{1,3}\\s+[0-9]{1,3}\\s*°", c0)) {
      co <- find_coords(c0)
      if (sum(co$type == "lat") >= 1 && sum(co$type == "lon") >= 1)
        steps[[length(steps) + 1]] <- list(lat = co$value[co$type == "lat"][1],
          lon = co$value[co$type == "lon"][1], edge = "line", on = NA, hlat = NA, hlon = NA,
          close = FALSE, text = c0)
      next
    }
    ## lettered clause
    co <- coords_tagged(c0)
    def <- co[!co$hint, ]; hin <- co[co$hint, ]
    commence <- grepl("commencing|beginning at", x)
    close <- grepl("point of commencement|back to the point of commencement|point where the line began", x)
    ## connector clauses ("along the geodesics sequentially connecting the
    ## following points") and the introductory sentence carry no point
    if (!nrow(co) && !close && !grepl("intersection|to the point|point of", x)) next
    ## a coordinate that is not on the AFZ etc. is sometimes a known place
    known <- NULL
    if (grepl("(boundary|border) between victoria and south australia|(boundary|border) between south australia and victoria", x))
      known <- c(lon = 140.965278)        # SA/Vic border meridian, 140 57' 55" E (approx.)
    ## the feature travelled ALONG to reach this point
    along <- regmatches(x, regexec("(?:along|following) (.*?)(to its|to the|to a |to latitude|to longitude|to [0-9]|back to|the point of|until|$)", x, perl = TRUE))[[1]]
    edge <- if (commence || !length(along)) "line" else classify_feature(along[2])
    ## "along that line / that outer limit": the feature the previous point lay on (2007 Sch 11)
    if (edge == "line" && length(along) && grepl("^(that|this) (line|outer limit|limit)", along[2]) && length(steps) &&
        !is.na(steps[[length(steps)]]$on)) edge <- steps[[length(steps)]]$on
    ## "along the geodesic and outer limit of the coastal waters ... to the point
    ## of latitude ..., longitude ..." between explicit points: a straight edge
    if (edge != "line" && length(along) && grepl("geodesic", along[2]) &&
        sum(def$type == "lat") >= 1 && sum(def$type == "lon") >= 1) edge <- "line"
    ## the feature this point lies ON
    on_txt <- regmatches(x, regexec("intersect(?:ion|s) (of|with|the) (.*)", x, perl = TRUE))[[1]]
    on <- if (length(on_txt)) classify_feature(on_txt[3]) else NA
    if (identical(on, "line")) on <- NA
    ## which coordinate the movement keeps: along a meridian (north/south) the
    ## longitude, along a parallel (east/west) the latitude
    mv <- if (!length(along)) "" else along[2]
    fix <- ""
    if (edge == "line" && !grepl("geodesic", mv)) {
      ## the instruments sometimes swap the words ("parallel of longitude")
      if (grepl("meridian of latitude", mv)) fix <- "lat"
      else if (grepl("parallel of longitude", mv)) fix <- "lon"
      else if (grepl("meridian", mv) || grepl("\\b(north|south)(erly)? along", x)) fix <- "lon"
      else if (grepl("parallel", mv) || grepl("\\b(east|west)(erly)? along", x)) fix <- "lat"
    }
    ## coordinates inside the "along ..." phrase name the line travelled
    ## ("east along the parallel of latitude 34 40 S"); only those after it
    ## name the end point
    tl_lat <- NA; tl_lon <- NA
    if (length(along) && !commence && nrow(def)) {
      a0 <- regexpr("along |following ", x); a1 <- a0 + nchar(along[1]) - nchar(along[3])
      inl <- def$pos >= a0 & def$pos < a1
      if (any(inl & def$type == "lat")) tl_lat <- def$value[inl & def$type == "lat"][1]
      if (any(inl & def$type == "lon")) tl_lon <- def$value[inl & def$type == "lon"][1]
      def <- def[!inl, ]
    }
    if (!is.na(tl_lon) && fix == "") fix <- "lon"
    if (!is.na(tl_lat) && fix == "") fix <- "lat"
    st <- list(lat = if (any(def$type == "lat")) def$value[def$type == "lat"][1] else NA,
               lon = if (any(def$type == "lon")) def$value[def$type == "lon"][1] else if (!is.null(known)) known[["lon"]] else NA,
               tl_lat = tl_lat, tl_lon = tl_lon,
               edge = edge, on = on, fix = fix,
               hlat = if (any(hin$type == "lat")) hin$value[hin$type == "lat"][1] else NA,
               hlon = if (any(hin$type == "lon")) hin$value[hin$type == "lon"][1] else NA,
               close = close, commence = commence, text = c0)
    ## a clause with several definite pairs (rare) adds the extra points as line steps
    np <- min(sum(def$type == "lat"), sum(def$type == "lon"))
    steps[[length(steps) + 1]] <- st
    if (np > 1 && !close) for (k in 2:np) steps[[length(steps) + 1]] <- list(
      lat = def$value[def$type == "lat"][k], lon = def$value[def$type == "lon"][k],
      edge = "line", on = NA, hlat = NA, hlon = NA, close = FALSE, text = c0)
  }
  ## a long point table numbered the wrong way round (2013 automatic longline
  ## closure: Point 1 is at 150° E, the text says "initially south westerly",
  ## but points 2..177 run from 129° E eastwards): if the point before the table
  ## lies next to its LAST row and far from its first, read the table backwards
  pnotes <- character()
  isrow <- vapply(steps, function(s) grepl("^\\s*[0-9]{1,3}\\s+[0-9]{1,3}\\s*°", s$text), TRUE)
  if (sum(isrow) >= 5) {
    r <- rle(isrow); ends <- cumsum(r$lengths); begs <- ends - r$lengths + 1
    for (q in which(r$values & r$lengths >= 5)) {
      b <- begs[q]; e <- ends[q]; if (b < 2) next
      pv <- steps[[b - 1]]; plon <- if (!is.na(pv$lon)) pv$lon else gv0(pv$tl_lon); plat <- if (!is.na(pv$lat)) pv$lat else gv0(pv$tl_lat)
      dd <- function(s) sqrt(ifelse(is.na(plon), 0, (s$lon - plon)^2) + ifelse(is.na(plat), 0, (s$lat - plat)^2))
      if (is.na(plon) && is.na(plat)) next
      if (dd(steps[[e]]) < 0.5 && dd(steps[[b]]) > 5 * max(dd(steps[[e]]), 0.1)) {
        steps[b:e] <- rev(steps[b:e])
        pnotes <- c(pnotes, sprintf("point table (%d rows) read in reverse order: its last point, not its first, adjoins the point before it", e - b + 1))
      }
    }
  }
  list(steps = steps, circles = circles, excl = excl, notes = pnotes)
}

## islands / features with "a line N nautical miles seaward of: X located near ..."
parse_seaward_points <- function(txt) {
  x <- tolower(txt)
  n <- regmatches(x, regexec("line\\s+(one|two|three|four|five|six|eight|ten|eleven|twelve|[0-9.]+)\\s+nautical\\s+miles?\\s+seaward\\s+of", x))[[1]]
  if (!length(n)) return(NULL)
  r <- if (n[2] %in% names(num_word)) num_word[[n[2]]] else as.numeric(n[2])
  co <- find_coords(txt)
  lat <- co$value[co$type == "lat"]; lon <- co$value[co$type == "lon"]
  k <- min(length(lat), length(lon))
  ## "(a) Nuyts Reef (west) located near ..." / "... located at latitude ..." (2010)
  nm <- regmatches(txt, gregexpr("[a-z]\\)\\s+([A-Z][A-Za-z' ()]+?)\\s+(located\\s+)?(near|at)\\b", txt, perl = TRUE))[[1]]
  nm <- trimws(sub("^[a-z]\\)\\s+", "", sub("\\s+(located\\s+)?(near|at)$", "", nm)))
  ## a single place: "ten nautical miles seaward of Seal Bay located at ..."
  if (!length(nm)) { m1 <- regmatches(txt, regexec("seaward of ([A-Z][A-Za-z' ]+?)\\s+(located|near|at)\\b", txt, perl = TRUE))[[1]]
    if (length(m1)) nm <- m1[2] }
  data.frame(name = c(nm, rep(NA, max(0, k - length(nm))))[seq_len(k)], lat = lat[1:k], lon = lon[1:k], r_nm = r)
}

## ---------------------------------------------------------------------------
## geometry
## ---------------------------------------------------------------------------
## Approximate positions of headlands named in "in the vicinity of ...". Used
## ONLY to choose between several crossings of a coastline, never as a vertex.
GAZETTEER <- rbind(
  c("cape willoughby", 138.13, -35.84), c("cape du couedic", 136.70, -36.06),
  c("troubridge point", 137.68, -35.16), c("cape jervis", 138.10, -35.60),
  c("newland head", 138.52, -35.65), c("victor harbor", 138.62, -35.56),
  c("point marsden", 137.64, -35.57), c("point brown", 133.86, -32.53),
  c("point bell", 133.13, -32.22), c("reynolds point", 137.72, -35.99))
gaz_hint <- function(text) {
  tx <- tolower(text)
  for (i in seq_len(nrow(GAZETTEER))) if (grepl(GAZETTEER[i, 1], tx, fixed = TRUE))
    return(as.numeric(GAZETTEER[i, 2:3]))
  NULL
}

## unit vector of the travel direction stated in a clause, NULL if none
direction_vector <- function(text) {
  tx <- tolower(text)
  m <- regmatches(tx, regexec("generally\\s+((north|south)?\\s*-?(east|west)?)erly", tx))[[1]]
  if (!length(m)) m <- regmatches(tx, regexec("\\b((north|south)?\\s*-?(east|west)?)erly\\b", tx))[[1]]
  if (!length(m)) m <- regmatches(tx, regexec("\\b((north|south)?()) along|\\b(()(east|west)) along", tx))[[1]]
  ## "then running east to the point of commencement" (2005)
  if (!length(m)) m <- regmatches(tx, regexec("(?:then|running)\\s+((north|south)?\\s*-?(east|west)?)\\s+to\\b", tx))[[1]]
  if (!length(m)) return(NULL)
  w <- m[2]
  v <- c((grepl("east", w)) - (grepl("west", w)), (grepl("north", w)) - (grepl("south", w)))
  if (all(v == 0)) return(NULL)
  v / sqrt(sum(v^2))
}

## keep the rings the text names: the mainland, Kangaroo Island or Tasmania
select_rings <- function(rings, text = "") {
  tx <- tolower(text)
  if (!length(rings) || !nzchar(tx)) return(rings)
  bb <- do.call(rbind, lapply(seq_along(rings), function(i) as.numeric(st_bbox(rings[i]))))
  ext <- (bb[, 3] - bb[, 1]) * (bb[, 4] - bb[, 2])
  cx <- (bb[, 1] + bb[, 3]) / 2; cy <- (bb[, 2] + bb[, 4]) / 2
  keep <- NULL
  if (grepl("king island", tx)) keep <- which(cx > 143.6 & cx < 144.3 & cy < -39.4 & cy > -40.4 & ext < 2)
  else if (grepl("flinders island", tx)) keep <- which(cx > 147.6 & cx < 148.5 & cy < -39.6 & cy > -40.4 & ext < 2)
  else if (grepl("kangaroo island", tx)) keep <- which(cx > 136.4 & cx < 138.3 & cy < -35.5 & cy > -36.2 & ext < 5)
  else if (grepl("tasmania", tx)) keep <- which(cx > 144 & cx < 149 & cy < -40.5 & cy > -44 & ext < 30)
  else if (grepl("mainland", tx)) keep <- which.max(ext)
  if (length(keep)) rings[keep] else rings
}

## intersection of a meridian (lon given) or parallel (lat given) with a feature
on_feature <- function(feat, f, lat = NA, lon = NA, near = NULL, text = "", cur = NULL, hinted = FALSE) {
  rings <- select_rings(feat[[f]], text)
  if (is.null(rings)) return(c(NA, NA))
  g <- gaz_hint(text); if (!is.null(g)) near <- g
  ln <- if (!is.na(lon)) st_sfc(st_linestring(cbind(c(lon, lon), c(-60, -5))), crs = 4326) else
        st_sfc(st_linestring(cbind(c(100, 180), c(lat, lat))), crs = 4326)
  p <- suppressWarnings(st_cast(st_intersection(rings, ln), "POINT"))
  if (!length(p)) {
    ## the generalised coastline can stop just short of a headland the
    ## meridian / parallel touches: take the nearest coastline vertex
    xy <- do.call(rbind, lapply(seq_along(rings), function(i) st_coordinates(rings[i])[, 1:2]))
    d <- if (!is.na(lon)) abs(xy[, 1] - lon) else abs(xy[, 2] - lat)
    if (!is.null(near) && all(is.finite(near))) d <- d + 0.01 * sqrt((xy[, 1] - near[1])^2 + (xy[, 2] - near[2])^2)
    v <- xy[which.min(d), ]
    if (min(d) > 0.05) return(c(NA, NA))
    return(if (!is.na(lon)) c(lon, v[2]) else c(v[1], lat))
  }
  xy <- st_coordinates(p)[, 1:2, drop = FALSE]
  tx <- tolower(text)
  ## a named headland the generalised coastline misses: no crossing within
  ## ~10 km of it, so put the point on the stated meridian / parallel at the
  ## headland's other coordinate
  if (!is.null(g)) {
    dg <- sqrt((xy[, 1] - g[1])^2 + (xy[, 2] - g[2])^2)
    if (min(dg) > 0.1) return(if (!is.na(lon)) c(lon, g[2]) else c(g[1], lat))
  }
  if (nrow(xy) > 1 && is.null(gaz_hint(text))) {
    ## "the southern coastline" on a meridian: the southernmost crossing, etc.
    if (!is.na(lon) && grepl("southern coast", tx)) return(xy[which.min(xy[, 2]), ])
    if (!is.na(lon) && grepl("northern coast", tx)) return(xy[which.max(xy[, 2]), ])
    if (!is.na(lat) && grepl("eastern coast", tx)) return(xy[which.max(xy[, 1]), ])
    if (!is.na(lat) && grepl("western coast", tx)) return(xy[which.min(xy[, 1]), ])
  }
  ## the direction of travel the clause states ("then generally north easterly
  ## along ...") picks the crossing that lies that way from the current point
  dv <- direction_vector(text)
  if (nrow(xy) > 1 && !hinted && !is.null(dv) && is.null(gaz_hint(text)) && !is.null(cur) && all(is.finite(cur))) {
    dx <- xy[, 1] - cur[1]; dy <- xy[, 2] - cur[2]; r <- sqrt(dx^2 + dy^2)
    sc <- (dx * dv[1] + dy * dv[2]) / pmax(r, 1e-9)
    ok <- r > 0.001
    ## the nearest crossing that lies the stated way (2007 Head of Bight: "then
    ## northerly along that meridian to ... the mainland" is the near coast,
    ## not the north coast of Australia)
    fw <- ok & sc > 0.5
    if (any(fw)) return(xy[which(fw)[which.min(r[fw])], ])
    if (any(ok)) return(xy[which(ok)[which.max(sc[ok])], ])
  }
  if (!is.null(near) && all(is.finite(near))) {
    d <- (xy[, 1] - near[1])^2 + (xy[, 2] - near[2])^2
    return(xy[which.min(d), ])
  }
  ## otherwise the crossing nearest the current point
  if (nrow(xy) > 1 && !is.null(cur) && all(is.finite(cur))) {
    d <- (xy[, 1] - cur[1])^2 + (xy[, 2] - cur[2])^2; d[d < 1e-4] <- Inf
    if (any(is.finite(d))) return(xy[which.min(d), ])
  }
  xy[1, ]
}

## the part of a feature ring between two points, the shorter way round
path_on_feature <- function(feat, f, p, q, text = "") {
  rings <- select_rings(feat[[f]], text)
  if (is.null(rings) || any(!is.finite(c(p, q)))) return(NULL)
  best <- NULL; bd <- Inf
  for (r in seq_along(rings)) {
    xy <- st_coordinates(rings[r])[, 1:2]
    dp <- (xy[, 1] - p[1])^2 + (xy[, 2] - p[2])^2; dq <- (xy[, 1] - q[1])^2 + (xy[, 2] - q[2])^2
    s <- min(dp) + min(dq)
    if (s < bd) { bd <- s; best <- list(xy = xy, i = which.min(dp), j = which.min(dq)) }
  }
  xy <- best$xy[-nrow(best$xy), , drop = FALSE]; n <- nrow(xy); i <- best$i; j <- best$j
  if (i > n) i <- 1; if (j > n) j <- 1
  fwd <- if (j >= i) i:j else c(i:n, 1:j)
  bwd <- if (i >= j) i:j else c(i:1, n:j)
  len <- function(ix) sum(sqrt(diff(xy[ix, 1])^2 + diff(xy[ix, 2])^2))
  ix <- if (len(fwd) <= len(bwd)) fwd else bwd
  ## the instrument often says which coast ("the southern coastline of
  ## Kangaroo Island"): take the way round on that side
  tx <- tolower(text)
  side <- if (grepl("southern coast", tx)) c(2, -1) else if (grepl("northern coast", tx)) c(2, 1) else
          if (grepl("eastern coast", tx)) c(1, 1) else if (grepl("western coast", tx)) c(1, -1) else NULL
  if (!is.null(side)) {
    m <- function(ix) mean(xy[ix, side[1]]) * side[2]
    ix <- if (m(fwd) >= m(bwd)) fwd else bwd
  }
  xy[ix, , drop = FALSE]
}

## resolve each step to a point, then assemble the ring
build_area <- function(pa, feat, start_near = NULL) {
  ## a commencement point on a feature ("the intersection of the coastline
  ## with the meridian ...") can cross the feature several times: build once,
  ## then rebuild taking the crossing nearest the second point
  if (is.null(start_near) && length(pa$steps) > 1 && isTRUE(pa$steps[[1]]$commence) &&
      (is.na(pa$steps[[1]]$lat) || is.na(pa$steps[[1]]$lon)) &&
      !all(is.finite(c(pa$steps[[1]]$hlat, pa$steps[[1]]$hlon)))) {
    first <- build_area(pa, feat, start_near = c(NA, NA))
    p2 <- first$pts[2, ]
    ## the closing clause may say which way the start lies ("then running east
    ## to the point of commencement", 2005 Sch 4): the crossing that way from
    ## the last point
    n <- length(pa$steps); s1 <- pa$steps[[1]]; sn <- pa$steps[[n]]
    f1 <- if (!is.na(s1$on)) s1$on else NA
    if (isTRUE(sn$close) && !is.na(f1) && !is.null(direction_vector(sn$text)) && n > 2 && all(is.finite(first$pts[n - 1, ]))) {
      xy <- on_feature(feat, f1, lat = if (is.na(s1$lon)) s1$lat else NA, lon = if (!is.na(s1$lon)) s1$lon else NA,
                       text = sn$text, cur = first$pts[n - 1, ])
      if (all(is.finite(xy))) p2 <- xy
    }
    if (all(is.finite(p2))) return(build_area(pa, feat, start_near = p2))
    return(first)
  }
  if (!is.null(start_near) && !all(is.finite(start_near))) start_near <- NULL
  st <- pa$steps
  notes <- if (!is.null(pa$notes)) pa$notes else character(); feats <- character()
  if (!length(st)) return(list(geom = NULL, notes = "no boundary steps", feats = feats, pts = NULL))
  cur <- c(NA, NA); pts <- matrix(NA, length(st), 2)
  gv <- function(s, n) if (is.null(s[[n]])) NA else s[[n]]
  for (k in seq_along(st)) {
    s <- st[[k]]
    if (isTRUE(s$close)) { pts[k, ] <- pts[1, ]; next }
    lat <- s$lat; lon <- s$lon
    ## moving along a parallel or meridian keeps that coordinate
    if (s$edge == "line" && !isTRUE(s$commence)) {
      if (identical(s$fix, "lon") && is.na(lon)) lon <- if (!is.na(gv(s, "tl_lon"))) s$tl_lon else cur[1]
      if (identical(s$fix, "lat") && is.na(lat)) lat <- if (!is.na(gv(s, "tl_lat"))) s$tl_lat else cur[2]
      if (is.null(s$fix) || !nzchar(s$fix)) {
        if (is.na(lon) && !is.na(lat) && is.na(s$on)) lon <- cur[1]
        if (is.na(lat) && !is.na(lon) && is.na(s$on)) lat <- cur[2]
      }
    }
    ## the end of a travelled line with no stated end: where the next step's
    ## line crosses it ("east along 34 40 S; then south along 123 40 E ...")
    if ((is.na(lat) || is.na(lon)) && is.na(s$on) && s$edge == "line" && k < length(st)) {
      nx <- st[[k + 1]]
      if (is.na(lon) && identical(nx$fix, "lon")) lon <- if (!is.na(gv(nx, "tl_lon"))) nx$tl_lon else nx$lon
      if (is.na(lat) && identical(nx$fix, "lat")) lat <- if (!is.na(gv(nx, "tl_lat"))) nx$tl_lat else nx$lat
    }
    hint <- c(if (!is.na(s$hlon)) s$hlon else if (!is.na(lon)) lon else cur[1],
              if (!is.na(s$hlat)) s$hlat else if (!is.na(lat)) lat else cur[2])
    if (is.na(lat) || is.na(lon)) {
      f <- if (!is.na(s$on)) s$on else if (s$edge != "line") s$edge else NA
      if (!is.na(f) && f == "line") f <- NA
      if (!is.na(f) && f %in% c("AFZ", "adjacent") && all(is.finite(c(s$hlat, s$hlon)))) {
        lat <- s$hlat; lon <- s$hlon; notes <- c(notes, sprintf("point on %s taken from the instrument's 'close to' point", f))
      } else if (!is.na(f) && f != "adjacent") {
        if (is.na(lat) && is.na(lon)) {
          if (all(is.finite(c(s$hlat, s$hlon)))) { lat <- s$hlat; lon <- s$hlon }
        } else {
          nr <- if (k == 1 && !is.null(start_near)) start_near else if (all(is.finite(hint))) hint else NULL
          xy <- on_feature(feat, f, lat = if (is.na(lon)) lat else NA, lon = if (!is.na(lon)) lon else NA,
                           near = nr, text = s$text, cur = if (k > 1) cur else NULL,
                           hinted = is.finite(s$hlat) || is.finite(s$hlon))
          lon <- xy[1]; lat <- xy[2]
        }
        feats <- c(feats, f)
      } else if (is.finite(s$hlat) || is.finite(s$hlon)) {
        if (is.na(lat) && is.finite(s$hlat)) lat <- s$hlat
        if (is.na(lon) && is.finite(s$hlon)) lon <- s$hlon
        notes <- c(notes, "point completed from a 'near' / 'close to' coordinate")
      }
    }
    if (is.na(lat) || is.na(lon)) notes <- c(notes, sprintf("step %d unresolved: %s", k, substr(s$text, 1, 70)))
    pts[k, ] <- c(lon, lat)
    if (all(is.finite(pts[k, ]))) cur <- pts[k, ]
  }
  ## commencement points on a feature need the next point to pick the right intersection
  ring <- pts[1, , drop = FALSE]
  if (length(st) < 2) return(list(geom = NULL, notes = c(notes, "only one boundary step parsed"), feats = unique(feats), pts = pts))
  for (k in 2:length(st)) {
    s <- st[[k]]
    if (s$edge != "line" && s$edge != "adjacent") {
      seg <- path_on_feature(feat, s$edge, pts[k - 1, ], pts[k, ], s$text)
      feats <- c(feats, s$edge)
      if (!is.null(seg)) ring <- rbind(ring, seg)
    }
    ring <- rbind(ring, pts[k, , drop = FALSE])
  }
  ring <- ring[stats::complete.cases(ring), , drop = FALSE]
  if (nrow(ring) < 3) return(list(geom = NULL, notes = c(notes, "fewer than 3 points"), feats = unique(feats), pts = pts))
  if (any(ring[1, ] != ring[nrow(ring), ])) ring <- rbind(ring, ring[1, ])
  g <- tryCatch({
    g <- st_make_valid(st_sfc(st_polygon(list(ring)), crs = 4326))
    g <- suppressWarnings(st_collection_extract(g, "POLYGON"))
    if (!length(g)) NULL else st_union(g)
  }, error = function(e) NULL)
  if (is.null(g)) return(list(geom = NULL, notes = c(notes, "ring did not form a valid polygon"), feats = unique(feats), pts = pts))
  list(geom = g, notes = notes, feats = unique(feats), pts = pts)
}

circle_geom <- function(lat, lon, r_nm) {
  if (!all(is.finite(c(lat, lon, r_nm)))) return(st_sfc(st_polygon(), crs = 4326))
  loc <- sprintf("+proj=aeqd +lat_0=%.8f +lon_0=%.8f +datum=WGS84 +units=m", lat, lon)
  st_transform(st_buffer(st_sfc(st_point(c(0, 0)), crs = loc), r_nm * NM, nQuadSegs = 90), 4326)
}

## split a schedule into its areas ("The area of ... is ...")
split_areas <- function(sch_text) {
  l <- strsplit(sch_text, "\n")[[1]]
  st <- grep("^\\s*The areas? (of|known as)|^\\s*The area of", l)
  if (!length(st)) return(list(list(name = NA, text = sch_text)))
  ends <- c(st[-1] - 1, length(l))
  lapply(seq_along(st), function(i) {
    t <- paste(l[st[i]:ends[i]], collapse = "\n")
    nm <- sub("^\\s*The areas? (of|known as) (the )?", "", paste(l[st[i]:min(ends[i], st[i] + 2)], collapse = " "))
    nm <- trimws(sub("\\s+(is|are)\\s+(that|the)\\s+part.*$", "", nm))
    list(name = nm, text = t)
  })
}
