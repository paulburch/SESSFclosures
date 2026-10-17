#' Were fishing positions inside a closure that applied to them?
#'
#' For each position (e.g. a shot's start position) and date, the closure
#' versions that contained it on that date, and whether the closure's rule
#' applied to the gear used.
#'
#' @param x A data frame with longitude, latitude and date columns, or an `sf`
#'   point object with a date column.
#' @param date,lon,lat Column names in `x` (`lon`/`lat` ignored for `sf`).
#' @param gear Optional gear column name in `x`, or a single gear for all rows.
#'   When given, `applies` says whether each closure closes that gear.
#' @param management_areas Include orange roughy management areas? Default
#'   `FALSE`.
#' @return A data frame with one row per position x closure match: `row`
#'   (row of `x`), `version_id`, `closure_id`, `area`, `gear_closed`,
#'   `trigger` (closure applies only after a catch trigger or condition),
#'   `quality` and, with `gear`, `applies`. Positions in no closure do not
#'   appear.
#' @details Positions are compared with the polygons in WGS84. Boundaries
#'   that follow the coastline, coastal waters or the AFZ are approximated
#'   (`quality == "approx_feature"`): near those edges, check against the
#'   AFMA polygons ([afma_polygons()]) or the instrument.
#' @examples
#' shots <- data.frame(lon = c(148.2, 150.3, 137.3), lat = c(-39.5, -37.5, -36.01),
#'                     date = as.Date(c("2010-03-01", "2024-06-01", "2008-01-01")),
#'                     gear = c("TW", "TW", "GN"))
#' in_closure(shots, gear = "gear")
#' @export
in_closure <- function(x, date = "date", lon = "lon", lat = "lat", gear = NULL, management_areas = FALSE) {
  ## the polygons were built on the plane (lon/lat, s2 off); some approximated
  ## AFZ / coastal-waters rings are not valid on the sphere
  old <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  pts <- if (inherits(x, "sf")) sf::st_transform(sf::st_geometry(x), 4326) else
    sf::st_as_sf(data.frame(lon = x[[lon]], lat = x[[lat]]), coords = c("lon", "lat"), crs = 4326)$geometry
  dts <- as.Date(if (inherits(x, "sf")) sf::st_drop_geometry(x)[[date]] else x[[date]])
  cl <- SESSFclosures::closures
  if (!management_areas) cl <- cl[!grepl("^none", cl$gear_closed), ]
  hit <- suppressMessages(sf::st_intersects(pts, cl))
  i <- rep(seq_along(hit), lengths(hit)); j <- unlist(hit)
  if (!length(i)) return(data.frame(row = integer(), version_id = character(), closure_id = character()))
  ok <- !is.na(dts[i]) & cl$valid_from[j] <= dts[i] & (is.na(cl$valid_to[j]) | cl$valid_to[j] >= dts[i])
  i <- i[ok]; j <- j[ok]
  out <- data.frame(row = i, version_id = cl$version_id[j], closure_id = cl$closure_id[j], area = cl$area[j],
                    gear_closed = cl$gear_closed[j], trigger = cl$trigger[j], quality = cl$quality[j],
                    stringsAsFactors = FALSE)
  if (!is.null(gear)) {
    g <- if (length(gear) == 1 && !inherits(x, "sf") && gear %in% names(x)) x[[gear]][i] else
         if (length(gear) == 1 && inherits(x, "sf") && gear %in% names(x)) sf::st_drop_geometry(x)[[gear]][i] else rep(gear, length(i))
    out$applies <- gear_applies(out$gear_closed, g)
  }
  out[order(out$row, out$closure_id), , drop = FALSE]
}
