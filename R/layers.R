gpkg_path <- function() {
  f <- system.file("extdata", "sessf_closures.gpkg", package = "SESSFclosures")
  if (!nzchar(f)) stop("sessf_closures.gpkg not found in the installed package")
  f
}

#' The full GeoPackage
#'
#' Path of the bundled `sessf_closures.gpkg` (layers `closures`, `vertices`,
#' `instruments`, `legislation_pdfs`, `boundary_pages`, `other_measures`,
#' `not_mapped`, `changes`, `afma_2016`, `afma_2024`), for use in GIS software.
#' @return File path.
#' @examples
#' sf::st_layers(closures_gpkg())
#' @export
closures_gpkg <- function() gpkg_path()

#' AFMA's own closure polygons
#'
#' Polygons published by AFMA for the schedules of the 2016 direction
#' (F2016L00549; per-schedule files dated 21 June 2016; Schedules 13 and 34
#' are the April 2023 files) and the 2021 direction (bundle dated 16 May 2024).
#' Downloaded 17 October 2026. Where a closure follows the coastline, coastal
#' waters or the AFZ, AFMA's polygon is the better geometry; each version in
#' [closures] carries its agreement with AFMA (`afma_iou`, `afma_flag`).
#'
#' @param direction `"2021"` or `"2016"`.
#' @return An `sf` data frame.
#' @export
afma_polygons <- function(direction = c("2021", "2016")) {
  direction <- match.arg(direction)
  sf::st_read(gpkg_path(), layer = if (direction == "2021") "afma_2024" else "afma_2016", quiet = TRUE)
}

#' Boundary points as resolved
#'
#' Every vertex of every closure boundary as read from the instruments, with
#' the text it came from.
#'
#' @param source_key Optional Federal Register IDs to keep.
#' @return An `sf` point data frame.
#' @export
closure_vertices <- function(source_key = NULL) {
  v <- sf::st_read(gpkg_path(), layer = "vertices", quiet = TRUE)
  if (!is.null(source_key)) v <- v[v$source_key %in% source_key, ]
  v
}

#' Map the closures in force on a date
#'
#' @inheritParams closures_on
#' @param col_by Colour by `"gear"` (default) or `"quality"`.
#' @param xlim,ylim Map extent (longitude, latitude).
#' @param ... Passed to [plot()].
#' @return The closures plotted (invisibly).
#' @examples
#' plot_closures("2013-07-01")
#' @export
plot_closures <- function(date = Sys.Date(), gear = NULL, col_by = c("gear", "quality"),
                          xlim = c(113, 160), ylim = c(-48, -24), ...) {
  col_by <- match.arg(col_by)
  x <- closures_on(date, gear = gear)
  key <- if (col_by == "gear") ifelse(grepl("^all", x$gear_closed), "all methods",
           ifelse(grepl("gillnet", x$gear_closed), "gillnet",
           ifelse(grepl("trawl", x$gear_closed), "trawl",
           ifelse(grepl("hook|longline", x$gear_closed), "hook", "other")))) else x$quality
  lev <- sort(unique(key))
  pal <- c(`all methods` = "#c62828", gillnet = "#2a78d6", trawl = "#eb6834", hook = "#1baf7a", other = "grey50",
           exact = "#2a78d6", approx_feature = "#eb6834", approx_island_as_point = "#1baf7a", capad_2022 = "#8e44ad")
  cols <- grDevices::adjustcolor(pal[key], alpha.f = 0.35)
  main <- sprintf("SESSF closures in force %s (%d areas)", format(as.Date(date), "%d %b %Y"), nrow(x))
  if (requireNamespace("ozmaps", quietly = TRUE)) {
    plot(sf::st_geometry(sf::st_as_sf(ozmaps::ozmap_states)), col = "grey92", border = "grey60", lwd = 0.3,
         xlim = xlim, ylim = ylim, axes = TRUE, main = main, ...)
    plot(sf::st_geometry(x), col = cols, border = "grey30", lwd = 0.3, add = TRUE)
  } else {
    plot(sf::st_geometry(x), col = cols, border = "grey30", lwd = 0.3, xlim = xlim, ylim = ylim, axes = TRUE, main = main, ...)
  }
  graphics::legend("bottomleft", legend = lev, fill = grDevices::adjustcolor(pal[lev], alpha.f = 0.35), bty = "n", cex = 0.8)
  invisible(x)
}
