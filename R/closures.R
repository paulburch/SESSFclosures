#' Closures in force on a date
#'
#' The closure versions whose boundary was in force on `date` (one row per
#' closure area), optionally only those that close a given gear.
#'
#' @param date A date (`Date` or `"YYYY-MM-DD"`).
#' @param gear Optional fishing gear (see [gear_applies()]); keeps the
#'   closures that close it. `NULL` keeps all.
#' @param triggers Keep closures that apply only after a catch trigger or other
#'   condition (`trigger == TRUE`)? Default `TRUE`.
#' @param management_areas Keep orange roughy management areas, which close
#'   no gear? Default `FALSE`.
#' @return An `sf` data frame (WGS84) with the columns of [closures].
#' @examples
#' x <- closures_on("2010-07-01")
#' nrow(x)
#' closures_on("2024-01-01", gear = "gillnet")[, c("closure_id", "area")]
#' @export
closures_on <- function(date, gear = NULL, triggers = TRUE, management_areas = FALSE) {
  d <- as.Date(date)
  if (length(d) != 1 || is.na(d)) stop("`date` must be a single date")
  cl <- SESSFclosures::closures
  x <- cl[cl$valid_from <= d & (is.na(cl$valid_to) | cl$valid_to >= d), ]
  if (!management_areas) x <- x[!grepl("^none", x$gear_closed), ]
  if (!triggers) x <- x[!x$trigger, ]
  if (!is.null(gear)) x <- x[gear_applies(x$gear_closed, gear) %in% TRUE, ]
  x
}

#' Closures in force now
#' @inheritParams closures_on
#' @return An `sf` data frame.
#' @examples
#' closures_now(gear = "trawl")
#' @export
closures_now <- function(gear = NULL, triggers = TRUE, management_areas = FALSE) {
  cl <- SESSFclosures::closures
  closures_on(max(c(Sys.Date(), cl$valid_from)), gear, triggers, management_areas)
}

#' The history of a closure
#'
#' All versions of one or more closures, oldest first, with the instrument
#' that began each version, what changed, and where its boundary is written.
#'
#' @param x A closure ID (`"C012"`), a version ID (`"C012_v2"`) or a
#'   regular expression matched against closure names (case-insensitive).
#' @return An `sf` data frame.
#' @examples
#' closure_history("Bass Strait")[, c("version_id", "valid_from", "valid_to", "change")]
#' @export
closure_history <- function(x) {
  cl <- SESSFclosures::closures
  ids <- if (all(grepl("^C[0-9]{3}(_v[0-9]+)?$", x))) sub("_v[0-9]+$", "", x) else
    unique(cl$closure_id[grepl(paste(x, collapse = "|"), cl$area, ignore.case = TRUE)])
  out <- cl[cl$closure_id %in% ids, ]
  out[order(out$closure_id, out$valid_from), ]
}

#' Where a closure boundary is written
#'
#' @param version_id One or more closure version IDs (e.g. `"C012_v2"`).
#' @return A data frame: the instrument (Federal Register ID and name), the
#'   schedule, the PDF page range, the Register link and the bundled PDF.
#' @examples
#' closure_source("C001_v2")
#' @export
closure_source <- function(version_id) {
  cl <- sf::st_drop_geometry(SESSFclosures::closures)
  x <- cl[match(version_id, cl$version_id), ]
  if (anyNA(x$version_id)) warning("unknown version_id: ", paste(version_id[is.na(x$version_id)], collapse = ", "))
  data.frame(version_id = version_id, area = x$area, register_id = x$source_key, instrument = x$instrument,
             schedule = x$schedule, pdf_pages = x$pdf_pages, source_ref = x$source_ref,
             register_url = x$register_url, last_restated_in = x$source_last,
             pdf = ifelse(is.na(x$pdf_file), NA, system.file("legislation", x$pdf_file, package = "SESSFclosures")),
             stringsAsFactors = FALSE)
}

#' Open the instrument PDF at the page where a closure is written
#'
#' @param version_id A closure version ID.
#' @param open Open the PDF in the browser? If `FALSE`, return the path.
#' @return The path of the bundled PDF (invisibly when opened).
#' @export
closure_pdf <- function(version_id, open = interactive()) {
  s <- closure_source(version_id[1])
  if (is.na(s$pdf) || !nzchar(s$pdf)) stop("no PDF for ", version_id)
  pg <- sub("-.*", "", s$pdf_pages)
  if (open) { utils::browseURL(paste0("file:///", normalizePath(s$pdf, "/"), if (!is.na(pg)) paste0("#page=", pg) else ""))
    return(invisible(s$pdf)) }
  s$pdf
}
