#' Has a person checked the closure boundaries?
#'
#' Summarises the human review of the closure boundaries recorded in
#' [review_log] (and in the `human_review`, `reviewed_by` and `review_date`
#' columns of [closures]). Versions of the package below 1.0.0 have not been
#' fully reviewed; see `NEWS.md`.
#'
#' @param in_force_only Count only the closures in force now?
#' @return A data frame: the number of closure versions by review result.
#' @examples
#' review_status()
#' @export
review_status <- function(in_force_only = FALSE) {
  cl <- sf::st_drop_geometry(SESSFclosures::closures)
  if (in_force_only) cl <- cl[is.na(cl$valid_to), ]
  out <- as.data.frame(table(human_review = cl$human_review), stringsAsFactors = FALSE)
  names(out)[2] <- "closure_versions"
  out
}

.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion(pkgname)
  if (v < "1.0.0")
    packageStartupMessage("SESSFclosures ", v, ": closure boundaries have not yet been checked by a person; ",
                          "see review_status() and NEWS.md. The instrument prevails over these polygons.")
}
