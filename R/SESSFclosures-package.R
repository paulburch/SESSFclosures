#' SESSFclosures: spatial closures of the SESSF, 2005 to now
#'
#' Closure boundaries from every SESSF closures instrument on the Federal
#' Register of Legislation, with the dates they were in force, the gear they
#' close and where each is written.
#'
#' Main functions: [closures_on()], [closures_now()], [in_closure()],
#' [closure_history()], [closure_source()], [closure_pdf()],
#' [gear_applies()], [afma_polygons()], [plot_closures()].
#' Data: [closures], [instruments], [legislation], [boundary_pages],
#' [other_measures], [not_mapped], [closure_changes], [text_fixes].
#'
#' **The polygons are an interpretation of the legal text, not the legal
#' description.** Boundaries along the coastline, coastal waters and the AFZ
#' are approximated (see `quality`); the instrument prevails.
#'
#' @keywords internal
#' @importFrom sf st_geometry
"_PACKAGE"
