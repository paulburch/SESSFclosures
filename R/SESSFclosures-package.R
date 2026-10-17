#' SESSFclosures: spatial closures of the SESSF, 2005 to now
#'
#' Closure boundaries from every SESSF closures instrument on the Federal
#' Register of Legislation, with the dates they were in force, the gear they
#' close and where each is written.
#'
#' Main functions: [closures_on()], [closures_now()], [in_closure()],
#' [closure_history()], [closure_source()], [closure_pdf()],
#' [gear_applies()], [afma_polygons()], [plot_closures()], [review_status()].
#' Data: [closures], [instruments], [legislation], [boundary_pages],
#' [other_measures], [not_mapped], [closure_changes], [text_fixes],
#' [review_log].
#'
#' **The polygons are an interpretation of the legal text, not the legal
#' description.** Boundaries along the coastline, coastal waters and the AFZ
#' are approximated (see `quality`); the instrument prevails.
#'
#' **Review status.** Versions below 1.0.0 have not had their boundaries
#' checked by a person; see [review_status()] and `NEWS.md`.
#'
#' **Acknowledgement.** The parser, the assembly of the closure history, the
#' package code and the documentation were written by Claude (Anthropic, model
#' Claude Opus 5.5), an AI assistant, working with and directed by Paul Burch
#' (CSIRO), who set the scope and approach. The checks done so far are listed
#' in `NEWS.md`.
#'
#' @keywords internal
#' @importFrom sf st_geometry
"_PACKAGE"
