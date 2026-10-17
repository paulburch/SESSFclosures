#' SESSF closure boundaries, 2005 to now
#'
#' One row per closure version: a closure area over a period in which its
#' boundary and its gear rule did not change. Built from every SESSF closures
#' instrument on the Federal Register of Legislation (as made, and the
#' compilations), May 2005 to October 2026. Instruments made before 2005 are
#' not on the Register.
#'
#' @format An `sf` data frame (polygons, WGS84) with columns:
#' \describe{
#'   \item{version_id}{closure version, e.g. C012_v2}
#'   \item{closure_id}{closure, e.g. C012; the same area across instruments
#'     (linked by source, name, or overlap of more than half)}
#'   \item{version}{version number within the closure}
#'   \item{area}{closure name as written}
#'   \item{schedule, schedule_title}{schedule of the instrument where this
#'     version began (`0` = instrument without schedules)}
#'   \item{instrument, source_key}{name and Federal Register ID of the
#'     instrument (or compilation) where this version began}
#'   \item{instrument_last, source_last}{the last instrument that restated it
#'     unchanged}
#'   \item{valid_from, valid_to}{first and last day in force (`valid_to` NA =
#'     in force now)}
#'   \item{change}{what began this version: `new`, `boundary changed`, `gear
#'     rule changed`, `catch-trigger / conditional rule added`, with the
#'     instruments that started or stopped that day}
#'   \item{ended_by}{the instruments that started or stopped on the day after
#'     `valid_to`}
#'   \item{gear_closed}{gear the closure closes, from the operative clauses:
#'     `all methods`, `trawl`, `demersal otter trawl`, `gillnet`, `hook`,
#'     `automatic longline`, combinations, or `none (orange roughy management
#'     area)`; see [gear_applies()]}
#'   \item{trigger}{`TRUE` if a clause closes the area only after a catch
#'     trigger (e.g. dogfish) or another condition}
#'   \item{species_specific}{`TRUE` if a clause names species}
#'   \item{rule_text}{the operative clauses naming the schedule, incl. exemptions}
#'   \item{quality}{`exact` (coordinates only), `approx_feature` (follows the
#'     coastline, coastal waters, a seaward line or the AFZ, approximated),
#'     `approx_island_as_point` (circle about the position given for an island),
#'     `capad_2022` (park defined by State law; CAPAD 2022 polygon)}
#'   \item{approximated_features, notes}{which features were approximated;
#'     parsing notes}
#'   \item{area_km2}{sea area, km2 (EPSG:3577)}
#'   \item{source_ref}{Register ID, schedule and PDF page range}
#'   \item{pdf_pages, pdf_file, register_url}{PDF page range (PDF pages, not
#'     printed numbers), bundled PDF file name, Register link}
#'   \item{afma_iou, afma_layer, afma_flag}{agreement with the AFMA polygon
#'     (intersection over union) for the 2016 and 2021 directions}
#'   \item{human_review, reviewed_by, review_date}{check of the boundary by a
#'     person: `not reviewed`, `correct`, `corrected` or `issue` (see
#'     [review_log], [review_status()])}
#' }
#' @details The boundaries were extracted from the instrument text by code
#'   written with Claude (Anthropic) and compared with AFMA's polygons, but in
#'   versions below 1.0.0 they have not been checked by a person.
#' @source Federal Register of Legislation, <https://www.legislation.gov.au>;
#'   compiled by CSIRO with the assistance of Claude (Anthropic), October 2026.
"closures"

#' Record of the human review of closure boundaries
#' @format Data frame, one row per check: `version_id`, `result` (`correct`,
#'   `corrected`, `issue`), `reviewed_by`, `review_date`, `method`, `notes`.
#'   Empty until the review starts.
"review_log"

#' Closures instruments and the dates each was in effect
#' @format Data frame: `key` (Register ID), `title` (the instrument a
#'   compilation belongs to), `kind` (as made / compilation), `name`, `start`
#'   (commencement), `end` (first day not in effect), `register_end`,
#'   `commencement_text`, `end_text`.
"instruments"

#' The authorised PDFs bundled with the package
#' @format Data frame: `register_id`, `title_id`, `start`, `pages`, `file`
#'   (in `system.file("legislation", package = "SESSFclosures")`), `url`, `name`.
"legislation"

#' PDF page range of every schedule of every instrument
#' @format Data frame: `key`, `schedule`, `schedule_title`, `pdf_page_from`,
#'   `pdf_page_to`, `pdf_pages`, `file`, `url`. Schedule 0 = the pages with
#'   coordinates in an instrument without schedules.
"boundary_pages"

#' Instruments with no closure polygon
#' @format Data frame: amending / revoking instruments, species bans over
#'   named waters and fishery-wide catch triggers, with their operative text.
"other_measures"

#' Areas named in an instrument but not drawn
#' @format Data frame (`key`, `schedule`, `area`, `reason`).
"not_mapped"

#' Dates on which closures instruments started or stopped
#' @format Data frame: `date`, `what`, `n_areas_in_effect`.
"closure_changes"

#' Corrections of drafting the parser could not read
#' @format Data frame: `key`, `pattern`, `replacement`, `note`. The
#'   coordinates are unchanged; clauses run together are split, missing
#'   hemispheres added.
"text_fixes"
