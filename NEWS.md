# SESSFclosures (development notes)

Version 1.0.0 will be released once the closure boundaries have been checked
by a person. That entry will record who checked them, when, how (e.g. against
the instrument text, AFMA's polygons, maps), which closures and versions were
checked, and any corrections made. Checks are recorded one closure version at
a time in `data-raw/closures/review/review_log.csv`. They appear in the
`human_review`, `reviewed_by` and `review_date` columns of `closures`, and are
summarised by `review_status()`.

# SESSFclosures 0.1.0 (2026-10-18)

First release. **The closure boundaries have not yet been checked by a person.**

* Boundaries of every spatial closure in the SESSF closures directions on the
  Federal Register of Legislation, May 2005 to the current compilation
  (June 2025) of the 2021 Closures Direction: 143 closures in 204 versions, 76
  in force.
* Boundaries were extracted automatically from the instrument text by a parser
  written with Claude (Anthropic). Checks done so far:
  * Each instrument was parsed independently, and successive instruments were
    compared where they restate the same closure.
  * Polygons were compared with AFMA's published polygons for the 2016 and 2021
    directions. Closures given wholly by coordinates agree to an
    intersection-over-union of 0.98 or more. Closures following the coastline,
    coastal waters or the AFZ, which are approximated, agree to 0.84–0.98.
  * Maps of the closures in force on selected dates were checked by eye.
  * The gear rules were compared with hand-made tables for the 2015 and 2021
    directions.
* Vignettes: `using-SESSFclosures` (how to use the data) and
  `how-SESSFclosures-was-built` (how the data were prepared, key prompts,
  lessons for similar analyses).
* Drafting the parser could not read as written is corrected in
  `data-raw/closures/data/text_fixes.csv` (coordinates unchanged).
* Parks defined only by State law (Kent Group National Park; Cape Howe,
  Discovery Bay and Twelve Apostles Marine National Parks) use CAPAD 2022
  polygons.
* Open questions for AFMA:
  * No consolidated closures direction was in force from 1 January to
    12 July 2007.
  * The Western Isles position (F2022L00116) falls on land.
  * AFMA's Schedule 30 polygon has a spike that isn't in the instrument.
  * Schedule 44 (2023) repeats Point 1.
  * The 2013 automatic longline point table is numbered in reverse.
  * Records of when trigger closures were triggered.
