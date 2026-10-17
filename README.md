# SESSFclosures

Spatial closures of Australia's **Southern and Eastern Scalefish and Shark
Fishery (SESSF)**, from May 2005 to now, as R data.

The package holds the boundary of every spatial closure written in the SESSF
closures directions on the [Federal Register of Legislation](https://www.legislation.gov.au),
from SESSF Direction No. 01 (May 2005) to the current compilation of the
*Fisheries Management (SESSF and SPF Closures) Direction 2021*. For each
closure it records:

- the polygon, for every period in which the boundary and gear rule did not change;
- the dates in force;
- the gear closed, and whether the closure applies only after a catch trigger;
- the instrument, schedule and PDF page where the boundary is written.

The authorised PDFs of the 62 instruments and compilations are bundled.

> **Status: version 0.1.0, not yet reviewed.** The boundaries were extracted
> automatically and compared with AFMA's published polygons, but they **have
> not yet been checked by a person**. Version 1.0.0 will follow that review,
> which will be described in [`NEWS.md`](NEWS.md). Progress is shown by
> `review_status()` and the `human_review` column of `closures`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("paulburch/SESSFclosures")
```

The package needs [`sf`](https://r-spatial.github.io/sf/). It is about 38 MB
installed, most of it the bundled PDFs.

## Quick start

```r
library(SESSFclosures)

closures_on("2012-07-01", gear = "trawl")   # closures in force that close trawl
closures_now()                               # in force today
plot_closures("2013-07-01")

# were these shots inside a closure that applied to their gear?
shots <- data.frame(lon = c(148.2, 155.8), lat = c(-39.5, -28.0),
                    date = as.Date(c("2010-03-01", "2020-01-01")), gear = c("TW", "TW"))
in_closure(shots, gear = "gear")

closure_history("Bass Strait")               # every version of a closure
closure_source("C029_v2")                    # where its boundary is written
closure_pdf("C029_v2")                       # open the PDF at that page
afma_polygons("2021")                        # AFMA's own polygons, for comparison
sf::st_layers(closures_gpkg())               # the full GeoPackage, for GIS users
```

See `vignette("using-SESSFclosures")`, and `vignette("how-SESSFclosures-was-built")`
for how the data were prepared, the key steps and prompts, and lessons for
similar analyses.

## Data

| Object | Content |
|---|---|
| `closures` | one row per closure version (`sf` polygons, WGS84): dates, gear, trigger, rule text, geometry quality, source reference, agreement with AFMA |
| `instruments` | every source text (as made or compilation) and the dates it was in effect |
| `legislation`, `boundary_pages` | bundled PDFs; PDF page range of every schedule |
| `other_measures`, `not_mapped` | instruments with no closure polygon; areas named but not drawn |
| `closure_changes` | the dates on which instruments started or stopped |
| `text_fixes` | documented corrections of drafting that could not be read as written |

## How accurate are the polygons?

**The polygons interpret the legal text; they are not the legal description.
The instrument prevails.**

- **Closures given wholly by coordinates** (`quality == "exact"`) match the
  polygons AFMA publishes for the 2016 and 2021 directions to an
  intersection-over-union of 0.98 or more.
- **Closures that follow the coastline, the outer limit of coastal waters, a
  line *n* nautical miles seaward, or the outer limit of the AFZ**
  (`quality == "approx_feature"`) use approximations of those lines. They agree
  with AFMA's polygons to an intersection-over-union of 0.84–0.98. Near those
  edges, use `afma_polygons()` or the instrument.
- **Sea lion closures around islands** (`approx_island_as_point`) are circles
  about the position given for each island.
- **The Kent Group National Park and the Victorian marine national parks** are
  defined in the instruments only by reference to State law. Their polygons
  (`capad_2022`) come from CAPAD 2022 and show the boundaries as at June 2022.
- **Catch-trigger closures** (`trigger == TRUE`, e.g. dogfish) applied only
  once a trigger was reached. The package records when such a closure *could*
  apply, not whether it was triggered.
- **No consolidated closures direction** was in force from 1 January to 12
  July 2007.

## Rebuilding the data

`data-raw/` holds everything needed to rebuild the data from the legislation:
- the instrument texts and catalogue;
- the parser;
- the scripts that fetch the PDFs, AFMA and CAPAD data.

From the package root:

```r
Rscript data-raw/run_all.R
```

See [`data-raw/README.md`](data-raw/README.md).

## Licence and sources

The code and derived polygons are © 2026 CSIRO, released under the BSD
3-clause licence (`LICENSE`). The bundled data keep their sources' terms
(`inst/COPYRIGHTS`):

- Federal Register of Legislation, © Commonwealth of Australia, CC BY 4.0.
- AFMA closure polygons, Australian Fisheries Management Authority.
- CAPAD 2022, Department of Climate Change, Energy, the Environment and Water,
  CC BY 4.0.

## Acknowledgements

This package was developed with substantial help from **Claude**, an AI
assistant made by Anthropic (model Claude Opus 5.5). Working under the
direction of Paul Burch, who set the scope and approach, Claude:
- found the instruments on the Federal Register;
- wrote the parser that reads the boundary descriptions;
- assembled the closure history;
- compared it with AFMA's polygons;
- wrote the package code, tests and documentation.

Claude is listed as a contributor in `DESCRIPTION`.

As with any automated extraction, errors are possible; the human review
described above is to find them. Please report errors through the GitHub
issues.

Maintainer: Paul Burch (CSIRO Environment), October 2026.
