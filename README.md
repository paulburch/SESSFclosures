# SESSFclosures

Spatial closures of the Southern and Eastern Scalefish and Shark Fishery
(SESSF), **May 2005 to now**, from every closures instrument on the Federal
Register of Legislation. Each closure is a polygon with the dates it was in
force, the gear it closes, and the instrument, schedule and PDF page where its
boundary is written. The authorised PDFs (62 instruments and compilations) are
bundled.

```r
# install from the folder (internal package)
install.packages("C:/_AI/Claude_code/SESSF_zones/SESSFclosures", repos = NULL, type = "source")

library(SESSFclosures)
closures_on("2012-07-01", gear = "trawl")       # closures in force, closing trawl
in_closure(shots, gear = "gear")                 # shots inside closures that applied
closure_history("Bass Strait")                   # versions of a closure
closure_source("C001_v2")                        # where its boundary is written
closure_pdf("C001_v2")                           # open the PDF at that page
afma_polygons("2021")                            # AFMA's polygons, for comparison
sf::st_layers(closures_gpkg())                   # the full GeoPackage, for GIS
```

Licence: BSD 3-clause (CSIRO 2026); bundled data keep their sources' terms (`inst/COPYRIGHTS`).

**The polygons interpret the legal text; they are not the legal description.**
Boundaries following the coastline, coastal waters or the AFZ are
approximated (`quality`), and the instrument prevails.

Data were built by the closures review in
`C:/_AI/Claude_code/SESSF_zones/closures` (see its `PROGRESS.md` and
`doc/sessf_closures_review.html`); `data-raw/make_data.R` refreshes the
package from it. Compiled by CSIRO with the assistance of Claude (Anthropic),
October 2026.
