# Rebuilding the SESSFclosures data

Everything needed to rebuild the package data from the legislation is in this
folder. From the package root:

```r
Rscript data-raw/run_all.R            # about 5-10 minutes
```

then rebuild and reinstall the package. Set `REFRESH_CATALOGUE=1` to search
the Federal Register again for new closures instruments before rebuilding.

R packages needed: `sf`, `jsonlite`, `xml2`, `pdftools`, `ozmaps`,
`rnaturalearth` (with `rnaturalearthdata` / `rnaturalearthhires`).

## Contents

| Path | What |
|---|---|
| `run_all.R` | runs every step below, then `make_data.R` |
| `make_data.R` | writes `data/*.rda`, copies the GeoPackage to `inst/extdata` and the PDFs to `inst/legislation` |
| `closures/catalogue/` | the closures instruments found on the Federal Register (`instrument_catalogue.csv`) and their compilations (`instrument_versions.csv`) |
| `closures/text/` | text of each instrument and compilation, as fetched from the Register's HTML view (first line gives the source URL). Missing texts are fetched again. |
| `closures/data/text_fixes.csv` | documented corrections of drafting the parser cannot read (coordinates unchanged) |
| `closures/capad/` | CAPAD 2022 Marine polygons for the parks defined only by State law (re-downloaded if missing) |
| `closures/afma_2016_urls.txt` | AFMA's per-schedule shapefiles of the 2016 direction (downloaded when needed) |
| `closures/R/fun_closures.R`, `fun_parse.R` | reading the texts, splitting schedules, parsing boundary descriptions, building polygons |
| `closures/R/03_features.R` | approximate coastline, *n* nm lines, outer limit of coastal waters, AFZ |
| `closures/R/04_build_version.R` | parses one instrument: polygons, vertices, checks |
| `closures/R/08_fetch_register_text.R`, `15_fetch_all.R` | fetch instrument texts from the Register |
| `closures/R/14_catalogue.R` | search the Register API for closures instruments |
| `closures/R/19_meta.R` | dates each instrument was in effect; operative rules (gear, triggers) |
| `closures/R/12_compare_afma.R`, `12b_compare_afma_2016.R` | agreement with AFMA's polygons |
| `closures/R/21_fetch_pdfs.R`, `22_pdf_pages.R` | authorised PDFs and the page range of every schedule |
| `closures/R/20_assemble_all.R` | links areas across instruments into closures and versions; writes the GeoPackage |

Outputs (`closures/output/`), downloads and PDF copies are not kept in git;
`run_all.R` recreates them.

## Sources

- Federal Register of Legislation, <https://www.legislation.gov.au>
  (API <https://api.prod.legislation.gov.au/v1/>), Commonwealth of
  Australia, CC BY 4.0.
- AFMA closure maps, <https://www.afma.gov.au/fishing-closures/maps-directions-closures-and-fishing-areas>.
- CAPAD 2022 Marine, DCCEEW, CC BY 4.0.
